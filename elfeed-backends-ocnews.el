;;; elfeed-ocnews.el --- ocnews api for elfeed -*- lexical-binding: t; -*-

(require 'json)
(require 'url)
(require 'elfeed-curl)
(require 'elfeed-db)
(require 'elfeed-lib)

                                        ;TODO:
;; define used variables and functions from elfeed
(defvar elfeed-use-curl)
(defvar elfeed-user-agent)
(defvar elfeed-initial-tags)
(defvar elfeed-new-entry-parse-hook)
(defvar elfeed--inhibit-update-init-hooks)
(defvar elfeed-update-init-hooks)
(declare-function elfeed-feed-autotags 'elfeed (url-or-feed))
(declare-function elfeed-generate-id 'elfeed (&optional content))
(declare-function elfeed-handle-http-error 'elfeed (url status))
(declare-function elfeed-handle-parse-error 'elfeed (url error))

(defcustom elfeed-ocnews-url "https://127.0.0.1:443"
  "ownCloud server address."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-ocnews-username ""
  "Username for ownCloud server."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-ocnews-password ""
  "Password for ownCloud server."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-ocnews-star-tag 'star
  "Default star tag for ownCloud News entry. If one entry set or
remove the tag, then the starred state in ownCloud will be synced, too."
  :group 'elfeed
  :type 'symbol)

(defvar elfeed-ocnews-feeds nil
  "Feed list from ownCloud News, will be filled before updating operation.")

(defconst elfeed-ocnews-api-base "/index.php/apps/news/api/v1-2")
(defconst elfeed-ocnews-api-feeds (concat elfeed-ocnews-api-base "/feeds"))
(defconst elfeed-ocnews-api-init-unread (concat elfeed-ocnews-api-base "/items?type=3&getRead=false&batchSize=-1"))
(defconst elfeed-ocnews-api-init-starred (concat elfeed-ocnews-api-base "/items?type=2&batchSize=-1"))
(defconst elfeed-ocnews-api-update (concat elfeed-ocnews-api-base "/items/updated?type=3&lastModified=%s"))
(defconst elfeed-ocnews-api-update-all (concat elfeed-ocnews-api-base "/items?type=3&batchSize=-1"))
(defconst elfeed-ocnews-api-read (concat elfeed-ocnews-api-base "/items/%s/read"))
(defconst elfeed-ocnews-api-unread (concat elfeed-ocnews-api-base "/items/%s/unread"))
(defconst elfeed-ocnews-api-star (concat elfeed-ocnews-api-base "/items/%s/%s/star"))
(defconst elfeed-ocnews-api-unstar (concat elfeed-ocnews-api-base "/items/%s/%s/unstar"))
(defconst elfeed-ocnews-api-read-multi (concat elfeed-ocnews-api-base "/items/read/multiple"))
(defconst elfeed-ocnews-api-unread-multi (concat elfeed-ocnews-api-base "/items/unread/multiple"))
(defconst elfeed-ocnews-api-star-multi (concat elfeed-ocnews-api-base "/items/star/multiple"))
(defconst elfeed-ocnews-api-unstar-multi (concat elfeed-ocnews-api-base "/items/unstar/multiple"))

(defun elfeed-ocnews--init-headers (&optional put-json)
  "Get http request headers with authorization and user agent information."
  (let* ((headers `(("User-Agent" . ,elfeed-user-agent))))
    (push `("Authorization" .
            ,(concat "Basic " (base64-encode-string
                               (concat elfeed-ocnews-username ":"
                                     elfeed-ocnews-password)))) headers)
    (when put-json
      (push `("Content-Type" . "application/json") headers))
    headers))

(defmacro elfeed-ocnews-with-fetch (url data &rest body)
  "Just like `elfeed-with-fetch', but special for ownCloud News HTTP
  request. DATA is in string format, if not nil will send PUT request."
  (declare (indent defun))
  `(let*  ((use-curl elfeed-use-curl) ; capture current value in closure
           (headers (elfeed-ocnews--init-headers ,data))
           (cb (lambda (status)
                 (if (elfeed-is-status-error status use-curl)
                     (let ((print-escape-newlines t))
                       (elfeed-handle-http-error
                        ,url
                        (if use-curl elfeed-curl-error-message status)))
                   (condition-case error
                       (progn
                         (unless use-curl
                           (elfeed-move-to-first-empty-line)
                           (set-buffer-multibyte t))
                         ,@body
                         (unless use-curl
                           (kill-buffer)))
                     (error (elfeed-handle-parse-error ,url error)))))))
     (if use-curl
         (if ,data
             (elfeed-curl-enqueue ,url cb :headers headers
                                  :method "PUT" :data ,data)
           (elfeed-curl-enqueue ,url cb :headers headers))
       (if ,data
           (let ((url-request-extra-headers headers)
                 (url-request-method "PUT")
                 (url-request-data ,data))
             (url-retrieve ,url cb () t t))
         (let ((url-request-extra-headers headers))
           (url-retrieve ,url cb () t t))))))

(defun elfeed-ocnews--parse-feeds ()
  "Parse the result feeds JSON buffer, ensure the point in the
right place that `json-read' could execute. Return
`elfeed-ocnews-feeds'."
  (setq elfeed-ocnews-feeds (json-read))
  ;; fill feeds in db
  (let* ((feeds (cdr (assoc 'feeds elfeed-ocnews-feeds))))
    (cl-loop for feed across feeds do
             (let* ((url (cdr (assoc 'url feed)))
                    (title (cdr (assoc 'title feed)))
                    (feed-db (elfeed-db-get-feed url)))
               (setf (elfeed-feed-url feed-db) url
                     (elfeed-feed-title feed-db) title))))
  elfeed-ocnews-feeds)

(defun elfeed-ocnews--update-feeds ()
  "Update ownCloud News's feed list."
  (elfeed-ocnews-with-fetch
    (concat elfeed-ocnews-url elfeed-ocnews-api-feeds)
    nil (elfeed-ocnews--parse-feeds)))

(defun elfeed-ocnews--get-feed-url (feed-id)
  "Get feed url through the ID."
  (catch 'found
    (let* ((feeds (cdr (assoc 'feeds elfeed-ocnews-feeds)))
           (length (length feeds)))
      (dotimes (i length)
        (let* ((feed (elt feeds i))
               (id (cdr (assoc 'id feed)))
               (url (cdr (assoc 'url feed))))
          (when (eq id feed-id)
            (throw 'found url)))))))

(defun elfeed-ocnews--get-last-modified ()
  "Get last entry modified time which is the seconds since
1970-01-01 00:00:00 UTC, if not initialized, just return nil. The
last modified time was saved in elfeed-db as a mock feed."
  (let* ((urlobj (url-generic-parse-url elfeed-ocnews-url))
         (url (concat "ocnews://" elfeed-ocnews-username  "@" (url-host urlobj)
                      ":" (number-to-string (url-port urlobj))))
         (feed (elfeed-db-get-feed url))
         (last-modified (elfeed-meta feed :last-modified)))
    (if last-modified
        last-modified
      nil)))

(defun elfeed-ocnews--set-last-modified (last-modified)
  "Set last entry modified time which is the seconds since
1970-01-01 00:00:00 UTC."
  (let* ((urlobj (url-generic-parse-url elfeed-ocnews-url))
         (url (concat "ocnews://" elfeed-ocnews-username  "@" (url-host urlobj)
                      ":" (number-to-string (url-port urlobj))))
         (feed (elfeed-db-get-feed url)))
    (setf (elfeed-meta feed :last-modified) last-modified)))

(defun elfeed-ocnews-is-ocnews-entry (entry)
  "Check if specific ENTRY is fetched from ownCloud News. Return t if
is, or return nil."
  (eq (elfeed-meta entry :source) 'ocnews))

(defun elfeed-ocnews--parse-entries (&optional mark-last-modified)
  "Parse the result entries JSON buffer, ensure the point in the
right place that `json-read' could execute. Return parsed
entries. If MARK-LAST-MODIFIED is nil, then just not update the
last-modifed value.

User could download items.json from ownCloud manually, for
example http://server/items?type=3&batchSize=-1, and import the
entries by calling `elfeed-ocnews--parse-entries' in the
buffer."
  (if elfeed-ocnews-feeds
      (let* ((begin-time (time-to-seconds))
             (max-last-modified 0)
             items entries)
        (elfeed-log 'debug "elfeed-ocnews: parsing entries")
        (setq items (cdr (assoc 'items (json-read))))
        (setq entries
              (cl-loop for item across items collect
                       (let* ((id (cdr (assoc 'id item)))
                              (guid-hash (cdr (assoc 'guidHash item)))
                              (feed-id (cdr (assoc 'feedId item)))
                              (feed-url (elfeed-ocnews--get-feed-url feed-id))
                              (url (cdr (assoc 'url item)))
                              (title (cdr (assoc 'title item)))
                              (author (cdr (assoc 'author item)))
                              (pub-date (cdr (assoc 'pubDate item)))
                              (body (cdr (assoc 'body item)))
                              (unread (not (eq (cdr (assoc 'unread item))
                                               ':json-false)))
                              (starred (not (eq (cdr (assoc 'starred item))
                                                ':json-false)))
                              (last-modified (cdr (assoc 'lastModified item)))
                              (enclosure-mime (cdr (assoc 'enclosureMime item)))
                              (enclosure-link (cdr (assoc 'enclosureLink item)))

                              (namespace (elfeed-url-to-namespace feed-url))
                              (full-id (cons namespace (elfeed-cleanup guid-hash)))
                              (original (elfeed-db-get-entry full-id))
                              (original-date (and original
                                                  (elfeed-entry-date original)))
                              (autotags (elfeed-feed-autotags feed-url))
                              (fixtags (elfeed-normalize-tags
                                        autotags elfeed-initial-tags))
                              (tags (progn
                                      (unless unread
                                        (setq fixtags (delete 'unread fixtags)))
                                      (when starred
                                        (push elfeed-ocnews-star-tag fixtags))
                                      fixtags))
                              (enclosures (when enclosure-link
                                            (list (list enclosure-link
                                                        enclosure-mime 0))))
                              (db-entry (elfeed-entry--create
                                         :title (elfeed-cleanup title)
                                         :id full-id
                                         :feed-id feed-url
                                         :link (elfeed-cleanup url)
                                         :tags tags
                                         :date (elfeed-new-date-for-entry
                                                original-date pub-date)
                                         :enclosures enclosures
                                         :content body
                                         :content-type 'html
                                         :meta `(,@(when author
                                                     (list :author author))
                                                 ,@(list :source 'ocnews
                                                         :id id
                                                         :guid-hash guid-hash
                                                         :feed-id feed-id)))))
                         ;; force override unread and star tags
                         (when original
                           (if unread (elfeed-tag-1 original 'unread)
                             (elfeed-untag-1 original 'unread))
                           (if starred (elfeed-tag-1 original elfeed-ocnews-star-tag)
                             (elfeed-untag-1 original elfeed-ocnews-star-tag)))
                         ;; get the last modified time
                         (when (> last-modified max-last-modified)
                           (setq max-last-modified last-modified))
                         (dolist (hook elfeed-new-entry-parse-hook)
                           (funcall hook :ocnews item db-entry))
                         db-entry)))
        ;; Update Last-Modified
        (when (and mark-last-modified (> max-last-modified 0))
          (elfeed-ocnews--set-last-modified max-last-modified))
        (elfeed-db-add entries)
        (elfeed-log 'debug "elfeed-ocnews: parse entries finished with %ss"
                    (- (time-to-seconds) begin-time))
        entries)
    (progn
      (elfeed-log 'error "Warning: elfeed-ocnews-feeds is nil, please call elfeed-ocnews--update-feeds first")
      nil)))

(defun elfeed-ocnews--do-update (action)
  "Real updating operations. ACTION could be init, update-all, or a
timestamp. For init, will fetch unread and starred entries. For
update-all, will fetch all entries without checking their states. And
for a timestamp means only update entries since the special
time."
  (let* ((url-update-all (concat elfeed-ocnews-url
                                 elfeed-ocnews-api-update-all))
         (url-init-unread (concat elfeed-ocnews-url
                                  elfeed-ocnews-api-init-unread))
         (url-init-starred (concat elfeed-ocnews-url
                                   elfeed-ocnews-api-init-starred))
         url)
    (cond
     ;; initial sync, fetch unread entries
     ((eq action 'init) (setq url url-init-unread))
     ;; update all entries
     ((eq action 'update-all) (setq url url-update-all))
     ;; update since last modified, action is the timestamp
     (action
      (setq url (concat elfeed-ocnews-url (format elfeed-ocnews-api-update
                                                  action)))))
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (elfeed-ocnews-with-fetch url nil
      (elfeed-ocnews--parse-entries t)
      (run-hook-with-args 'elfeed-update-hooks url))
    (when (eq action 'init)
      ;; initial sync, fetch starred entries
      (elfeed-ocnews-with-fetch url-init-starred nil
        ;; do not remember the last-modifed for starred entries, for
        ;; they always not the last entries.
        (elfeed-ocnews--parse-entries)
        (run-hook-with-args 'elfeed-update-hooks url-init-starred)))))

(defun elfeed-ocnews-update ()
  "Update the entries in ownCloud News, if first time run,
will initial sync, or will fetch the updated entries."
  (interactive)
  (let* ((last-modified (elfeed-ocnews--get-last-modified)))
  (elfeed-ocnews-with-fetch
    (concat elfeed-ocnews-url elfeed-ocnews-api-feeds) nil
    (elfeed-ocnews--parse-feeds)
    (if last-modified
        (elfeed-ocnews--do-update last-modified)
      (elfeed-ocnews--do-update 'init)))))

(defun elfeed-ocnews-update-since (&optional timestamp)
  "Update the entries since special TIMESTAMP, the TIMESTAMP is the seconds since
1970-01-01 00:00:00 UTC, the default TIMESTAMP point to 24 hours ago."
  (interactive)
  (unless timestamp
    (setq timestamp (- (time-to-seconds) (* 24 3600))))
  (elfeed-ocnews-with-fetch
    (concat elfeed-ocnews-url elfeed-ocnews-api-feeds) nil
    (elfeed-ocnews--parse-feeds)
    (elfeed-ocnews--do-update timestamp)))

(defun elfeed-ocnews-reinit ()
  "Retry initial sync, fetch all unread and starred entries from ownCloud
News. This may take a long time, ensure `elfeed-curl-timeout' is big
enough."
  (interactive)
  (elfeed-ocnews-with-fetch
    (concat elfeed-ocnews-url elfeed-ocnews-api-feeds) nil
    (elfeed-ocnews--parse-feeds)
    (elfeed-ocnews--do-update 'init)))

(defun elfeed-ocnews-update-all ()
  "Force fetch all the entries from ownCloud News without checking
their states. This may take a long time, ensure `elfeed-curl-timeout'
is big enough."
  (interactive)
  (elfeed-ocnews-with-fetch
    (concat elfeed-ocnews-url elfeed-ocnews-api-feeds) nil
    (elfeed-ocnews--parse-feeds)
    (elfeed-ocnews--do-update 'update-all)))

(defun elfeed-ocnews-mark-read (entry)
  "Notify special ownCloud News entry as read."
  (let* ((id (elfeed-meta entry :id))
         (url (concat elfeed-ocnews-url
                      (format elfeed-ocnews-api-read id))))
    (elfeed-ocnews-with-fetch url "{}")))

(defun elfeed-ocnews-mark-unread (entry)
  "Notify special ownCloud News entry as unread."
  (let* ((id (elfeed-meta entry :id))
         (url (concat elfeed-ocnews-url
                      (format elfeed-ocnews-api-unread id))))
    (elfeed-ocnews-with-fetch url "{}")))

(defun elfeed-ocnews-mark-star (entry)
  "Notify special ownCloud News entry as starred."
  (let* ((feed-id (elfeed-meta entry :feed-id))
         (guid-hash (elfeed-meta entry :guid-hash))
         (url (concat elfeed-ocnews-url (format elfeed-ocnews-api-star
                                                feed-id guid-hash))))
    (elfeed-ocnews-with-fetch url "{}")))

(defun elfeed-ocnews-mark-unstar (entry)
  "Notify special ownCloud News entry as unstarred."
  (let* ((feed-id (elfeed-meta entry :feed-id))
         (guid-hash (elfeed-meta entry :guid-hash))
         (url (concat elfeed-ocnews-url (format elfeed-ocnews-api-unstar
                                                feed-id guid-hash))))
    (elfeed-ocnews-with-fetch url "{}")))

(defun elfeed-ocnews-mark-read-multi (entries)
  "Notify multiple ownCloud news entries to be read."
  (let* ((url (concat elfeed-ocnews-url elfeed-ocnews-api-read-multi))
         (ids (cl-loop for entry in entries collect
                       (when (elfeed-ocnews-is-ocnews-entry entry)
                         (elfeed-meta entry :id))))
         (data (json-encode-list (list (cons 'items ids)))))
    (when ids
      (elfeed-ocnews-with-fetch url data))))

(defun elfeed-ocnews-mark-unread-multi (entries)
  "Notify multiple ownCloud news entries to be unread."
  (let* ((url (concat elfeed-ocnews-url elfeed-ocnews-api-unread-multi))
         (ids (cl-loop for entry in entries collect
                       (when (elfeed-ocnews-is-ocnews-entry entry)
                         (elfeed-meta entry :id))))
         (data (json-encode-list (list (cons 'items ids)))))
    (when ids
      (elfeed-ocnews-with-fetch url data))))

(defun elfeed-ocnews-mark-star-multi (entries)
  "Notify multiple ownCloud news entries to be starred."
  (let* ((url (concat elfeed-ocnews-url elfeed-ocnews-api-star-multi))
         (items (cl-loop for entry in entries collect
                         (when (elfeed-ocnews-is-ocnews-entry entry)
                           (let* ((feed-id (elfeed-meta entry :feed-id))
                                  (guid-hash (elfeed-meta entry :guid-hash)))
                             (list (cons 'feedId feed-id)
                                   (cons 'guidHash guid-hash))))))
         (data (json-encode-list (list (cons 'items items)))))
    (when items
      (elfeed-ocnews-with-fetch url data))))

(defun elfeed-ocnews-mark-unstar-multi (entries)
  "Notify multiple ownCloud news entries to be unstarred."
  (let* ((url (concat elfeed-ocnews-url elfeed-ocnews-api-unstar-multi))
         (items (cl-loop for entry in entries collect
                         (when (elfeed-ocnews-is-ocnews-entry entry)
                           (let* ((feed-id (elfeed-meta entry :feed-id))
                                  (guid-hash (elfeed-meta entry :guid-hash)))
                             (list (cons 'feedId feed-id)
                                   (cons 'guidHash guid-hash))))))
         (data (json-encode-list (list (cons 'items items)))))
    (when items
      (elfeed-ocnews-with-fetch url data))))

(defun elfeed-ocnews-sync-tag-multi (entries tag action)
  "Notify multiple ownCloud news entries to be unread. ENTRIES is the
elfeed entriy list object, TAG is the action tag, for example unread
and `elfeed-ocnews-star-tag', ACTION could be add, remove or toggle."
  (cond
   ((eq action 'add)
    (cond
     ((eq tag 'unread) (elfeed-ocnews-mark-unread-multi entries))
     ((eq tag elfeed-ocnews-star-tag) (elfeed-ocnews-mark-star-multi entries))))
   ((eq action 'remove)
    (cond
     ((eq tag 'unread) (elfeed-ocnews-mark-read-multi entries))
     ((eq tag elfeed-ocnews-star-tag) (elfeed-ocnews-mark-unstar-multi entries))))
   ((eq action 'toggle)
    (cond
     ((eq tag 'unread)
      (let* ((entries-read (cl-loop for entry in entries
                                    when (elfeed-tagged-p tag entry)
                                    collect entry))
             (entries-unread (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
        (elfeed-ocnews-mark-read-multi entries-read)
        (elfeed-ocnews-mark-unread-multi entries-unread)))
     ((eq tag elfeed-ocnews-star-tag)
      (let* ((entries-unstar (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry))
             (entries-star (cl-loop for entry in entries
                                    unless (elfeed-tagged-p tag entry)
                                    collect entry)))
        (elfeed-ocnews-mark-star-multi entries-star)
        (elfeed-ocnews-mark-unstar-multi entries-unstar)))))))

(provide 'elfeed-ocnews)

;;; elfeed-ocnews.el ends here
