;;; elfeed-sources-owncloud.el --- owncloud news api for elfeed -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'elfeed)

(defcustom elfeed-sources-owncloud-url "https://127.0.0.1:443"
  "ownCloud server address."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-sources-owncloud-username ""
  "Username for ownCloud server."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-sources-owncloud-password ""
  "Password for ownCloud server."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-sources-owncloud-star-tag 'star
  "Default star tag for ownCloud News entry. If one entry set or
remove the tag, then the starred state in ownCloud will be synced, too."
  :group 'elfeed
  :type 'symbol)

(defvar elfeed-sources-owncloud-feeds nil
  "Feed list from ownCloud News, will be filled before updating operation.")

(defconst elfeed-sources-owncloud-api-base "/index.php/apps/news/api/v1-2")
(defconst elfeed-sources-owncloud-api-feeds (concat elfeed-sources-owncloud-api-base "/feeds"))
(defconst elfeed-sources-owncloud-api-init-unread (concat elfeed-sources-owncloud-api-base "/items?type=3&getRead=false&batchSize=-1"))
(defconst elfeed-sources-owncloud-api-init-starred (concat elfeed-sources-owncloud-api-base "/items?type=2&batchSize=-1"))
(defconst elfeed-sources-owncloud-api-update-all (concat elfeed-sources-owncloud-api-base "/items/updated?type=3&lastModified=%s"))
(defconst elfeed-sources-owncloud-api-update-feed (concat elfeed-sources-owncloud-api-base "/items?type=0&id=%s&getRead=false"))
(defconst elfeed-sources-owncloud-api-update-force-all (concat elfeed-sources-owncloud-api-base "/items?type=3&batchSize=-1"))
(defconst elfeed-sources-owncloud-api-read (concat elfeed-sources-owncloud-api-base "/items/%s/read"))
(defconst elfeed-sources-owncloud-api-unread (concat elfeed-sources-owncloud-api-base "/items/%s/unread"))
(defconst elfeed-sources-owncloud-api-star (concat elfeed-sources-owncloud-api-base "/items/%s/%s/star"))
(defconst elfeed-sources-owncloud-api-unstar (concat elfeed-sources-owncloud-api-base "/items/%s/%s/unstar"))
(defconst elfeed-sources-owncloud-api-read-multi (concat elfeed-sources-owncloud-api-base "/items/read/multiple"))
(defconst elfeed-sources-owncloud-api-unread-multi (concat elfeed-sources-owncloud-api-base "/items/unread/multiple"))
(defconst elfeed-sources-owncloud-api-star-multi (concat elfeed-sources-owncloud-api-base "/items/star/multiple"))
(defconst elfeed-sources-owncloud-api-unstar-multi (concat elfeed-sources-owncloud-api-base "/items/unstar/multiple"))

(defun elfeed-sources-owncloud--init-headers (&optional put-json)
  "Get http request headers with authorization and user agent information."
  (let* ((headers `(("User-Agent" . ,elfeed-user-agent))))
    (push `("Authorization" .
            ,(concat "Basic " (base64-encode-string
                               (concat elfeed-sources-owncloud-username ":"
                                       elfeed-sources-owncloud-password)))) headers)
    (when put-json
      (push `("Content-Type" . "application/json") headers))
    headers))

(defmacro elfeed-sources-owncloud-with-fetch (url data &rest body)
  "Just like `elfeed-with-fetch', but special for ownCloud News HTTP
request. DATA is in string format, if not nil will send PUT request."
  (declare (indent defun))
  `(let*  ((use-curl elfeed-use-curl) ; capture current value in closure
           (headers (elfeed-sources-owncloud--init-headers ,data))
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

(defun elfeed-sources-owncloud--parse-feeds ()
  "Parse the result feeds JSON buffer, ensure the point in the right place that
`json-read' could execute. Return `elfeed-sources-owncloud-feeds'."
  (setq elfeed-sources-owncloud-feeds (json-read))
  ;; fill feeds in db
  (let* ((feeds (cdr (assoc 'feeds elfeed-sources-owncloud-feeds))))
    (cl-loop for feed across feeds do
             (let* ((url (cdr (assoc 'url feed)))
                    (title (cdr (assoc 'title feed)))
                    (feed-db (elfeed-db-get-feed url)))
               (setf (elfeed-feed-url feed-db) url
                     (elfeed-feed-title feed-db) title))))
  elfeed-sources-owncloud-feeds)

(defun elfeed-sources-owncloud--update-feed-list ()
  "Update ownCloud News's feed list."
  (elfeed-sources-owncloud-with-fetch
    (concat elfeed-sources-owncloud-url elfeed-sources-owncloud-api-feeds)
    nil (elfeed-sources-owncloud--parse-feeds)))

(defun elfeed-sources-owncloud--get-feed-url (feed-id)
  "Get feed url through the ID."
  (catch 'found
    (let* ((feeds (cdr (assoc 'feeds elfeed-sources-owncloud-feeds)))
           (length (length feeds)))
      (dotimes (i length)
        (let* ((feed (elt feeds i))
               (id (cdr (assoc 'id feed)))
               (url (cdr (assoc 'url feed))))
          (when (eq id feed-id)
            (throw 'found url)))))))

(defun elfeed-sources-owncloud--get-feed-id (feed-url)
  "Get feed ID through the url."
  (catch 'found
    (let* ((feeds (cdr (assoc 'feeds elfeed-sources-owncloud-feeds)))
           (length (length feeds)))
      (dotimes (i length)
        (let* ((feed (elt feeds i))
               (id (cdr (assoc 'id feed)))
               (url (cdr (assoc 'url feed))))
          (when (string= url feed-url)
            (throw 'found id)))))))

(defun elfeed-sources-owncloud--get-last-modified ()
  "Get last entry modified time which is the seconds since 1970-01-01 00:00:00
UTC, if not initialized, just return nil. The last modified time was saved in
elfeed-db as a mock feed."
  (let* ((urlobj (url-generic-parse-url elfeed-sources-owncloud-url))
         (url (concat "owncloud://" elfeed-sources-owncloud-username  "@" (url-host urlobj)
                      ":" (number-to-string (url-port urlobj))))
         (feed (elfeed-db-get-feed url))
         (last-modified (elfeed-meta feed :last-modified)))
    (if last-modified
        last-modified
      nil)))

(defun elfeed-sources-owncloud--set-last-modified (last-modified)
  "Set last entry modified time which is the seconds since 1970-01-01 00:00:00
UTC."
  (let* ((urlobj (url-generic-parse-url elfeed-sources-owncloud-url))
         (url (concat "owncloud://" elfeed-sources-owncloud-username  "@" (url-host urlobj)
                      ":" (number-to-string (url-port urlobj))))
         (feed (elfeed-db-get-feed url)))
    (setf (elfeed-meta feed :last-modified) last-modified)))

(defun elfeed-sources-owncloud-is-owncloud-entry (entry)
  "Check if specific ENTRY is fetched from ownCloud News. Return t if
is, or return nil."
  (eq (elfeed-meta entry :source) 'owncloud))

(defun elfeed-sources-owncloud--parse-entries (&optional mark-last-modified)
  "Parse the result entries JSON buffer, ensure the point in the right place
that `json-read' could execute. Return parsed entries. If MARK-LAST-MODIFIED is
nil, then just not update the last-modifed value.

User could download items.json from ownCloud manually, for example
http://server/items?type=3&batchSize=-1, and import the entries by calling
`elfeed-sources-owncloud--parse-entries' in the buffer."
  (if elfeed-sources-owncloud-feeds
      (let* ((begin-time (time-to-seconds))
             (max-last-modified 0)
             items entries)
        (elfeed-log 'debug "elfeed-sources-owncloud: parsing entries")
        (setq items (cdr (assoc 'items (json-read))))
        (setq entries
              (cl-loop for item across items collect
                       (let* ((id (cdr (assoc 'id item)))
                              (guid-hash (cdr (assoc 'guidHash item)))
                              (feed-id (cdr (assoc 'feedId item)))
                              (feed-url (elfeed-sources-owncloud--get-feed-url feed-id))
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
                                        (push elfeed-sources-owncloud-star-tag fixtags))
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
                                                 ,@(list :source 'owncloud
                                                         :id id
                                                         :guid-hash guid-hash
                                                         :feed-id feed-id)))))
                         ;; force override unread and star tags without repeat sync operation
                         (when original
                           (if unread (elfeed-tag-1 original 'unread)
                             (elfeed-untag-1 original 'unread))
                           (if starred (elfeed-tag-1 original elfeed-sources-owncloud-star-tag)
                               (elfeed-untag-1 original elfeed-sources-owncloud-star-tag)))
                         ;; get the last modified time
                         (when (> last-modified max-last-modified)
                           (setq max-last-modified last-modified))
                         (dolist (hook elfeed-new-entry-parse-hook)
                           (funcall hook :owncloud item db-entry))
                         db-entry)))
        ;; Update Last-Modified
        (when (and mark-last-modified (> max-last-modified 0))
          (elfeed-sources-owncloud--set-last-modified max-last-modified))
        (elfeed-db-add entries)
        (elfeed-log 'debug "elfeed-sources-owncloud: parse %s entries finished with %ss"
                    (length entries) (- (time-to-seconds) begin-time))
        entries)
    (progn
      (elfeed-log 'error "Warning: elfeed-sources-owncloud-feeds is nil, please call elfeed-sources-owncloud--update-feed-list first")
      nil)))

(defun elfeed-sources-owncloud--do-update (action &optional arg)
  "Real updating operations. ACTION could be init, update-feed,
update-force-all, or a timestamp. For init, will fetch unread and starred
entries. For update-feed, will fetch unread entries for special feed, the ARG is
the feed id. For update-force-all, will fetch all entries without checking their
states. And for a timestamp means only update entries since the special time,
the ARG is the time-stamp."
  (let* ((url-update-force-all (concat elfeed-sources-owncloud-url
                                       elfeed-sources-owncloud-api-update-force-all))
         (url-init-unread (concat elfeed-sources-owncloud-url
                                  elfeed-sources-owncloud-api-init-unread))
         (url-init-starred (concat elfeed-sources-owncloud-url
                                   elfeed-sources-owncloud-api-init-starred))
         (mark-last-modified t)
         url)
    (cond
     ;; initial sync, fetch unread entries
     ((eq action 'init) (setq url url-init-unread))
     ;; update entries for special feed
     ((eq action 'update-feed)
      (setq mark-last-modified nil)
      (setq url (concat elfeed-sources-owncloud-url
                        (format elfeed-sources-owncloud-api-update-feed arg))))
     ;; update all entries
     ((eq action 'update-force-all) (setq url url-update-force-all))
     ;; update since last modified, action is the time-stamp
     ((eq action 'timestamp)
      (setq url (concat elfeed-sources-owncloud-url
                        (format elfeed-sources-owncloud-api-update-all arg)))))
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (elfeed-sources-owncloud-with-fetch url nil
      (elfeed-sources-owncloud--parse-entries mark-last-modified)
      (run-hook-with-args 'elfeed-update-hooks url))
    (when (eq action 'init)
      ;; initial sync, fetch starred entries
      (elfeed-sources-owncloud-with-fetch url-init-starred nil
        ;; do not remember the last-modifed for starred entries, for
        ;; they always not the last entries.
        (elfeed-sources-owncloud--parse-entries)
        (run-hook-with-args 'elfeed-update-hooks url-init-starred)))))

(defun elfeed-sources-owncloud-update-all ()
  "Update entries in ownCloud News, if first time run, will initial sync, or
will fetch the updated entries."
  (interactive)
  (let* ((last-modified (elfeed-sources-owncloud--get-last-modified)))
    (elfeed-sources-owncloud-with-fetch
     (concat elfeed-sources-owncloud-url elfeed-sources-owncloud-api-feeds) nil
     (elfeed-sources-owncloud--parse-feeds)
     (if last-modified
         (elfeed-sources-owncloud--do-update 'timestamp last-modified)
       (elfeed-sources-owncloud--do-update 'init)))))

(defun elfeed-sources-owncloud-update-feed (feed-url)
  "Update entries under special feed in ownCloud News."
  (interactive)
  (let* ((feed-id (elfeed-sources-owncloud--get-feed-id feed-url)))
    (when feed-id
      (elfeed-sources-owncloud--do-update 'update-feed feed-id))))

(defun elfeed-sources-owncloud-update-since (&optional timestamp)
  "Update entries since special TIMESTAMP, the TIMESTAMP is the seconds since
1970-01-01 00:00:00 UTC, the default TIMESTAMP point to 24 hours ago."
  (interactive)
  (unless timestamp
    (setq timestamp (- (time-to-seconds) (* 24 3600))))
  (elfeed-sources-owncloud-with-fetch
   (concat elfeed-sources-owncloud-url elfeed-sources-owncloud-api-feeds) nil
   (elfeed-sources-owncloud--parse-feeds)
   (elfeed-sources-owncloud--do-update 'timestamp timestamp)))

(defun elfeed-sources-owncloud-reinit ()
  "Retry initial sync, fetch all unread and starred entries from ownCloud
News. This may take a long time, ensure `elfeed-curl-timeout' is big enough."
  (interactive)
  (elfeed-sources-owncloud-with-fetch
   (concat elfeed-sources-owncloud-url elfeed-sources-owncloud-api-feeds) nil
   (elfeed-sources-owncloud--parse-feeds)
   (elfeed-sources-owncloud--do-update 'init)))

(defun elfeed-sources-owncloud-update-force-all ()
  "Force fetch all the entries from ownCloud News without checking their
states. This may take a long time, ensure `elfeed-curl-timeout' is big enough."
  (interactive)
  (elfeed-sources-owncloud-with-fetch
   (concat elfeed-sources-owncloud-url elfeed-sources-owncloud-api-feeds) nil
   (elfeed-sources-owncloud--parse-feeds)
   (elfeed-sources-owncloud--do-update 'update-force-all)))

(defun elfeed-sources-owncloud-mark-read (entry)
  "Notify special ownCloud News entry as read."
  (let* ((id (elfeed-meta entry :id))
         (url (concat elfeed-sources-owncloud-url
                      (format elfeed-sources-owncloud-api-read id))))
    (elfeed-sources-owncloud-with-fetch url "{}")))

(defun elfeed-sources-owncloud-mark-unread (entry)
  "Notify special ownCloud News entry as unread."
  (let* ((id (elfeed-meta entry :id))
         (url (concat elfeed-sources-owncloud-url
                      (format elfeed-sources-owncloud-api-unread id))))
    (elfeed-sources-owncloud-with-fetch url "{}")))

(defun elfeed-sources-owncloud-mark-star (entry)
  "Notify special ownCloud News entry as starred."
  (let* ((feed-id (elfeed-meta entry :feed-id))
         (guid-hash (elfeed-meta entry :guid-hash))
         (url (concat elfeed-sources-owncloud-url (format elfeed-sources-owncloud-api-star
                                                feed-id guid-hash))))
    (elfeed-sources-owncloud-with-fetch url "{}")))

(defun elfeed-sources-owncloud-mark-unstar (entry)
  "Notify special ownCloud News entry as unstarred."
  (let* ((feed-id (elfeed-meta entry :feed-id))
         (guid-hash (elfeed-meta entry :guid-hash))
         (url (concat elfeed-sources-owncloud-url (format elfeed-sources-owncloud-api-unstar
                                                feed-id guid-hash))))
    (elfeed-sources-owncloud-with-fetch url "{}")))

(defun elfeed-sources-owncloud-mark-read-multi (entries)
  "Notify multiple ownCloud news entries to be read."
  (let* ((url (concat elfeed-sources-owncloud-url elfeed-sources-owncloud-api-read-multi))
         (ids (cl-loop for entry in entries collect
                       (when (elfeed-sources-owncloud-is-owncloud-entry entry)
                         (elfeed-meta entry :id))))
         (data (json-encode-list (list (cons 'items ids)))))
    (when ids
      (elfeed-sources-owncloud-with-fetch url data))))

(defun elfeed-sources-owncloud-mark-unread-multi (entries)
  "Notify multiple ownCloud news entries to be unread."
  (let* ((url (concat elfeed-sources-owncloud-url elfeed-sources-owncloud-api-unread-multi))
         (ids (cl-loop for entry in entries collect
                       (when (elfeed-sources-owncloud-is-owncloud-entry entry)
                         (elfeed-meta entry :id))))
         (data (json-encode-list (list (cons 'items ids)))))
    (when ids
      (elfeed-sources-owncloud-with-fetch url data))))

(defun elfeed-sources-owncloud-mark-star-multi (entries)
  "Notify multiple ownCloud news entries to be starred."
  (let* ((url (concat elfeed-sources-owncloud-url elfeed-sources-owncloud-api-star-multi))
         (items (cl-loop for entry in entries collect
                         (when (elfeed-sources-owncloud-is-owncloud-entry entry)
                           (let* ((feed-id (elfeed-meta entry :feed-id))
                                  (guid-hash (elfeed-meta entry :guid-hash)))
                             (list (cons 'feedId feed-id)
                                   (cons 'guidHash guid-hash))))))
         (data (json-encode-list (list (cons 'items items)))))
    (when items
      (elfeed-sources-owncloud-with-fetch url data))))

(defun elfeed-sources-owncloud-mark-unstar-multi (entries)
  "Notify multiple ownCloud news entries to be unstarred."
  (let* ((url (concat elfeed-sources-owncloud-url elfeed-sources-owncloud-api-unstar-multi))
         (items (cl-loop for entry in entries collect
                         (when (elfeed-sources-owncloud-is-owncloud-entry entry)
                           (let* ((feed-id (elfeed-meta entry :feed-id))
                                  (guid-hash (elfeed-meta entry :guid-hash)))
                             (list (cons 'feedId feed-id)
                                   (cons 'guidHash guid-hash))))))
         (data (json-encode-list (list (cons 'items items)))))
    (when items
      (elfeed-sources-owncloud-with-fetch url data))))

(defun elfeed-sources-owncloud-sync-tag-multi (entries tag action)
  "Notify multiple ownCloud news entries to be unread. ENTRIES is the elfeed
entriy list object, TAG is the action tag, for example unread and
`elfeed-sources-owncloud-star-tag', ACTION could be add or remove."
  (cond
   ((eq action 'add)
    (cond
     ((eq tag 'unread) (elfeed-sources-owncloud-mark-unread-multi entries))
     ((eq tag elfeed-sources-owncloud-star-tag) (elfeed-sources-owncloud-mark-star-multi entries))))
   ((eq action 'remove)
    (cond
     ((eq tag 'unread) (elfeed-sources-owncloud-mark-read-multi entries))
     ((eq tag elfeed-sources-owncloud-star-tag) (elfeed-sources-owncloud-mark-unstar-multi entries))))))

(defun elfeed-sources-owncloud-pre-tag (entries &rest tags)
  "Sync unread/starred stats before tags added."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-sources-owncloud-sync-tag-multi entries-modified tag 'add))))

(defun elfeed-sources-owncloud-pre-untag (entries &rest tags)
  "Sync unread/starred stats before tags removed."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-sources-owncloud-sync-tag-multi entries-modified tag 'remove))))


(provide 'elfeed-sources-owncloud)

;;; elfeed-sources-owncloud.el ends here
