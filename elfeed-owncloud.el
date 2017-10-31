;;; elfeed-owncloud.el --- owncloud news api for elfeed -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'elfeed)

(defcustom elfeed-owncloud-maxsize 1000
  "Maximize entries size for each request."
  :group 'elfeed-protocol
  :type 'integer)

(defcustom elfeed-owncloud-star-tag 'star
  "Default star tag for ownCloud News entry. If one entry set or
remove the tag, then the starred state in ownCloud will be synced, too."
  :group 'elfeed-protocol
  :type 'symbol)

(defvar elfeed-owncloud-feeds (make-hash-table :test 'equal)
  "Feed list from ownCloud News, will be filled before updating operation.")

(defconst elfeed-owncloud-api-base "/index.php/apps/news/api/v1-2")
(defconst elfeed-owncloud-api-feeds (concat elfeed-owncloud-api-base "/feeds"))
(defconst elfeed-owncloud-api-init-unread (concat elfeed-owncloud-api-base "/items?type=3&getRead=false&batchSize=%s"))
(defconst elfeed-owncloud-api-init-starred (concat elfeed-owncloud-api-base "/items?type=2&getRead=true&batchSize=%s"))
(defconst elfeed-owncloud-api-update (concat elfeed-owncloud-api-base "/items/updated?type=3&lastModified=%s"))
(defconst elfeed-owncloud-api-update-feed (concat elfeed-owncloud-api-base "/items?type=0&id=%s&getRead=false&batchSize=%s"))
(defconst elfeed-owncloud-api-update-offset (concat elfeed-owncloud-api-base "/items?type=3&offset=%s&getRead=true&batchSize=%s&oldestFirst=true"))
(defconst elfeed-owncloud-api-read (concat elfeed-owncloud-api-base "/items/%s/read"))
(defconst elfeed-owncloud-api-unread (concat elfeed-owncloud-api-base "/items/%s/unread"))
(defconst elfeed-owncloud-api-star (concat elfeed-owncloud-api-base "/items/%s/%s/star"))
(defconst elfeed-owncloud-api-unstar (concat elfeed-owncloud-api-base "/items/%s/%s/unstar"))
(defconst elfeed-owncloud-api-read-multi (concat elfeed-owncloud-api-base "/items/read/multiple"))
(defconst elfeed-owncloud-api-unread-multi (concat elfeed-owncloud-api-base "/items/unread/multiple"))
(defconst elfeed-owncloud-api-star-multi (concat elfeed-owncloud-api-base "/items/star/multiple"))
(defconst elfeed-owncloud-api-unstar-multi (concat elfeed-owncloud-api-base "/items/unstar/multiple"))

(defun elfeed-protocol-owncloud-id (url)
  (elfeed-protocol-id "owncloud" url))

(defun elfeed-owncloud--init-headers (url &optional put-json)
  "Get http request headers with authorization and user agent information. URL
should contains user and password fields, if not, will query in the related feed
properties."
  (let* ((urlobj (url-generic-parse-url url))
         (user (url-user urlobj))
         (proto-id (elfeed-protocol-owncloud-id url))
         (feed (elfeed-protocol-id-to-feed proto-id))
         (proto-props (when (listp feed) (cdr feed)))
         (password (if (url-password urlobj)
                       (url-password urlobj)
                     (plist-get proto-props :password)))
         (headers `(("User-Agent" . ,elfeed-user-agent))))
    (when (or (not user) (not password))
      (elfeed-log 'error "elfeed-owncloud: missing username or password"))
    (push `("Authorization" .
            ,(concat "Basic " (base64-encode-string
                               (concat user ":" password))))
          headers)
    (when put-json
      (push `("Content-Type" . "application/json") headers))
    headers))

(defmacro elfeed-owncloud-with-fetch (url data &rest body)
  "Just like `elfeed-with-fetch', but special for ownCloud News HTTP
request. DATA is in string format, if not nil will send PUT request."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl) ; capture current value in closure
          (headers (elfeed-owncloud--init-headers ,url ,data))
          (no-auth-url (elfeed-protocol-no-auth-url ,url))
          (cb (lambda (status)
                (if (elfeed-is-status-error status use-curl)
                    (let ((print-escape-newlines t))
                      (elfeed-handle-http-error
                       no-auth-url
                       (if use-curl elfeed-curl-error-message status)))
                  (condition-case error
                      (progn
                        (unless use-curl
                          (elfeed-move-to-first-empty-line)
                          (set-buffer-multibyte t))
                        ,@body
                        (unless use-curl
                          (kill-buffer)))
                     (error (elfeed-handle-parse-error no-auth-url error)))))))
     (if use-curl
         (if ,data
             (elfeed-curl-enqueue no-auth-url cb :headers headers
                                  :method "PUT" :data ,data)
           (elfeed-curl-enqueue no-auth-url cb :headers headers))
       (if ,data
           (let ((url-request-extra-headers headers)
                 (url-request-method "PUT")
                 (url-request-data ,data))
             (url-retrieve no-auth-url cb () t t))
         (let ((url-request-extra-headers headers))
           (url-retrieve no-auth-url cb () t t))))))

(defun elfeed-owncloud--parse-feeds (url)
  "Parse the result feeds JSON buffer and fill pasred feeds to db, ensure the
point in the right place that `json-read' could execute. Return
`elfeed-owncloud-feeds'."
  (let* ((proto-id (elfeed-protocol-owncloud-id url))
         (parsed-feeds (json-read))
         (feeds (cdr (assoc 'feeds parsed-feeds))))
    (puthash proto-id feeds elfeed-owncloud-feeds)
    (cl-loop for feed across feeds do
             (let* ((feed-url (cdr (assoc 'url feed)))
                    (feed-id (elfeed-protocol-format-entry-feed-id
                              proto-id feed-url))
                    (feed-title (cdr (assoc 'title feed)))
                    (feed-db (elfeed-db-get-feed feed-id)))
               (setf (elfeed-feed-url feed-db) feed-id
                     (elfeed-feed-title feed-db) feed-title)))
    elfeed-owncloud-feeds))

(defun elfeed-owncloud--update-feed-list (url)
  "Update ownCloud News's feed list."
  (elfeed-owncloud-with-fetch
    (concat url elfeed-owncloud-api-feeds)
    nil (elfeed-owncloud--parse-feeds url)))

(defun elfeed-owncloud--get-feed-url (url feed-id)
  "Get feed url through the ownCloud feed id."
  (catch 'found
    (let* ((proto-id (elfeed-protocol-owncloud-id url))
           (feeds (gethash proto-id elfeed-owncloud-feeds))
           (length (length feeds)))
      (dotimes (i length)
        (let* ((feed (elt feeds i))
               (id (cdr (assoc 'id feed)))
               (url (cdr (assoc 'url feed))))
          (when (eq id feed-id)
            (throw 'found url)))))))

(defun elfeed-owncloud--get-feed-id (url feed-url)
  "Get ownCloud feed id through the url."
  (catch 'found
    (let* ((proto-id (elfeed-protocol-owncloud-id url))
           (feeds (gethash proto-id elfeed-owncloud-feeds))
           (length (length feeds)))
      (dotimes (i length)
        (let* ((feed (elt feeds i))
               (id (cdr (assoc 'id feed)))
               (url (cdr (assoc 'url feed))))
          (when (string= url feed-url)
            (throw 'found id)))))))

(defun elfeed-owncloud--get-last-modified (proto-id)
  "Get last entry modified time which is the seconds since 1970-01-01 00:00:00
UTC, if not initialized, just return 0. The last modified time was saved in
elfeed-db as a mock feed."
  (let* ((feed (elfeed-db-get-feed proto-id))
         (last-modified (elfeed-meta feed :last-modified)))
    (if last-modified
        last-modified
      0)))

(defun elfeed-owncloud--set-last-modified (proto-id last-modified)
  "Set last entry modified time which is the seconds since 1970-01-01 00:00:00
UTC."
  (let* ((feed (elfeed-db-get-feed proto-id)))
    (setf (elfeed-meta feed :last-modified) last-modified)))

(defun elfeed-owncloud--get-last-entry-id (proto-id)
  "Get last entry id, if not initialized, just return 0."
  (let* ((feed (elfeed-db-get-feed proto-id))
         (last-entry-id (elfeed-meta feed :last-entry-id)))
    (if last-entry-id
        last-entry-id
      0)))

(defun elfeed-owncloud--set-last-entry-id (proto-id last-entry-id)
  "Set last entry id to elfeed-db."
  (let* ((feed (elfeed-db-get-feed proto-id)))
    (setf (elfeed-meta feed :last-entry-id) last-entry-id)))

(defun elfeed-owncloud-entry-p (entry)
  "Check if specific ENTRY is fetched from ownCloud News. Return t if
is, or return nil."
  (let* ((proto-id (elfeed-protocol-entry-protocol-id entry))
         (proto-type (when proto-id (elfeed-protocol-type proto-id))))
    (string= proto-type "owncloud")))

(defun elfeed-owncloud--parse-entries (url &optional mark-last-modified callback)
  "Parse the result entries JSON buffer, ensure the point in the right place
that `json-read' could execute. Return parsed entries. If MARK-LAST-MODIFIED is
nil, then just not update the last-modifed value.

User could download items.json from ownCloud manually, for example
http://myhost.com/items?type=3&batchSize=-1, and import the entries by calling
`elfeed-owncloud--parse-entries' in the buffer."
  (if elfeed-owncloud-feeds
      (let* ((proto-id (elfeed-protocol-owncloud-id url))
             (begin-time (time-to-seconds))
             (max-last-modified 0)
             (max-last-entry-id 0)
             items entries)
        (elfeed-log 'debug "elfeed-owncloud: parsing entries")
        (setq items (cdr (assoc 'items (json-read))))
        (setq entries
              (cl-loop for item across items collect
                       (let* ((id (cdr (assoc 'id item)))
                              (guid-hash (cdr (assoc 'guidHash item)))
                              (feed-id (cdr (assoc 'feedId item)))
                              (feed-url (elfeed-owncloud--get-feed-url url feed-id))
                              (entry-url (cdr (assoc 'url item)))
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
                              (autotags (elfeed-protocol-feed-autotags proto-id feed-url))
                              (fixtags (elfeed-normalize-tags
                                        autotags elfeed-initial-tags))
                              (tags (progn
                                      (unless unread
                                        (setq fixtags (delete 'unread fixtags)))
                                      (when starred
                                        (push elfeed-owncloud-star-tag fixtags))
                                      fixtags))
                              (enclosures (when enclosure-link
                                            (list (list enclosure-link
                                                        enclosure-mime 0))))
                              (db-entry (elfeed-entry--create
                                         :title (elfeed-cleanup title)
                                         :id full-id
                                         :feed-id (elfeed-protocol-format-entry-feed-id
                                                   proto-id feed-url)
                                         :link (elfeed-cleanup entry-url)
                                         :tags tags
                                         :date (elfeed-new-date-for-entry
                                                original-date pub-date)
                                         :enclosures enclosures
                                         :content body
                                         :content-type 'html
                                         :meta `(,@(when author
                                                     (list :author author))
                                                 ,@(list :protocol-id proto-id
                                                         :id id
                                                         :guid-hash guid-hash
                                                         :feed-id feed-id)))))
                         ;; force override unread and star tags without repeat sync operation
                         (when original
                           (if unread (elfeed-tag-1 original 'unread)
                             (elfeed-untag-1 original 'unread))
                           (if starred (elfeed-tag-1 original elfeed-owncloud-star-tag)
                               (elfeed-untag-1 original elfeed-owncloud-star-tag)))
                         ;; get the last modified time and entry id
                         (when (> last-modified max-last-modified)
                           (setq max-last-modified last-modified))
                         (when (> id max-last-entry-id)
                           (setq max-last-entry-id id))
                         (dolist (hook elfeed-new-entry-parse-hook)
                           (funcall hook :owncloud item db-entry))
                         db-entry)))
        ;; Update last modified time and last entry id
        (when (and mark-last-modified (> max-last-modified 0))
          (elfeed-owncloud--set-last-modified proto-id max-last-modified))
        (when (and mark-last-modified (> max-last-entry-id 0))
          (elfeed-owncloud--set-last-entry-id proto-id max-last-entry-id))
        (elfeed-db-add entries)
        (when callback (funcall callback entries))
        (elfeed-log 'debug "elfeed-owncloud: parse %s entries finished with %ss"
                    (length entries) (- (time-to-seconds) begin-time))
        entries)
    (progn
      (elfeed-log 'error "Warning: elfeed-owncloud-feeds is nil, please call elfeed-owncloud--update-feed-list first")
      nil)))

(defun elfeed-owncloud--do-update (url action &optional arg callback)
  "Real updating operations. URL is the host name of ownCloud server, and
authentication info is always required, for example
\"https://user:pass@myhost.com\". ACTION could be init, update-feed,
update-offset, or a timestamp. For init, will fetch unread and starred
entries. For update-feed, will fetch unread entries for special feed, the ARG is
the feed id. For update-offset, will fetch all entries after the provide offset
id. And for a timestamp means only update entries since the special time, the
ARG is the time-stamp."
  (let* ((proto-id (elfeed-protocol-owncloud-id url))
         (offset-id (if arg arg (elfeed-owncloud--get-last-entry-id proto-id)))
         (url-update-offset (concat url (format elfeed-owncloud-api-update-offset
                                                offset-id elfeed-owncloud-maxsize)))
         (url-init-unread (concat url (format
                                       elfeed-owncloud-api-init-unread
                                       elfeed-owncloud-maxsize)))
         (url-init-starred (concat url (format
                                        elfeed-owncloud-api-init-starred
                                        elfeed-owncloud-maxsize)))
         (mark-last-modified t)
         url-opt)
    (cond
     ;; initial sync, fetch unread entries
     ((eq action 'init) (setq url-opt url-init-unread))
     ;; update entries for special feed
     ((eq action 'update-feed)
      (setq mark-last-modified nil)
      (setq url-opt (concat url
                        (format elfeed-owncloud-api-update-feed arg
                                elfeed-owncloud-maxsize))))
     ;; update all entries
     ((eq action 'update-offset) (setq url-opt url-update-offset))
     ;; update since last modified, action is the time-stamp
     ((eq action 'timestamp)
      (setq url-opt (concat url (format elfeed-owncloud-api-update arg)))))
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (elfeed-owncloud-with-fetch url-opt nil
      (elfeed-owncloud--parse-entries url mark-last-modified callback)
      (run-hook-with-args 'elfeed-update-hooks url-opt))
    (when (eq action 'init)
      ;; initial sync, fetch starred entries
      (elfeed-owncloud-with-fetch url-init-starred nil
        ;; do not remember the last-modifed for starred entries, for
        ;; they always not the last entries.
        (elfeed-owncloud--parse-entries url nil callback)
        (run-hook-with-args 'elfeed-update-hooks url-init-starred)))))

(defun elfeed-owncloud-reinit (url)
  "Retry initial sync, fetch all unread and starred entries from ownCloud
News. This may take a long time, ensure `elfeed-curl-timeout' is big enough."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-owncloud-with-fetch
   (concat url elfeed-owncloud-api-feeds) nil
   (elfeed-owncloud--parse-feeds url)
   (elfeed-owncloud--do-update url 'init)))

(defun elfeed-owncloud-update-skip (url &optional timestamp)
  "Update entries since special TIMESTAMP, the TIMESTAMP is the seconds since
1970-01-01 00:00:00 UTC, the default TIMESTAMP point to 1 hours ago."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (unless timestamp
    (setq timestamp (- (time-to-seconds) (* 1 3600))))
  (elfeed-owncloud-with-fetch
   (concat url elfeed-owncloud-api-feeds) nil
   (elfeed-owncloud--parse-feeds url)
   (elfeed-owncloud--do-update url 'timestamp timestamp)))

(defun elfeed-owncloud-update-offset (url &optional offset)
  "Fetch all the entries after the OFFSET id from ownCloud News. If OFFSET not
provide, will update since the last entry id."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-owncloud-with-fetch
   (concat url elfeed-owncloud-api-feeds) nil
   (elfeed-owncloud--parse-feeds url)
   (elfeed-owncloud--do-update url 'update-offset offset)))

(defun elfeed-owncloud-mark-read (url entry)
  "Notify special ownCloud News entry as read."
  (let* ((id (elfeed-meta entry :id))
         (url (concat url (format elfeed-owncloud-api-read id))))
    (elfeed-owncloud-with-fetch url "{}")))

(defun elfeed-owncloud-mark-unread (url entry)
  "Notify special ownCloud News entry as unread."
  (let* ((id (elfeed-meta entry :id))
         (url (concat url (format elfeed-owncloud-api-unread id))))
    (elfeed-owncloud-with-fetch url "{}")))

(defun elfeed-owncloud-mark-star (url entry)
  "Notify special ownCloud News entry as starred."
  (let* ((feed-id (elfeed-meta entry :feed-id))
         (guid-hash (elfeed-meta entry :guid-hash))
         (url (concat url (format elfeed-owncloud-api-star
                                  feed-id guid-hash))))
    (elfeed-owncloud-with-fetch url "{}")))

(defun elfeed-owncloud-mark-unstar (url entry)
  "Notify special ownCloud News entry as unstarred."
  (let* ((feed-id (elfeed-meta entry :feed-id))
         (guid-hash (elfeed-meta entry :guid-hash))
         (url (concat url (format elfeed-owncloud-api-unstar
                                  feed-id guid-hash))))
    (elfeed-owncloud-with-fetch url "{}")))

(defun elfeed-owncloud-mark-read-multi (url entries)
  "Notify multiple ownCloud news entries to be read."
  (let* ((url (concat url elfeed-owncloud-api-read-multi))
         (ids (cl-loop for entry in entries collect
                       (when (elfeed-owncloud-entry-p entry)
                         (elfeed-meta entry :id))))
         (data (json-encode-list (list (cons 'items ids)))))
    (when ids
      (elfeed-owncloud-with-fetch url data))))

(defun elfeed-owncloud-mark-unread-multi (url entries)
  "Notify multiple ownCloud news entries to be unread."
  (let* ((url (concat url elfeed-owncloud-api-unread-multi))
         (ids (cl-loop for entry in entries collect
                       (when (elfeed-owncloud-entry-p entry)
                         (elfeed-meta entry :id))))
         (data (json-encode-list (list (cons 'items ids)))))
    (when ids
      (elfeed-owncloud-with-fetch url data))))

(defun elfeed-owncloud-mark-star-multi (url entries)
  "Notify multiple ownCloud news entries to be starred."
  (let* ((url (concat url elfeed-owncloud-api-star-multi))
         (items (cl-loop for entry in entries collect
                         (when (elfeed-owncloud-entry-p entry)
                           (let* ((feed-id (elfeed-meta entry :feed-id))
                                  (guid-hash (elfeed-meta entry :guid-hash)))
                             (list (cons 'feedId feed-id)
                                   (cons 'guidHash guid-hash))))))
         (data (json-encode-list (list (cons 'items items)))))
    (when items
      (elfeed-owncloud-with-fetch url data))))

(defun elfeed-owncloud-mark-unstar-multi (url entries)
  "Notify multiple ownCloud news entries to be unstarred."
  (let* ((url (concat url elfeed-owncloud-api-unstar-multi))
         (items (cl-loop for entry in entries collect
                         (when (elfeed-owncloud-entry-p entry)
                           (let* ((feed-id (elfeed-meta entry :feed-id))
                                  (guid-hash (elfeed-meta entry :guid-hash)))
                             (list (cons 'feedId feed-id)
                                   (cons 'guidHash guid-hash))))))
         (data (json-encode-list (list (cons 'items items)))))
    (when items
      (elfeed-owncloud-with-fetch url data))))

(defun elfeed-owncloud-sync-tag-multi (url entries tag action)
  "Notify multiple ownCloud news entries to be unread. ENTRIES is the elfeed
entriy list object, TAG is the action tag, for example unread and
`elfeed-owncloud-star-tag', ACTION could be add or remove."
  (cond
   ((eq action 'add)
    (cond
     ((eq tag 'unread) (elfeed-owncloud-mark-unread-multi url entries))
     ((eq tag elfeed-owncloud-star-tag)
      (elfeed-owncloud-mark-star-multi url entries))))
   ((eq action 'remove)
    (cond
     ((eq tag 'unread) (elfeed-owncloud-mark-read-multi url entries))
     ((eq tag elfeed-owncloud-star-tag)
      (elfeed-owncloud-mark-unstar-multi url entries))))))

(defun elfeed-protocol-owncloud-pre-tag (url entries &rest tags)
  "Sync unread/starred stats before tags added."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-owncloud-sync-tag-multi url entries-modified tag 'add))))

(defun elfeed-protocol-owncloud-pre-untag (url entries &rest tags)
  "Sync unread/starred stats before tags removed."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-owncloud-sync-tag-multi url entries-modified tag 'remove))))

(defun elfeed-owncloud-update-feed (url feed-url &optional callback)
  "Update entries under special feed in ownCloud News."
  (interactive)
  (let* ((feed-id (elfeed-owncloud--get-feed-id url feed-url)))
    (when feed-id
      (elfeed-owncloud--do-update url 'update-feed feed-id callback))))

(defun elfeed-protocol-owncloud-update (url &optional callback)
  "ownCloud News protocol updater. URL is the host name of ownCloud server, and
authentication info is always required, for example
\"https://user:pass@myhost.com\". If first time run, it will initial sync for
target URL, or will only fetch the updated entries since last modified."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((host-url (elfeed-protocol-host-url url))
         (feed-url (elfeed-protocol-feed-url url)))
    (if feed-url (elfeed-owncloud-update-feed host-url feed-url callback)
      (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
             (last-modified (elfeed-owncloud--get-last-modified proto-id)))
        (elfeed-owncloud-with-fetch
         (concat host-url elfeed-owncloud-api-feeds) nil
         (elfeed-owncloud--parse-feeds host-url)
         (if (> last-modified 0)
             (elfeed-owncloud--do-update host-url 'timestamp (+ 1 last-modified) callback)
       (elfeed-owncloud--do-update host-url 'init nil callback)))))))

(provide 'elfeed-owncloud)

;;; elfeed-owncloud.el ends here
