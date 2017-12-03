;;; elfeed-protocol-owncloud.el --- ownCloud News protocol for elfeed -*- lexical-binding: t; -*-

;;; Commentary:
;; ownCloud News protocol for elfeed.

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'elfeed)

;;; Code:

(defcustom elfeed-protocol-owncloud-maxsize 1000
  "Maximize entries size for each request."
  :group 'elfeed-protocol
  :type 'integer)

(defcustom elfeed-protocol-owncloud-star-tag 'star
  "Default star tag for ownCloud News entry.
If one entry set or remove the tag,
then the starred state in ownCloud will be synced, too."
  :group 'elfeed-protocol
  :type 'symbol)

(defvar elfeed-protocol-owncloud-feeds (make-hash-table :test 'equal)
  "Feed list from ownCloud News, will be filled before updating operation.")

(defconst elfeed-protocol-owncloud-api-base "/index.php/apps/news/api/v1-2")
(defconst elfeed-protocol-owncloud-api-feeds (concat elfeed-protocol-owncloud-api-base "/feeds"))
(defconst elfeed-protocol-owncloud-api-init-unread (concat elfeed-protocol-owncloud-api-base "/items?type=3&getRead=false&batchSize=%s"))
(defconst elfeed-protocol-owncloud-api-init-starred (concat elfeed-protocol-owncloud-api-base "/items?type=2&getRead=true&batchSize=%s"))
(defconst elfeed-protocol-owncloud-api-update (concat elfeed-protocol-owncloud-api-base "/items/updated?type=3&lastModified=%s"))
(defconst elfeed-protocol-owncloud-api-update-feed (concat elfeed-protocol-owncloud-api-base "/items?type=0&id=%s&getRead=false&batchSize=%s"))
(defconst elfeed-protocol-owncloud-api-update-offset (concat elfeed-protocol-owncloud-api-base "/items?type=3&offset=%s&getRead=true&batchSize=%s&oldestFirst=true"))
(defconst elfeed-protocol-owncloud-api-read (concat elfeed-protocol-owncloud-api-base "/items/%s/read"))
(defconst elfeed-protocol-owncloud-api-unread (concat elfeed-protocol-owncloud-api-base "/items/%s/unread"))
(defconst elfeed-protocol-owncloud-api-star (concat elfeed-protocol-owncloud-api-base "/items/%s/%s/star"))
(defconst elfeed-protocol-owncloud-api-unstar (concat elfeed-protocol-owncloud-api-base "/items/%s/%s/unstar"))
(defconst elfeed-protocol-owncloud-api-read-multi (concat elfeed-protocol-owncloud-api-base "/items/read/multiple"))
(defconst elfeed-protocol-owncloud-api-unread-multi (concat elfeed-protocol-owncloud-api-base "/items/unread/multiple"))
(defconst elfeed-protocol-owncloud-api-star-multi (concat elfeed-protocol-owncloud-api-base "/items/star/multiple"))
(defconst elfeed-protocol-owncloud-api-unstar-multi (concat elfeed-protocol-owncloud-api-base "/items/unstar/multiple"))

(defun elfeed-protocol-owncloud-id (url)
  "Get owncloud protocol id with URL."
  (elfeed-protocol-id "owncloud" url))

(defun elfeed-protocol-owncloud--init-headers (url &optional put-json)
  "Get http request headers with authorization and user agent information.
URL should contains user and password fields, if not, will query in the related
feed properties.  Will set content type to json if PUT-JSON is not nil."
  (let* ((urlobj (url-generic-parse-url url))
         (user (url-user urlobj))
         (proto-id (elfeed-protocol-owncloud-id url))
         (password (if (url-password urlobj)
                       (url-password urlobj)
                     (elfeed-protocol-meta-password proto-id)))
         (headers `(("User-Agent" . ,elfeed-user-agent))))
    (when (or (not user) (not password))
      (elfeed-log 'error "elfeed-protocol-owncloud: missing username or password"))
    (push `("Authorization" .
            ,(concat "Basic " (base64-encode-string
                               (concat user ":" password))))
          headers)
    (when put-json
      (push `("Content-Type" . "application/json") headers))
    headers))

(defmacro elfeed-protocol-owncloud-with-fetch (url data &rest body)
  "Just like `elfeed-with-fetch' but special for ownCloud HTTP request.
URL is the target url, DATA is in string format, if not nil will send PUT
request.  Optional argument BODY is the rest Lisp code after operation
finished."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl) ; capture current value in closure
          (headers (elfeed-protocol-owncloud--init-headers ,url ,data))
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

(defun elfeed-protocol-owncloud--parse-feeds (url)
  "Parse the feeds JSON buffer and fill results to db.
URL is the target ownCloud url to fetch feeds.  Ensure the point in the right
place that `json-read' could execute.  Return `elfeed-protocol-owncloud-feeds'."
  (let* ((proto-id (elfeed-protocol-owncloud-id url))
         (parsed-feeds (json-read))
         (feeds (map-elt parsed-feeds 'feeds)))
    (puthash proto-id feeds elfeed-protocol-owncloud-feeds)
    (cl-loop for feed across feeds do
             (let* ((feed-url (map-elt feed 'url))
                    (feed-id (elfeed-protocol-format-subfeed-id
                              proto-id feed-url))
                    (feed-title (map-elt feed 'title))
                    (feed-db (elfeed-db-get-feed feed-id)))
               (setf (elfeed-feed-url feed-db) feed-id
                     (elfeed-feed-title feed-db) feed-title)))
    elfeed-protocol-owncloud-feeds))

(defun elfeed-protocol-owncloud--update-feed-list (url)
  "Update ownCloud News feed list.  URL is the target protocol feed url."
  (elfeed-protocol-owncloud-with-fetch
    (concat url elfeed-protocol-owncloud-api-feeds)
    nil (elfeed-protocol-owncloud--parse-feeds url)))

(defun elfeed-protocol-owncloud--get-feed-url (url feed-id)
  "Get child feed url for the ownCloud protocol feed URL and target FEED-ID."
  (catch 'found
    (let* ((proto-id (elfeed-protocol-owncloud-id url))
           (feeds (gethash proto-id elfeed-protocol-owncloud-feeds))
           (length (length feeds)))
      (dotimes (i length)
        (let* ((feed (elt feeds i))
               (id (map-elt feed 'id))
               (url (map-elt feed 'url)))
          (when (eq id feed-id)
            (throw 'found url)))))))

(defun elfeed-protocol-owncloud--get-feed-id (url feed-url)
  "Get child feed id the ownCloud protocol feed URL and target FEED-URL."
  (catch 'found
    (let* ((proto-id (elfeed-protocol-owncloud-id url))
           (feeds (gethash proto-id elfeed-protocol-owncloud-feeds))
           (length (length feeds)))
      (dotimes (i length)
        (let* ((feed (elt feeds i))
               (id (map-elt feed 'id))
               (url (map-elt feed 'url)))
          (when (string= url feed-url)
            (throw 'found id)))))))

(defun elfeed-protocol-owncloud--get-last-modified (proto-id)
  "Get last entry modified time.
PROTO-ID is the target protocol feed id.  If not initialized just return 0.  The
last modified time was saved in elfeed db as a mock feed."
  (let* ((feed (elfeed-db-get-feed proto-id))
         (last-modified (elfeed-meta feed :last-modified)))
    (if last-modified
        last-modified
      0)))

(defun elfeed-protocol-owncloud--set-last-modified (proto-id last-modified)
  "Set last entry modified time.
PROTO-ID is the target protocol feed id.  LAST-MODIFIED is the target value."
  (let* ((feed (elfeed-db-get-feed proto-id)))
    (setf (elfeed-meta feed :last-modified) last-modified)))

(defun elfeed-protocol-owncloud--get-last-entry-id (proto-id)
  "Get last entry id.
PROTO-ID is the target protocol feed id.  If not initialized, just return 0."
  (let* ((feed (elfeed-db-get-feed proto-id))
         (last-entry-id (elfeed-meta feed :last-entry-id)))
    (if last-entry-id
        last-entry-id
      0)))

(defun elfeed-protocol-owncloud--set-last-entry-id (proto-id last-entry-id)
  "Set last entry id to elfeed db.
PROTO-ID is the target protocol feed id.  LAST-ENTRY-ID is the target value."
  (let* ((feed (elfeed-db-get-feed proto-id)))
    (setf (elfeed-meta feed :last-entry-id) last-entry-id)))

(defun elfeed-protocol-owncloud-entry-p (entry)
  "Check if specific ENTRY is fetched from ownCloud News."
  (let* ((proto-id (elfeed-protocol-entry-protocol-id entry))
         (proto-type (when proto-id (elfeed-protocol-type proto-id))))
    (string= proto-type "owncloud")))

(defun elfeed-protocol-owncloud--parse-entries (url &optional mark-last-modified callback)
  "Parse the entries JSON buffer and fill results to elfeed db.
Ensure the point in the right place that `json-read' could execute.  URL is the
target ownCloud url to fetch entries.  If MARK-LAST-MODIFIED is nil, then just
not update the last-modifed value.  If CALLBACK is not nil, will call it with
the result entries as argument.  Return parsed entries.

User could download items.json from ownCloud manually, for example
http://myhost.com/items?type=3&batchSize=-1, and import the entries by calling
`elfeed-protocol-owncloud--parse-entries' in the buffer."
  (if elfeed-protocol-owncloud-feeds
      (let* ((proto-id (elfeed-protocol-owncloud-id url))
             (begin-time (time-to-seconds))
             (max-last-modified 0)
             (max-last-entry-id 0)
             items entries)
        (elfeed-log 'debug "elfeed-protocol-owncloud: parsing entries")
        (setq items (map-elt (json-read) 'items))
        (setq entries
              (cl-loop for item across items collect
                       (pcase-let* (((map id ('guidHash guid-hash) ('feedId feed-id) ('url entry-url) title
                                          author ('pubDate pub-date) body ('lastModified last-modified)
                                          ('enclosureMime enclosure-mime) ('enclosureLink enclosure-link))
                                     item)
                                    (feed-url (elfeed-protocol-owncloud--get-feed-url url feed-id))
                                    (unread (not (eq (map-elt item 'unread)
                                                     ':json-false)))
                                    (starred (not (eq (map-elt item 'starred)
                                                      ':json-false)))

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
                                              (push elfeed-protocol-owncloud-star-tag fixtags))
                                            fixtags))
                                    (enclosures (when enclosure-link
                                                  (list (list enclosure-link
                                                              enclosure-mime 0))))
                                    (db-entry (elfeed-entry--create
                                               :title (elfeed-cleanup title)
                                               :id full-id
                                               :feed-id (elfeed-protocol-format-subfeed-id
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
                           (if starred (elfeed-tag-1 original elfeed-protocol-owncloud-star-tag)
                             (elfeed-untag-1 original elfeed-protocol-owncloud-star-tag)))
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
          (elfeed-protocol-owncloud--set-last-modified proto-id max-last-modified))
        (when (and mark-last-modified (> max-last-entry-id 0))
          (elfeed-protocol-owncloud--set-last-entry-id proto-id max-last-entry-id))
        (elfeed-db-add entries)
        (when callback (funcall callback entries))
        (elfeed-log 'debug "elfeed-protocol-owncloud: parse %s entries finished with %ss"
                    (length entries) (- (time-to-seconds) begin-time))
        entries)
    (progn
      (elfeed-log 'error "Warning: elfeed-protocol-owncloud-feeds is nil, please call elfeed-protocol-owncloud--update-feed-list first")
      nil)))

(defun elfeed-protocol-owncloud--do-update (url action &optional arg callback)
  "Real ownCloud updating operations.
URL is the host name of ownCloud server, and authentication info is always
required, for example \"https://user:pass@myhost.com\". ACTION could be init,
update-feed, update-offset, or a timestamp. For init, will fetch unread and
starred entries. For update-feed, will fetch unread entries for special feed,
the ARG is the feed id. For update-offset, will fetch all entries after the
provide offset id. And for a timestamp means only update entries since the
special time, the ARG is the time-stamp.  If CALLBACK is not nil, will call it
with the result entries as argument."
  (let* ((proto-id (elfeed-protocol-owncloud-id url))
         (offset-id (if arg arg (elfeed-protocol-owncloud--get-last-entry-id proto-id)))
         (url-update-offset (concat url (format elfeed-protocol-owncloud-api-update-offset
                                                offset-id elfeed-protocol-owncloud-maxsize)))
         (url-init-unread (concat url (format
                                       elfeed-protocol-owncloud-api-init-unread
                                       elfeed-protocol-owncloud-maxsize)))
         (url-init-starred (concat url (format
                                        elfeed-protocol-owncloud-api-init-starred
                                        elfeed-protocol-owncloud-maxsize)))
         (mark-last-modified t)
         url-opt)
    (cond
     ;; initial sync, fetch unread entries
     ((eq action 'init) (setq url-opt url-init-unread))
     ;; update entries for special feed
     ((eq action 'update-feed)
      (setq mark-last-modified nil)
      (setq url-opt (concat url
                        (format elfeed-protocol-owncloud-api-update-feed arg
                                elfeed-protocol-owncloud-maxsize))))
     ;; update all entries
     ((eq action 'update-offset) (setq url-opt url-update-offset))
     ;; update since last modified, action is the time-stamp
     ((eq action 'timestamp)
      (setq url-opt (concat url (format elfeed-protocol-owncloud-api-update arg)))))
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (elfeed-protocol-owncloud-with-fetch url-opt nil
      (elfeed-protocol-owncloud--parse-entries url mark-last-modified callback)
      (run-hook-with-args 'elfeed-update-hooks url-opt))
    (when (eq action 'init)
      ;; initial sync, fetch starred entries
      (elfeed-protocol-owncloud-with-fetch url-init-starred nil
        ;; do not remember the last-modifed for starred entries, for
        ;; they always not the last entries.
        (elfeed-protocol-owncloud--parse-entries url nil callback)
        (run-hook-with-args 'elfeed-update-hooks url-init-starred)))))

(defun elfeed-protocol-owncloud-reinit (url)
  "Retry initial sync operation.
Will fetch all unread and starred entries from ownCloud News.  URL is the host
name of ownCloud server.  This may take a long time, ensure
`elfeed-curl-timeout' is big enough."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-owncloud-with-fetch
   (concat url elfeed-protocol-owncloud-api-feeds) nil
   (elfeed-protocol-owncloud--parse-feeds url)
   (elfeed-protocol-owncloud--do-update url 'init)))

(defun elfeed-protocol-owncloud-update-skip (url &optional timestamp)
  "Update entries since special timestamp.
URL is the host name of ownCloud server.  TIMESTAMP is the seconds since
1970-01-01 00:00:00 UTC, the default timestamp just point to 1 hours ago."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (unless timestamp
    (setq timestamp (- (time-to-seconds) (* 1 3600))))
  (elfeed-protocol-owncloud-with-fetch
   (concat url elfeed-protocol-owncloud-api-feeds) nil
   (elfeed-protocol-owncloud--parse-feeds url)
   (elfeed-protocol-owncloud--do-update url 'timestamp timestamp)))

(defun elfeed-protocol-owncloud-update-offset (url &optional offset)
  "Fetch all the entries after the offset id.
URL is the host name of ownCloud server.  If OFFSET not
provide, will update since the last entry id."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-owncloud-with-fetch
   (concat url elfeed-protocol-owncloud-api-feeds) nil
   (elfeed-protocol-owncloud--parse-feeds url)
   (elfeed-protocol-owncloud--do-update url 'update-offset offset)))

(defun elfeed-protocol-owncloud-mark-read (url entry)
  "Notify special entry as read.
URL is the host name of ownCloud server.  ENTRY is the target entry object."
  (let* ((id (elfeed-meta entry :id))
         (url (concat url (format elfeed-protocol-owncloud-api-read id))))
    (elfeed-protocol-owncloud-with-fetch url "{}")))

(defun elfeed-protocol-owncloud-mark-unread (url entry)
  "Notify special entry as unread.
URL is the host name of ownCloud server.  ENTRY is the target entry object."
  (let* ((id (elfeed-meta entry :id))
         (url (concat url (format elfeed-protocol-owncloud-api-unread id))))
    (elfeed-protocol-owncloud-with-fetch url "{}")))

(defun elfeed-protocol-owncloud-mark-star (url entry)
  "Notify special entry as starred.
URL is the host name of ownCloud server.  ENTRY is the target entry object."
  (let* ((feed-id (elfeed-meta entry :feed-id))
         (guid-hash (elfeed-meta entry :guid-hash))
         (url (concat url (format elfeed-protocol-owncloud-api-star
                                  feed-id guid-hash))))
    (elfeed-protocol-owncloud-with-fetch url "{}")))

(defun elfeed-protocol-owncloud-mark-unstar (url entry)
  "Notify special entry as unstarred.
URL is the host name of ownCloud server.  ENTRY is the target entry object."
  (let* ((feed-id (elfeed-meta entry :feed-id))
         (guid-hash (elfeed-meta entry :guid-hash))
         (url (concat url (format elfeed-protocol-owncloud-api-unstar
                                  feed-id guid-hash))))
    (elfeed-protocol-owncloud-with-fetch url "{}")))

(defun elfeed-protocol-owncloud-mark-read-multi (url entries)
  "Notify multiple entries to be read.
URL is the host name of ownCloud server.  ENTRIES is the target entry objects."
  (let* ((url (concat url elfeed-protocol-owncloud-api-read-multi))
         (ids (cl-loop for entry in entries collect
                       (when (elfeed-protocol-owncloud-entry-p entry)
                         (elfeed-meta entry :id))))
         (data (json-encode-list (list (cons 'items ids)))))
    (when ids
      (elfeed-protocol-owncloud-with-fetch url data))))

(defun elfeed-protocol-owncloud-mark-unread-multi (url entries)
  "Notify multiple entries to be unread.
URL is the host name of ownCloud server.  ENTRIES is the target entry objects."
  (let* ((url (concat url elfeed-protocol-owncloud-api-unread-multi))
         (ids (cl-loop for entry in entries collect
                       (when (elfeed-protocol-owncloud-entry-p entry)
                         (elfeed-meta entry :id))))
         (data (json-encode-list (list (cons 'items ids)))))
    (when ids
      (elfeed-protocol-owncloud-with-fetch url data))))

(defun elfeed-protocol-owncloud-mark-star-multi (url entries)
  "Notify multiple entries to be starred.
URL is the host name of ownCloud server.  ENTRIES is the target entry objects."
  (let* ((url (concat url elfeed-protocol-owncloud-api-star-multi))
         (items (cl-loop for entry in entries collect
                         (when (elfeed-protocol-owncloud-entry-p entry)
                           (let* ((feed-id (elfeed-meta entry :feed-id))
                                  (guid-hash (elfeed-meta entry :guid-hash)))
                             (list (cons 'feedId feed-id)
                                   (cons 'guidHash guid-hash))))))
         (data (json-encode-list (list (cons 'items items)))))
    (when items
      (elfeed-protocol-owncloud-with-fetch url data))))

(defun elfeed-protocol-owncloud-mark-unstar-multi (url entries)
  "Notify multiple entries to be unstarred.
URL is the host name of ownCloud server.  ENTRIES is the target entry objects."
  (let* ((url (concat url elfeed-protocol-owncloud-api-unstar-multi))
         (items (cl-loop for entry in entries collect
                         (when (elfeed-protocol-owncloud-entry-p entry)
                           (let* ((feed-id (elfeed-meta entry :feed-id))
                                  (guid-hash (elfeed-meta entry :guid-hash)))
                             (list (cons 'feedId feed-id)
                                   (cons 'guidHash guid-hash))))))
         (data (json-encode-list (list (cons 'items items)))))
    (when items
      (elfeed-protocol-owncloud-with-fetch url data))))

(defun elfeed-protocol-owncloud-sync-tag-multi (url entries tag action)
  "Notify multiple entries to be unread.
URL is the host name of ownCloud server.  ENTRIES is the target entry
objects.  TAG is the action tag, for example unread and
`elfeed-protocol-owncloud-star-tag', ACTION could be add or remove."
  (cond
   ((eq action 'add)
    (cond
     ((eq tag 'unread) (elfeed-protocol-owncloud-mark-unread-multi url entries))
     ((eq tag elfeed-protocol-owncloud-star-tag)
      (elfeed-protocol-owncloud-mark-star-multi url entries))))
   ((eq action 'remove)
    (cond
     ((eq tag 'unread) (elfeed-protocol-owncloud-mark-read-multi url entries))
     ((eq tag elfeed-protocol-owncloud-star-tag)
      (elfeed-protocol-owncloud-mark-unstar-multi url entries))))))

(defun elfeed-protocol-owncloud-pre-tag (url entries &rest tags)
  "Sync unread/starred stats before tags added.
URL is the host name of ownCloud server.  ENTRIES is the target entry
objects.  TAGS is the tags are adding now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-owncloud-sync-tag-multi url entries-modified tag 'add))))

(defun elfeed-protocol-owncloud-pre-untag (url entries &rest tags)
  "Sync unread/starred stats before tags removed.
URL is the host name of ownCloud server.  ENTRIES is the target entry
objects.  TAGS is the tags are removing now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-owncloud-sync-tag-multi url entries-modified tag 'remove))))

(defun elfeed-protocol-owncloud-update-feed (url feed-url &optional callback)
  "Update child feed in ownCloud News.
URL is the host name of ownCloud server, FEED-URL is the target child feed url,
if CALLBACK is not nil will call it with the result entries as argument."
  (interactive)
  (let* ((feed-id (elfeed-protocol-owncloud--get-feed-id url feed-url)))
    (when feed-id
      (elfeed-protocol-owncloud--do-update url 'update-feed feed-id callback))))

(defun elfeed-protocol-owncloud-update (url &optional callback)
  "OwnCloud News protocol updater.
URL is the host name of ownCloud server, and authentication info is always
required, for example \"https://user:pass@myhost.com\". If first time run, it
will initial sync for target URL, or will only fetch the updated entries since
last modified. if CALLBACK is not nil will call it with the result entries as
argument"
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((host-url (elfeed-protocol-host-url url))
         (subfeed-url (elfeed-protocol-subfeed-url url)))
    (if subfeed-url (elfeed-protocol-owncloud-update-feed host-url subfeed-url callback)
      (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
             (last-modified (elfeed-protocol-owncloud--get-last-modified proto-id)))
        (elfeed-protocol-owncloud-with-fetch
         (concat host-url elfeed-protocol-owncloud-api-feeds) nil
         (elfeed-protocol-owncloud--parse-feeds host-url)
         (if (> last-modified 0)
             (elfeed-protocol-owncloud--do-update host-url 'timestamp (+ 1 last-modified) callback)
       (elfeed-protocol-owncloud--do-update host-url 'init nil callback)))))))

(provide 'elfeed-protocol-owncloud)

;;; elfeed-protocol-owncloud.el ends here
