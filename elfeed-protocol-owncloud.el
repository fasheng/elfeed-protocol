;;; elfeed-protocol-owncloud.el --- ownCloud News protocol for elfeed -*- lexical-binding: t; -*-

;;; Commentary:
;; ownCloud News protocol for elfeed.

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'elfeed)
(require 'elfeed-protocol-common)

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

(defcustom elfeed-protocol-owncloud-update-with-modified-time t
  "Determine default update method for ownCloud News.
If t will update since last modified time, and if nil will update since last entry id."
  :group 'elfeed-protocol
  :type 'boolean)

(defvar elfeed-protocol-owncloud-feeds (make-hash-table :test 'equal)
  "Feed list from ownCloud News, will be filled before updating operation.")

(defconst elfeed-protocol-owncloud-api-base "/index.php/apps/news/api/v1-2")
(defconst elfeed-protocol-owncloud-api-feeds (concat elfeed-protocol-owncloud-api-base "/feeds"))
(defconst elfeed-protocol-owncloud-api-init-unread (concat elfeed-protocol-owncloud-api-base "/items?type=3&getRead=false&batchSize=%s"))
(defconst elfeed-protocol-owncloud-api-init-starred (concat elfeed-protocol-owncloud-api-base "/items?type=2&getRead=true&batchSize=-1"))
(defconst elfeed-protocol-owncloud-api-update (concat elfeed-protocol-owncloud-api-base "/items/updated?type=3&lastModified=%s"))
(defconst elfeed-protocol-owncloud-api-update-subfeed (concat elfeed-protocol-owncloud-api-base "/items?type=0&id=%s&getRead=false&batchSize=%s"))
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
  (let* ((proto-id (elfeed-protocol-owncloud-id url))
         (user (elfeed-protocol-meta-user proto-id))
         (password (elfeed-protocol-meta-password proto-id))
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
                  (progn
                    (unless use-curl
                      (elfeed-move-to-first-empty-line)
                      (set-buffer-multibyte t))
                    (when elfeed-protocol-log-trace
                      (elfeed-log 'debug "elfeed-protocol-owncloud: %s" (buffer-string)))
                    ,@body
                    (unless use-curl
                      (kill-buffer)))))))
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

(defun elfeed-protocol-owncloud--parse-feeds (host-url)
  "Parse the feeds JSON buffer and fill results to db.
HOST-URL is the host name of ownCloud server.  Ensure the point in the
right place that `json-read' could execute.  Return
`elfeed-protocol-owncloud-feeds'."
  (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
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
    (elfeed-log 'debug "elfeed-protocol-owncloud: found %s feeds" (length feeds))
    elfeed-protocol-owncloud-feeds))

(defun elfeed-protocol-owncloud--update-feed-list (host-url)
  "Update ownCloud News feed list.
HOST-URL is the host name of ownCloud server."
  (elfeed-log 'debug "elfeed-protocol-owncloud: update feed list")
  (elfeed-protocol-owncloud-with-fetch
    (concat host-url elfeed-protocol-owncloud-api-feeds)
    nil (elfeed-protocol-owncloud--parse-feeds host-url)))

(defun elfeed-protocol-owncloud--get-subfeed-url (host-url feed-id)
  "Get sub feed url for the ownCloud protocol feed HOST-URL and FEED-ID."
  (let* ((url (catch 'found
                (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
                       (feeds (gethash proto-id elfeed-protocol-owncloud-feeds))
                       (length (length feeds)))
                  (dotimes (i length)
                    (let* ((feed (elt feeds i))
                           (id (map-elt feed 'id))
                           (url (map-elt feed 'url)))
                      (when (eq id feed-id)
                        (throw 'found url))))))))
    (unless url
      (setq url elfeed-protocol-unknown-feed-url)
      (elfeed-log 'warn "elfeed-protocol-owncloud: no subfeed for feed id %s, fallback to unknown feed" feed-id))
    url))

(defun elfeed-protocol-owncloud--get-subfeed-id (host-url feed-url)
  "Get sub feed url for the owncloud protocol feed HOST-URL and FEED-URL."
  (let* ((id (catch 'found
               (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
                      (feeds (gethash proto-id elfeed-protocol-owncloud-feeds))
                      (length (length feeds)))
                 (dotimes (i length)
                   (let* ((feed (elt feeds i))
                          (id (map-elt feed 'id))
                          (url (map-elt feed 'url)))
                     (when (string= url feed-url)
                       (throw 'found id))))))))
    (unless id
      (elfeed-log 'error "elfeed-protocol-owncloud: no subfeed for feed url %s" feed-url))
    id))

(defun elfeed-protocol-owncloud-entry-p (entry)
  "Check if specific ENTRY is fetched from ownCloud News."
  (let* ((proto-id (elfeed-protocol-entry-protocol-id entry))
         (proto-type (when proto-id (elfeed-protocol-type proto-id))))
    (string= proto-type "owncloud")))

(defun elfeed-protocol-owncloud--parse-entries (host-url &optional mark-state callback)
  "Parse the entries JSON buffer and fill results to elfeed db.
Ensure the point in the right place that `json-read' could execute.
HOST-URL is the host name of ownCloud server.  If MARK-STATE is nil,
then just not update the :last-modifed, :first-entry-id
and :last-entry-id values.  If CALLBACK is not nil, will call it with
the result entries as argument.  Return parsed entries.

User could download items.json from ownCloud manually, for example
http://myhost.com/items?type=3&batchSize=-1, and import the entries by calling
`elfeed-protocol-owncloud--parse-entries' in the buffer."
  (if (> (hash-table-count elfeed-protocol-owncloud-feeds) 0)
      (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
             (unread-num 0)
             (starred-num 0)
             (begin-time (time-to-seconds))
             (min-first-entry-id (elfeed-protocol-get-first-entry-id proto-id))
             (max-last-entry-id (elfeed-protocol-get-last-entry-id proto-id))
             (max-last-modified (elfeed-protocol-get-last-modified proto-id))
             items entries)
        (elfeed-log 'debug "elfeed-protocol-owncloud: parsing entries, first-entry-id: %d last-entry-id: %d last-modified: %d"
                    (elfeed-protocol-get-first-entry-id proto-id)
                    (elfeed-protocol-get-last-entry-id proto-id)
                    (elfeed-protocol-get-last-modified proto-id))
        (setq items (map-elt (json-read) 'items))
        (setq entries
              (cl-loop for item across items collect
                       (pcase-let* (((map id ('guidHash guid-hash) ('feedId feed-id) ('url entry-url) title
                                          author ('pubDate pub-date) body ('lastModified last-modified)
                                          ('enclosureMime enclosure-mime) ('enclosureLink enclosure-link))
                                     item)
                                    (feed-url (elfeed-protocol-owncloud--get-subfeed-url host-url feed-id))
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
                         (when unread (setq unread-num (1+ unread-num)))
                         (when starred (setq starred-num (1+ starred-num)))

                         ;; force override unread and star tags without repeat sync operation
                         (when original
                           (if unread (elfeed-tag-1 original 'unread)
                             (elfeed-untag-1 original 'unread))
                           (if starred (elfeed-tag-1 original elfeed-protocol-owncloud-star-tag)
                             (elfeed-untag-1 original elfeed-protocol-owncloud-star-tag)))

                         ;; calculate the last modified time and first last entry id
                         (when (or (< min-first-entry-id 0) (< id min-first-entry-id))
                           (setq min-first-entry-id id))
                         (when (or (< max-last-entry-id 0) (> id max-last-entry-id))
                           (setq max-last-entry-id id))
                         (when (> last-modified max-last-modified)
                           (setq max-last-modified last-modified))

                         (dolist (hook elfeed-new-entry-parse-hook)
                           (run-hook-with-args hook :owncloud item db-entry))
                         db-entry)))
        (elfeed-db-add entries)
        (when callback (funcall callback entries))

        ;; update last modified time and first last entry id
        (when (and mark-state (>= min-first-entry-id 0))
          (elfeed-protocol-set-first-entry-id proto-id min-first-entry-id))
        (when (and mark-state (>= max-last-entry-id 0))
          (elfeed-protocol-set-last-entry-id proto-id max-last-entry-id))
        (when (and mark-state (> max-last-modified 0))
          (elfeed-protocol-set-last-modified proto-id max-last-modified))

        (elfeed-log 'debug "elfeed-protocol-owncloud: parsed %s entries(%d unread, %d starred) with %fs, first-entry-id: %d last-entry-id: %d last-modified: %d"
                    (length entries) unread-num starred-num
                    (- (time-to-seconds) begin-time)
                    (elfeed-protocol-get-first-entry-id proto-id)
                    (elfeed-protocol-get-last-entry-id proto-id)
                    (elfeed-protocol-get-last-modified proto-id))
        entries)
    (progn
      (elfeed-log 'error "elfeed-protocol-owncloud:: elfeed-protocol-owncloud-feeds is nil, please call elfeed-protocol-owncloud--update-feed-list first")
      nil)))

(defun elfeed-protocol-owncloud--do-update (host-url action &optional arg callback)
  "Real ownCloud updating operations.
HOST-URL is the host name of ownCloud server, and user field
authentication info is always required so could find the related
protocol feed id correctly, for example
\"https://user:pass@myhost.com\". ACTION could be init,
update-since-time, update-subfeed and update-since-id. For init, will
fetch unread and starred entries. For update-subfeed, will fetch
unread entries for special sub feed, the ARG is the feed id. For
update-since-id, will fetch all entries after the provide entry
id. And for a update-since-time means only update entries since the
special modified time, the ARG is the time-stamp.  If CALLBACK is not
nil, will call it with the result entries as argument."
  (elfeed-log 'debug "elfeed-protocol-owncloud: update entries with action %s, arg %s" action arg)
  (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
         (offset-id (if arg arg (elfeed-protocol-get-last-entry-id proto-id)))
         (url-update-since-id (concat host-url (format elfeed-protocol-owncloud-api-update-offset
                                                       offset-id elfeed-protocol-owncloud-maxsize)))
         (url-init-unread (concat host-url (format
                                            elfeed-protocol-owncloud-api-init-unread
                                            elfeed-protocol-owncloud-maxsize)))
         (url-init-starred (concat host-url elfeed-protocol-owncloud-api-init-starred))
         (mark-state t)
         url-opt)
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (cond
     ;; initial sync, fetch unread entries
     ((eq action 'init)
      (elfeed-protocol-set-last-modified proto-id 0)
      (elfeed-protocol-set-first-entry-id proto-id -1)
      (elfeed-protocol-set-last-entry-id proto-id -1)
      (elfeed-protocol-clean-pending-ids proto-id)
      (setq url-opt url-init-unread))
     ;; update entries since last modified
     ((eq action 'update-since-time)
      (setq url-opt (concat host-url (format elfeed-protocol-owncloud-api-update arg))))
     ;; update entries since special entry id
     ((eq action 'update-since-id) (setq url-opt url-update-since-id))
     ;; update entries for special sub feed
     ((eq action 'update-subfeed)
      (setq mark-state nil)
      (setq url-opt (concat host-url
                            (format elfeed-protocol-owncloud-api-update-subfeed arg
                                    elfeed-protocol-owncloud-maxsize)))))
    (elfeed-protocol-owncloud-with-fetch url-opt nil
      (elfeed-protocol-owncloud--parse-entries host-url mark-state callback)
      (run-hook-with-args 'elfeed-update-hooks host-url))
    (when (eq action 'init)
      ;; initial sync, fetch starred entries
      (elfeed-protocol-owncloud-with-fetch url-init-starred nil
        ;; do not remember the last-modifed for starred entries, for
        ;; they always not the last entries.
        (elfeed-protocol-owncloud--parse-entries host-url nil callback)
        (run-hook-with-args 'elfeed-update-hooks url-init-starred)))))

(defun elfeed-protocol-owncloud-reinit (host-url)
  "Retry initial sync operation.
Will fetch all unread and starred entries from ownCloud News.
HOST-URL is the host name of ownCloud server.  This may take a long
time, ensure `elfeed-curl-timeout' is big enough."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-owncloud-with-fetch
    (concat host-url elfeed-protocol-owncloud-api-feeds) nil
    (elfeed-protocol-owncloud--parse-feeds host-url)
    (elfeed-protocol-owncloud--do-update host-url 'init)))

(defun elfeed-protocol-owncloud-update-since-timestamp (host-url &optional timestamp)
  "Update entries since special timestamp.
HOST-URL is the host name of ownCloud server.  TIMESTAMP is the
seconds since 1970-01-01 00:00:00 UTC, the default timestamp just
point to 1 hours ago."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (unless timestamp
    (setq timestamp (- (time-to-seconds) (* 1 3600))))
  (elfeed-protocol-owncloud-with-fetch
    (concat host-url elfeed-protocol-owncloud-api-feeds) nil
    (elfeed-protocol-owncloud--parse-feeds host-url)
    (elfeed-protocol-owncloud--do-update host-url 'update-since-time timestamp)))

(defun elfeed-protocol-owncloud-update-since-id (host-url &optional id)
  "Fetch entries after special id.
HOST-URL is the host name of ownCloud server.  If ID not provide, will
update since the last entry id."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-owncloud-with-fetch
    (concat host-url elfeed-protocol-owncloud-api-feeds) nil
    (elfeed-protocol-owncloud--parse-feeds host-url)
    (elfeed-protocol-owncloud--do-update host-url 'update-since-id id)))

(defun elfeed-protocol-owncloud-update-older (host-url)
  "Fetch older entries which entry id less than :first-entry-id.
HOST-URL is the host name of ownCloud server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
         (first-entry-id (elfeed-protocol-get-first-entry-id proto-id))
         (since-id (- first-entry-id elfeed-protocol-owncloud-maxsize)))
    (elfeed-protocol-owncloud-update-since-id host-url since-id)))

(defun elfeed-protocol-owncloud-mark-read (host-url id)
  "Notify special entry as read.
HOST-URL is the host name of ownCloud server.  ID is the target entry id."
  (let* ((url (concat host-url (format elfeed-protocol-owncloud-api-read id))))
    (elfeed-protocol-owncloud-with-fetch url "{}")))

(defun elfeed-protocol-owncloud-mark-unread (host-url id)
  "Notify special entry as unread.
HOST-URL is the host name of ownCloud server.  ID is the target entry id."
  (let* ((url (concat host-url (format elfeed-protocol-owncloud-api-unread id))))
    (elfeed-protocol-owncloud-with-fetch url "{}")))

(defun elfeed-protocol-owncloud-mark-starred (host-url star-id)
  "Notify special entry as starred.
HOST-URL is the host name of ownCloud server.  STAR-ID is the target entry id
with format (cons feed-id guid-hash)."
  (let* ((feed-id (car star-id))
         (guid-hash (cdr star-id))
         (url (concat host-url (format elfeed-protocol-owncloud-api-star
                                       feed-id guid-hash))))
    (elfeed-protocol-owncloud-with-fetch url "{}")))

(defun elfeed-protocol-owncloud-mark-unstarred (host-url star-id)
  "Notify special entry as unstarred.
HOST-URL is the host name of ownCloud server.  STAR-ID is the target entry id
with format (cons feed-id guid-hash)."
  (let* ((feed-id (car star-id))
         (guid-hash (cdr star-id))
         (url (concat host-url (format elfeed-protocol-owncloud-api-unstar
                                       feed-id guid-hash))))
    (elfeed-protocol-owncloud-with-fetch url "{}")))

(defun elfeed-protocol-owncloud-mark-read-multi (host-url ids)
  "Notify multiple entries to be read.
HOST-URL is the host name of ownCloud server.  IDS is the target entry ids."
  (let* ((url (concat host-url elfeed-protocol-owncloud-api-read-multi))
         (data (json-encode-list `((items . ,ids)))))
    (when ids
      (elfeed-log 'debug "elfeed-protocol-owncloud: mark read, ids: %s" ids)
      (elfeed-protocol-owncloud-with-fetch url data))))

(defun elfeed-protocol-owncloud-mark-unread-multi (host-url ids)
  "Notify multiple entries to be unread.
HOST-URL is the host name of ownCloud server.  IDS is the target entry ids."
  (let* ((url (concat host-url elfeed-protocol-owncloud-api-unread-multi))
         (data (json-encode-list `((items . ,ids)))))
    (when ids
      (elfeed-log 'debug "elfeed-protocol-owncloud: mark unread, ids: %s" ids)
      (elfeed-protocol-owncloud-with-fetch url data))))

(defun elfeed-protocol-owncloud-mark-starred-multi (host-url star-ids)
  "Notify multiple entries to be starred.
HOST-URL is the host name of ownCloud server.  STAR-IDS is the target entry ids
with format (list (cons feed-id guid-hash))."
  (let* ((url (concat host-url elfeed-protocol-owncloud-api-star-multi))
         (items (cl-loop for star-id in star-ids collect
                         (let* ((feed-id (car star-id))
                                (guid-hash (cdr star-id)))
                           `((feedId . ,feed-id)
                             (guidHash . ,guid-hash)))))
         (data (json-encode-list `((items . ,items)))))
    (when items
      (elfeed-log 'debug "elfeed-protocol-owncloud: mark starred, star-ids: %s" star-ids)
      (elfeed-protocol-owncloud-with-fetch url data))))

(defun elfeed-protocol-owncloud-mark-unstarred-multi (host-url star-ids)
  "Notify multiple entries to be unstarred.
HOST-URL is the host name of ownCloud server.  STAR-IDS is the target entry ids
with format (list (cons feed-id guid-hash))."
  (let* ((url (concat host-url elfeed-protocol-owncloud-api-unstar-multi))
         (items (cl-loop for star-id in star-ids collect
                         (let* ((feed-id (car star-id))
                                (guid-hash (cdr star-id)))
                           `((feedId . ,feed-id)
                             (guidHash . ,guid-hash)))))
         (data (json-encode-list `((items . ,items)))))
    (when items
      (elfeed-log 'debug "elfeed-protocol-owncloud: mark unstarred, star-ids: %s" star-ids)
      (elfeed-protocol-owncloud-with-fetch url data))))

(defun elfeed-protocol-owncloud-sync-pending-ids (host-url)
  "Sync pending read/unread/starred/unstarred entry states to ownCloud server.
HOST-URL is the host name of ownCloud server."
  (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
         (pending-read-ids (elfeed-protocol-get-pending-ids proto-id :pending-read))
         (pending-unread-ids (elfeed-protocol-get-pending-ids proto-id :pending-unread))
         (pending-starred-ids (elfeed-protocol-get-pending-ids proto-id :pending-starred))
         (pending-unstarred-ids (elfeed-protocol-get-pending-ids proto-id :pending-unstarred)))
    (when pending-read-ids (elfeed-protocol-owncloud-mark-read-multi host-url pending-read-ids))
    (when pending-unread-ids (elfeed-protocol-owncloud-mark-unread-multi host-url pending-unread-ids))
    (when pending-starred-ids (elfeed-protocol-owncloud-mark-starred-multi host-url pending-starred-ids))
    (when pending-unstarred-ids (elfeed-protocol-owncloud-mark-unstarred-multi host-url pending-unstarred-ids))
    (elfeed-protocol-clean-pending-ids proto-id)))

(defun elfeed-protocol-owncloud-append-pending-ids (host-url entries tag action)
  "Sync unread and starred tag states to ownCloud server.
HOST-URL is the host name of ownCloud server.  ENTRIES is the target
entry objects.  TAG is the action tag, for example unread and
`elfeed-protocol-owncloud-star-tag', ACTION could be add or remove."
  (when entries
    (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
           (ids (cl-loop for entry in entries collect
                         (when (elfeed-protocol-owncloud-entry-p entry)
                           (elfeed-meta entry :id))))
           (star-ids (cl-loop for entry in entries collect
                              (when (elfeed-protocol-owncloud-entry-p entry)
                                (cons (elfeed-meta entry :feed-id)
                                      (elfeed-meta entry :guid-hash))))))
      (cond
       ((eq action 'add)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-unread ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-read ids))
         ((eq tag elfeed-protocol-owncloud-star-tag)
          (elfeed-protocol-append-pending-ids proto-id :pending-starred star-ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-unstarred star-ids))))
       ((eq action 'remove)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-read ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-unread ids))
         ((eq tag elfeed-protocol-owncloud-star-tag)
          (elfeed-protocol-append-pending-ids proto-id :pending-unstarred star-ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-starred star-ids))))))))

(defun elfeed-protocol-owncloud-pre-tag (host-url entries &rest tags)
  "Sync unread, starred states before tags added.
HOST-URL is the host name of ownCloud server.  ENTRIES is the target
entry objects.  TAGS is the tags are adding now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-owncloud-append-pending-ids host-url entries-modified tag 'add)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-owncloud-sync-pending-ids host-url)))

(defun elfeed-protocol-owncloud-pre-untag (host-url entries &rest tags)
  "Sync unread, starred states before tags removed.
HOST-URL is the host name of ownCloud server.  ENTRIES is the target entry
objects.  TAGS is the tags are removing now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-owncloud-append-pending-ids host-url entries-modified tag 'remove)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-owncloud-sync-pending-ids host-url)))

(defun elfeed-protocol-owncloud-update-subfeed (host-url feed-url &optional callback)
  "Update sub feed in ownCloud News.
HOST-URL is the host name of ownCloud server, FEED-URL is the target
sub feed url, if CALLBACK is not nil will call it with the result
entries as argument."
  (interactive)
  (let* ((feed-id (elfeed-protocol-owncloud--get-subfeed-id host-url feed-url)))
    (when feed-id
      (elfeed-protocol-owncloud--do-update host-url 'update-subfeed feed-id callback))))

(defun elfeed-protocol-owncloud-update (host-or-subfeed-url &optional callback)
  "OwnCloud News protocol updater.
HOST-OR-SUBFEED-URL could be the host name of ownCloud server, and
user field authentication info is always required so could find the
related protocol feed id correctly, for example
\"https://user@myhost.com\".  And HOST-OR-SUBFEED-URL also could be the
sub feed url, too, for example
\"https://user@myhost.com::https://subfeed.com\".  If first time run,
it will initial sync operation, or will only fetch the updated entries
since last modified. if CALLBACK is not nil will call it with the
result entries as argument"
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((host-url (elfeed-protocol-host-url host-or-subfeed-url))
         (subfeed-url (elfeed-protocol-subfeed-url host-or-subfeed-url))
         (proto-id (elfeed-protocol-owncloud-id host-url)))
    (elfeed-protocol-add-unknown-feed proto-id) ; add unknown feed for fallback
    (elfeed-protocol-owncloud-sync-pending-ids host-url)
    (if subfeed-url (elfeed-protocol-owncloud-update-subfeed host-url subfeed-url callback)
      (let* ((proto-id (elfeed-protocol-owncloud-id host-url))
             (last-modified (elfeed-protocol-get-last-modified proto-id))
             (last-entry-id (elfeed-protocol-get-last-entry-id proto-id)))
        (elfeed-protocol-owncloud-with-fetch
          (concat host-url elfeed-protocol-owncloud-api-feeds) nil
          (elfeed-protocol-owncloud--parse-feeds host-url)
          (if (> last-modified 0)
              (if elfeed-protocol-owncloud-update-with-modified-time
                  (elfeed-protocol-owncloud--do-update host-url 'update-since-time last-modified callback)
                (elfeed-protocol-owncloud--do-update host-url 'update-since-id last-entry-id callback))
            (elfeed-protocol-owncloud--do-update host-url 'init nil callback)))))))

(provide 'elfeed-protocol-owncloud)

;;; elfeed-protocol-owncloud.el ends here
