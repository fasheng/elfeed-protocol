;;; elfeed-protocol-fever.el ---  Fever protocol for elfeed -*- lexical-binding: t; -*-

;;; Commentary:
;; Fever protocol for elfeed.

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'subr-x)
(require 'elfeed)
(require 'elfeed-protocol-common)

;;; Code:

(defcustom elfeed-protocol-fever-maxsize 50
  "Maximize entries size for each request.
Fever API limit a maximum of 50, so set bigger than 50 just invalid."
  :group 'elfeed-protocol
  :type 'integer)

(defcustom elfeed-protocol-fever-star-tag 'star
  "Default star tag for Fever entry.
If one entry set or remove the tag,
then the starred state in Fever will be synced, too."
  :group 'elfeed-protocol
  :type 'symbol)

(defvar elfeed-protocol-fever-feeds (make-hash-table :test 'equal)
  "Feed list from Fever, will be filled before updating operation.")

(defconst elfeed-protocol-fever-api-base "?api")
(defconst elfeed-protocol-fever-api-feeds (concat elfeed-protocol-fever-api-base "&feeds"))
(defconst elfeed-protocol-fever-api-items (concat elfeed-protocol-fever-api-base "&items"))
(defconst elfeed-protocol-fever-api-saved-item-ids (concat elfeed-protocol-fever-api-base "&saved_item_ids"))
(defconst elfeed-protocol-fever-api-unread-item-ids (concat elfeed-protocol-fever-api-base "&unread_item_ids"))
(defconst elfeed-protocol-fever-api-item-state-read "read")
(defconst elfeed-protocol-fever-api-item-state-unread "unread")
(defconst elfeed-protocol-fever-api-item-state-saved "saved")
(defconst elfeed-protocol-fever-api-item-state-unsaved "unsaved")

(defun elfeed-protocol-fever-id (url)
  "Get fever protocol id with URL."
  (elfeed-protocol-id "fever" url))

(defun elfeed-protocol-fever-entry-p (entry)
  "Check if specific ENTRY is fetched from Fever."
  (let* ((proto-id (elfeed-protocol-entry-protocol-id entry))
         (proto-type (when proto-id (elfeed-protocol-type proto-id))))
    (string= proto-type "fever")))

(defun elfeed-protocol-fever--init-headers ()
  "Get http request headers for fever."
  `(("User-Agent" . ,elfeed-user-agent)))

(defun elfeed-protocol-fever--get-api-url (host-url)
  "Get fever server API url.
HOST-URL is the host name of Fever server."
  (let* ((proto-id (elfeed-protocol-fever-id host-url))
         (url (elfeed-protocol-meta-data proto-id :api-url)))
    url))

(defun elfeed-protocol-fever--build-data (host-url &optional parameter)
  "Build data string that sent to Fever server.
HOST-URL is the host name of Fever server. PARAMETER is optional"
  (let* ((proto-id (elfeed-protocol-fever-id host-url))
         (user (elfeed-protocol-meta-user proto-id))
         (password (elfeed-protocol-meta-password proto-id))
         (data (concat "api_key=" (md5 (concat user ":" password)))))
    (if parameter
        (concat data "&" parameter)
      data)))

(defun elfeed-protocol-fever-get-update-mark (proto-id update-action)
  "Get last update mark for special UPDATE-ACTION.
PROTO-ID is the target protocol feed id.  UPDATE-ACTION could be update or
update-older.  If not initialized, just return -1."
  (interactive (list (completing-read "Protocol Feed: " (elfeed-protocol-feed-list))
                     (intern (completing-read "Update action: " '(update update-older)))))
  (let* ((key (cond
               ((eq update-action 'update) :last-entry-id)
               ((eq update-action 'update-older) :first-entry-id)))
         (mark (elfeed-protocol-get-feed-meta-data proto-id key)))
    (if mark mark -1)))

(defun elfeed-protocol-fever-set-update-mark (proto-id update-action mark)
  "Set last update mark to elfeed db.
PROTO-ID is the target protocol feed id.  UPDATE-ACTION could be update or
update-older.  MARK the target value."
  (interactive (list (completing-read "Protocol Feed: " (elfeed-protocol-feed-list))
                     (intern (completing-read "Update action: " '(update update-older)))
                     (read-number "Mark number: ")))
  (let* ((key (cond
               ((eq update-action 'update) :last-entry-id)
               ((eq update-action 'update-older) :first-entry-id))))
    (elfeed-protocol-set-feed-meta-data proto-id key mark)))

(defmacro elfeed-protocol-fever-with-fetch (url method data &rest body)
  "Just like `elfeed-with-fetch' but special for fever HTTP request.
URL is the Fever api url to request, METHOD could be \"GET\" or \"POST\",
DATA is the string to send.  Optional argument BODY is the rest Lisp code after
operation finished."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl) ; capture current value in closure
          (headers (elfeed-protocol-fever--init-headers))
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
                      (elfeed-log 'debug "elfeed-protocol-fever: %s" (buffer-string)))
                    (elfeed-protocol-fever--parse-result ,@body)
                    (unless use-curl
                      (kill-buffer)))))))
     (if use-curl
         (elfeed-curl-enqueue no-auth-url cb :headers headers
                              :method ,method :data ,data)
       (let ((url-request-extra-headers headers)
             (url-request-method ,method)
             (url-request-data ,data))
         (url-retrieve no-auth-url cb () t t)))))

(defmacro elfeed-protocol-fever--parse-result (&rest body)
  "Parse fever api result JSON buffer.
Will eval rest BODY expressions at end."
  (declare (indent defun))
  `(let* ((result (json-read))
          (errmsg (map-elt result 'error)))
     (if errmsg
         (elfeed-log 'error "elfeed-protocol-fever: %s" errmsg)
       ,@body)))

(defmacro elfeed-protocol-fever-fetch-prepare (host-url &rest body)
  "Ensure logged in and feed list updated before expressions.
HOST-URL is the host name of Fever server.  And will eval rest
BODY expressions at end."
  (declare (indent defun))
  `(elfeed-protocol-fever--update-feed-list
    ,host-url (lambda () ,@body)))

(defun elfeed-protocol-fever--update-feed-list (host-url &optional callback)
  "Update Fever server feeds list.
HOST-URL is the host name of Fever server.  Will call CALLBACK
at end."
  (elfeed-log 'debug "elfeed-protocol-fever: update feed list")
  (let* ((url (concat (elfeed-protocol-fever--get-api-url host-url)
                      elfeed-protocol-fever-api-feeds))
         (data (elfeed-protocol-fever--build-data host-url)))
    (elfeed-protocol-fever-with-fetch
      url "POST" data
      (elfeed-protocol-fever--parse-feeds host-url (map-elt result 'feeds))
      (when callback (funcall callback)))))

(defun elfeed-protocol-fever--parse-feeds (host-url feeds)
  "Parse the feeds JSON buffer and fill results to db.
HOST-URL is the host name of Fever server.  FEEDS is the result JSON content by
http request.  Return `elfeed-protocol-fever-feeds'."
  (let* ((proto-id (elfeed-protocol-fever-id host-url)))
    (puthash proto-id feeds elfeed-protocol-fever-feeds)
    (cl-loop for feed across feeds do
             (let* ((feed-url (map-elt feed 'url))
                    (feed-id (elfeed-protocol-format-subfeed-id
                              proto-id feed-url))
                    (feed-title (map-elt feed 'title))
                    (feed-db (elfeed-db-get-feed feed-id)))
               (setf (elfeed-feed-url feed-db) feed-id
                     (elfeed-feed-title feed-db) feed-title)))
    (elfeed-log 'debug "elfeed-protocol-fever: found %s feeds" (length feeds))
    elfeed-protocol-fever-feeds))

(defun elfeed-protocol-fever--get-subfeed-url (host-url feed-id)
  "Get sub feed url for the fever protocol feed HOST-URL and FEED-ID."
  (let* ((url (catch 'found
                (let* ((proto-id (elfeed-protocol-fever-id host-url))
                       (feeds (gethash proto-id elfeed-protocol-fever-feeds))
                       (length (length feeds)))
                  (dotimes (i length)
                    (let* ((feed (elt feeds i))
                           (id (map-elt feed 'id))
                           (url (map-elt feed 'url)))
                      (when (eq id feed-id)
                        (throw 'found url))))))))
    (unless url
      (setq url elfeed-protocol-unknown-feed-url)
      (elfeed-log 'warn "elfeed-protocol-fever: no subfeed for feed id %s, fallback to unknown feed" feed-id))
    url))

(defun elfeed-protocol-fever--get-subfeed-id (host-url feed-url)
  "Get sub feed id the fever protocol feed HOST-URL and FEED-URL."
  (let* ((id (catch 'found
               (let* ((proto-id (elfeed-protocol-fever-id host-url))
                      (feeds (gethash proto-id elfeed-protocol-fever-feeds))
                      (length (length feeds)))
                 (dotimes (i length)
                   (let* ((feed (elt feeds i))
                          (id (map-elt feed 'id))
                          (url (map-elt feed 'url)))
                     (when (string= url feed-url)
                       (throw 'found id))))))))
    (unless id
      (elfeed-log 'error "elfeed-protocol-fever: no subfeed for feed url %s" feed-url))
    id))

(defun elfeed-protocol-fever--get-entries (host-url ids &optional mark-state update-action callback)
  "Get entries from Fever server.
HOST-URL is the host name of Fever server.  IDS is the a comma-separated string
of item ids to request.  MARK-STATE UPDATE-ACTION CALLBACK will send to
`elfeed-protocol-fever--parse-entries'."
  (let* ((url (concat (elfeed-protocol-fever--get-api-url host-url)
                      elfeed-protocol-fever-api-items))
         (data (elfeed-protocol-fever--build-data host-url))
         (split-ids (elfeed-protocol-split-ids-sub-size
                     "," ids elfeed-protocol-fever-maxsize)))
    (cl-loop for sub-ids in split-ids do
             (elfeed-log 'debug "elfeed-protocol-fever: get entries %s" sub-ids)
             (elfeed-protocol-fever-with-fetch
              (concat url "&with_ids=" sub-ids) "POST" data
              (elfeed-protocol-fever--parse-entries host-url (map-elt result 'items) mark-state update-action callback)
              (run-hook-with-args 'elfeed-update-hooks host-url)))))

(defun elfeed-protocol-fever--parse-entries (host-url items &optional mark-state update-action callback)
  "Parse the entries JSON buffer and fill results to elfeed db.
HOST-URL is the host name of Fever server.  ITEMS is the result JSON items by
http request.  If MARK-STATE is nil, then just not update :last-entry-id or
:first-entry-id values.  UPDATE-ACTION could be update, update-older or
update-star.  If CALLBACK is not nil, will call it with the result entries as
argument.  Return parsed entries."
  (if (> (hash-table-count elfeed-protocol-fever-feeds) 0)
      (let* ((proto-id (elfeed-protocol-fever-id host-url))
             (entry-mark (elfeed-protocol-fever-get-update-mark proto-id update-action))
             (min-entry-id -1)
             (max-entry-id -1)
             (first-entry-id -1)
             (unread-num 0)
             (starred-num 0)
             (begin-time (time-to-seconds))
             entries)
        (elfeed-log 'debug "elfeed-protocol-fever: %s, parsing entries, entry-mark: %d" update-action entry-mark)
        (setq entries
              (cl-loop for item across items collect
                       (pcase-let* (((map id ('url entry-url) title
                                          author ('created_on_time pub-date) ('html body)
                                          ('feed_id feed-id))
                                     item)
                                    (guid-hash (elfeed-generate-id (format "%s%s%s%s" title entry-url pub-date body)))
                                    (feed-url
                                     (if (null feed-id)
                                         ""
                                       (elfeed-protocol-fever--get-subfeed-url host-url feed-id)))
                                    (unread (eq (map-elt item 'is_read) 0))
                                    (starred (eq (map-elt item 'is_saved) 1))

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
                                              (push elfeed-protocol-fever-star-tag fixtags))
                                            fixtags))
                                    (db-entry (elfeed-entry--create
                                               :title (elfeed-cleanup title)
                                               :id full-id
                                               :feed-id (elfeed-protocol-format-subfeed-id
                                                         proto-id feed-url)
                                               :link (elfeed-cleanup entry-url)
                                               :tags tags
                                               :date (elfeed-new-date-for-entry
                                                      original-date pub-date)
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
                           (if starred (elfeed-tag-1 original elfeed-protocol-fever-star-tag)
                             (elfeed-untag-1 original elfeed-protocol-fever-star-tag)))

                         (when (> id max-entry-id)
                           (setq max-entry-id id))
                         (if (< min-entry-id 0)
                             (setq min-entry-id id)
                           (when (< id min-entry-id)
                             (setq min-entry-id id)))

                         (dolist (hook elfeed-new-entry-parse-hook)
                           (run-hook-with-args hook :fever item db-entry))
                         db-entry)))
        (elfeed-db-add entries)
        (when callback (funcall callback entries))

        ;; update last entry skip count
        (when mark-state
          (if (>= entry-mark 0)
              ;; update entry mark
              (cond
               ((eq update-action 'update)
                (elfeed-protocol-fever-set-update-mark
                 proto-id update-action (max entry-mark max-entry-id)))
               ((eq update-action 'update-older)
                (let* ((id (max 1 (- entry-mark elfeed-protocol-fever-maxsize))))
                  (elfeed-protocol-fever-set-update-mark
                   proto-id update-action id))))
            ;; init entry mark
            (setq first-entry-id (max 1 max-entry-id))
            (cond
             ((eq update-action 'update)
              (elfeed-protocol-fever-set-update-mark proto-id update-action first-entry-id)
              ;; set :first-entry-id same with :last-entry-id
              (elfeed-protocol-fever-set-update-mark proto-id 'update-older first-entry-id))
             ((eq update-action 'update-older)
              (elfeed-protocol-fever-set-update-mark proto-id update-action first-entry-id)))))

        (elfeed-log 'debug "elfeed-protocol-fever: %s, parsed %d entries(%d unread, %d starred, min-entry-id %d, max-entry-id %d) with %fs, entry-mark: %d"
                    update-action (length entries) unread-num starred-num min-entry-id max-entry-id
                    (- (time-to-seconds) begin-time)
                    (elfeed-protocol-fever-get-update-mark proto-id update-action))
        entries)
    (progn
      (elfeed-log 'error "elfeed-protocol-fever: elfeed-protocol-fever-feeds is nil, please call elfeed-protocol-fever--update-feed-list first")
      nil)))

(defun elfeed-protocol-fever--do-update (host-url action &optional arg callback)
  "Real fever protocol updating operations.
HOST-URL is the host name of Fever server, and user field authentication info is
always required so could find the related protocol feed id correctly, for
example \"https://user:pass@myhost.com\". ACTION could be init, update,
update-older and update-star. For init, will fetch unread, starred and latest
entries. For update and update-older, will fetch entries with article ids, the
ARG is the ids. For update-star, will fetch starred entries after the skipped
count, the ARG is the skip count.  If CALLBACK is not nil, will call it with the
result entries as argument."
  (elfeed-log 'debug "elfeed-protocol-fever: update entries with action %s, arg %s" action arg)
  (let* ((proto-id (elfeed-protocol-fever-id host-url))
         (url-starred (concat (elfeed-protocol-fever--get-api-url host-url)
                              elfeed-protocol-fever-api-saved-item-ids))
         (url-unread (concat (elfeed-protocol-fever--get-api-url host-url)
                             elfeed-protocol-fever-api-unread-item-ids))
         (data (elfeed-protocol-fever--build-data host-url)))
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (cond
     ;; initial sync, fetch starred and unread entries
     ((eq action 'init)
      (elfeed-protocol-fever-set-update-mark proto-id 'update -1)
      (elfeed-protocol-fever-set-update-mark proto-id 'update-older -1)
      (elfeed-protocol-clean-pending-ids proto-id)
      (elfeed-protocol-fever-with-fetch
       url-starred "POST" data
       (elfeed-protocol-fever--get-entries host-url (map-elt result 'saved_item_ids) nil 'update-star callback)
       (elfeed-protocol-fever-with-fetch
        url-unread "POST" data
        (elfeed-protocol-fever--get-entries host-url (map-elt result 'unread_item_ids) t 'update callback))))
     ;; update older or latest entries
     ((or (eq action 'update) (eq action 'update-older))
      (elfeed-protocol-fever--get-entries host-url arg t action callback))
     ;; update starred entries
     ((eq action 'update-star)
      (elfeed-protocol-fever-with-fetch
       url-starred "POST" data
       (elfeed-protocol-fever--get-entries host-url (map-elt result 'saved_item_ids) nil action callback))))))

(defun elfeed-protocol-fever-reinit (host-url)
  "Retry initial sync operation.
Will fetch starred and unread entries from Fever server.
HOST-URL is the host name of Fever server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-fever-fetch-prepare
   host-url
   (elfeed-protocol-fever--do-update host-url 'init)))

(defun elfeed-protocol-fever-update-older (host-url)
  "Fetch older entries.
HOST-URL is the host name of Fever server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((proto-id (elfeed-protocol-fever-id host-url))
         (first-entry-id (elfeed-protocol-fever-get-update-mark proto-id 'update-older))
         (ids (elfeed-protocol-generate-ids-str ","
                                                (max 1 (- first-entry-id elfeed-protocol-fever-maxsize))
                                                (max 1 (- first-entry-id 1)))))
    (elfeed-protocol-fever-fetch-prepare
     host-url
     (elfeed-protocol-fever--do-update host-url 'update-older ids))))

(defun elfeed-protocol-fever-update-star (host-url)
  "Fetch all starred entries.
HOST-URL is the host name of Fever server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-fever-fetch-prepare
   host-url
   (elfeed-protocol-fever--do-update host-url 'update-star)))

(defun elfeed-protocol-fever--write-item (host-url id state)
  "Notify item to be read/unread/starred/unstarred.
HOST-URL is the host name of Fever server.  ID is the target entry id.
STATE could be \"read\", \"unread\", \"saved\" and \"unsaved\"."
  (let* ((url (elfeed-protocol-fever--get-api-url host-url))
         (data (elfeed-protocol-fever--build-data
                host-url (format "mark=item&id=%s&as=%s" id state))))
    (elfeed-log 'debug "elfeed-protocol-fever: mark item %s as %s" id state)
    (elfeed-protocol-fever-with-fetch
      url "POST" data)))

(defun elfeed-protocol-fever-mark-read (host-url id)
  "Notify item to be read.
HOST-URL is the host name of Fever server.  ID is the target entry id."
  (elfeed-protocol-fever--write-item
   host-url id elfeed-protocol-fever-api-item-state-read))

(defun elfeed-protocol-fever-mark-unread (host-url id)
  "Notify item to be unread.
HOST-URL is the host name of Fever server.  ID is the target entry id."
  (elfeed-protocol-fever--write-item
   host-url id elfeed-protocol-fever-api-item-state-unread))

(defun elfeed-protocol-fever-mark-starred (host-url id)
  "Notify item to be starred.
HOST-URL is the host name of Fever server.  ID is the target entry id."
  (elfeed-protocol-fever--write-item
   host-url id elfeed-protocol-fever-api-item-state-saved))

(defun elfeed-protocol-fever-mark-unstarred (host-url id)
  "Notify item to be unstarred.
HOST-URL is the host name of Fever server.  ID is the target entry id."
  (elfeed-protocol-fever--write-item
   host-url id elfeed-protocol-fever-api-item-state-unsaved))

(defun elfeed-protocol-fever-sync-pending-ids (host-url)
  "Sync pending read/unread/starred/unstarred entry states to Fever server.
HOST-URL is the host name of Fever server."
  (let* ((proto-id (elfeed-protocol-fever-id host-url))
         (pending-read-ids (elfeed-protocol-get-pending-ids proto-id :pending-read))
         (pending-unread-ids (elfeed-protocol-get-pending-ids proto-id :pending-unread))
         (pending-starred-ids (elfeed-protocol-get-pending-ids proto-id :pending-starred))
         (pending-unstarred-ids (elfeed-protocol-get-pending-ids proto-id :pending-unstarred)))
    (dolist (id pending-read-ids) (elfeed-protocol-fever-mark-read host-url id))
    (dolist (id pending-unread-ids) (elfeed-protocol-fever-mark-unread host-url id))
    (dolist (id pending-starred-ids) (elfeed-protocol-fever-mark-starred host-url id))
    (dolist (id pending-unstarred-ids) (elfeed-protocol-fever-mark-unstarred host-url id))
    (elfeed-protocol-clean-pending-ids proto-id)))

(defun elfeed-protocol-fever-append-pending-id (host-url entry tag action)
  "Append read/unread/starred/unstarred ids to pending list.
HOST-URL is the host name of Fever server.  ENTRY is the target entry object.
TAG is the action tag, for example unread and `elfeed-protocol-fever-star-tag',
ACTION could be add or remove."
  (when (elfeed-protocol-fever-entry-p entry)
    (let* ((proto-id (elfeed-protocol-fever-id host-url))
           (id (elfeed-meta entry :id)))
      (cond
       ((eq action 'add)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-unread (list id))
          (elfeed-protocol-remove-pending-ids proto-id :pending-read (list id)))
         ((eq tag elfeed-protocol-fever-star-tag)
          (elfeed-protocol-append-pending-ids proto-id :pending-starred (list id))
          (elfeed-protocol-remove-pending-ids proto-id :pending-unstarred (list id)))))
       ((eq action 'remove)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-read (list id))
          (elfeed-protocol-remove-pending-ids proto-id :pending-unread (list id)))
         ((eq tag elfeed-protocol-fever-star-tag)
          (elfeed-protocol-append-pending-ids proto-id :pending-unstarred (list id))
          (elfeed-protocol-remove-pending-ids proto-id :pending-starred (list id)))))))))

(defun elfeed-protocol-fever-pre-tag (host-url entries &rest tags)
  "Sync unread and starred states before tags added.
HOST-URL is the host name of Fever server.  ENTRIES is the
target entry objects.  TAGS is the tags are adding now."
  (dolist (tag tags)
    (cl-loop for entry in entries
             unless (elfeed-tagged-p tag entry)
             do (elfeed-protocol-fever-append-pending-id host-url entry tag 'add)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-fever-sync-pending-ids host-url)))

(defun elfeed-protocol-fever-pre-untag (host-url entries &rest tags)
  "Sync unread and starred states before tags removed.
HOST-URL is the host name of Fever server.  ENTRIES is the
target entry objects.  TAGS is the tags are removing now."
  (dolist (tag tags)
    (cl-loop for entry in entries
             when (elfeed-tagged-p tag entry)
             collect (elfeed-protocol-fever-append-pending-id host-url entry tag 'remove)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-fever-sync-pending-ids host-url)))

(defun elfeed-protocol-fever-update (host-or-subfeed-url &optional callback)
  "Fever protocol updater.
HOST-OR-SUBFEED-URL could be the host name of Fever server, and user field
authentication info is always required so could find the related protocol feed
id correctly, for example \"https://user@myhost.com\".  And HOST-OR-SUBFEED-URL
also could be the sub feed url, too, for example
\"https://user@myhost.com::https://subfeed.com\".  If first time run, it will
initial sync operation, or will only fetch the updated entries since last
modified. if CALLBACK is not nil will call it with the result entries as
argument"
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((host-url (elfeed-protocol-host-url host-or-subfeed-url))
         (proto-id (elfeed-protocol-fever-id host-url))
         (last-entry-id (elfeed-protocol-fever-get-update-mark proto-id 'update))
         (ids (elfeed-protocol-generate-ids-str
               ","
               (1+ last-entry-id)
               (+ last-entry-id elfeed-protocol-fever-maxsize))))
    (elfeed-protocol-add-unknown-feed proto-id) ; add unknown feed for fallback
    (elfeed-protocol-fever-sync-pending-ids host-url)
    (elfeed-protocol-fever-fetch-prepare
     host-url
     (if (>= last-entry-id 0)
         (elfeed-protocol-fever--do-update host-url 'update ids callback)
       (elfeed-protocol-fever--do-update host-url 'init nil callback)))))

(provide 'elfeed-protocol-fever)

;;; elfeed-protocol-fever.el ends here
