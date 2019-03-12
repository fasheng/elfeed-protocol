;;; elfeed-protocol-ttrss.el --- Tiny Tiny RSS protocol for elfeed -*- lexical-binding: t; -*-

;;; Commentary:
;; Tiny Tiny RSS protocol for elfeed.

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'elfeed)
(require 'subr-x)

;;; Code:

(defcustom elfeed-protocol-ttrss-sid nil
  "Tiny Tiny RSS session id.
Will generate one if is empty or invalid."
  :group 'elfeed-protocol
  :type 'string)

(defcustom elfeed-protocol-ttrss-maxsize 200
  "Maximize entries size for each request.
As the document said, before API level 6 maximum amount of returned headlines is
capped at 60, API 6 and above sets it to 200. So set bigger than 200 just
invalid."
  :group 'elfeed-protocol
  :type 'integer)

(defcustom elfeed-protocol-ttrss-star-tag 'star
  "Default star tag for Tiny Tiny RSS entry.
If one entry set or remove the tag,
then the starred state in Tiny Tiny RSS will be synced, too."
  :group 'elfeed-protocol
  :type 'symbol)

(defcustom elfeed-protocol-ttrss-publish-tag 'publish
  "Default publish tag for Tiny Tiny RSS entry.
If one entry set or remove the tag,
then the published state in Tiny Tiny RSS will be synced, too."
  :group 'elfeed-protocol
  :type 'symbol)

(defcustom elfeed-protocol-ttrss-fetch-tags t
  "Determine if fetch entry tags from Tiny Tiny RSS."
  :group 'elfeed-protocol
  :type 'boolean)

(defvar elfeed-protocol-ttrss-feeds (make-hash-table :test 'equal)
  "Feed list from Tiny Tiny RSS, will be filled before updating operation.")

(defconst elfeed-protocol-ttrss-api-base "/api/")
(defconst elfeed-protocol-ttrss-api-max-limit 200)
(defconst elfeed-protocol-ttrss-api-status-ok 0)
(defconst elfeed-protocol-ttrss-api-status-err 1)
(defconst elfeed-protocol-ttrss-api-feed-id-starred -1)
(defconst elfeed-protocol-ttrss-api-feed-id-published -2)
(defconst elfeed-protocol-ttrss-api-feed-id-fresh -3)
(defconst elfeed-protocol-ttrss-api-feed-id-all-articles -4)
(defconst elfeed-protocol-ttrss-api-feed-id-archived 0)
(defconst elfeed-protocol-ttrss-api-view-mode-all-articles "all_articles")
(defconst elfeed-protocol-ttrss-api-view-mode-unread "unread")
(defconst elfeed-protocol-ttrss-api-view-mode-adaptive "adaptive")
(defconst elfeed-protocol-ttrss-api-view-mode-marked "marked")
(defconst elfeed-protocol-ttrss-api-view-mode-updated "updated")
(defconst elfeed-protocol-ttrss-api-update-article-field-starred 0)
(defconst elfeed-protocol-ttrss-api-update-article-field-published 1)
(defconst elfeed-protocol-ttrss-api-update-article-field-unread 2)
(defconst elfeed-protocol-ttrss-api-update-article-field-article-note 3)

(defun elfeed-protocol-ttrss-id (url)
  "Get ttrss protocol id with URL."
  (elfeed-protocol-id "ttrss" url))

(defun elfeed-protocol-ttrss--init-headers ()
  "Get http request headers for ttrss."
  `(("User-Agent" . ,elfeed-user-agent)
    ("Content-Type" . "application/json")))

(defun elfeed-protocol-ttrss-get-entry-skip (proto-id update-action)
  "Get last entry skip count for special UPDATE-ACTION.
PROTO-ID is the target protocol feed id.  UPDATE-ACTION could be update,
update-older or update-star.  If not initialized, just return -1."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))
                     (completing-read "Update action: " '(update update-older update-star))))
  (let* ((feed (elfeed-db-get-feed proto-id))
         (key-name (cond
                    ((eq update-action 'update) :last-entry-skip)
                    ((eq update-action 'update-older) :first-entry-skip)
                    ((eq update-action 'update-star) :star-entry-skip)))
         (skip (elfeed-meta feed key-name)))
    (if skip skip -1)))

(defun elfeed-protocol-ttrss-set-entry-skip (proto-id update-action skip)
  "Set last entry skip count to elfeed db.
PROTO-ID is the target protocol feed id.  UPDATE-ACTION could be update,
update-older or update-star.  SKIP the target value."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))
                     (completing-read "Update action: " '(update update-older update-star))
                     (read-string "Skip count: ")))
  (let* ((feed (elfeed-db-get-feed proto-id))
         (key-name (cond
                    ((eq update-action 'update) :last-entry-skip)
                    ((eq update-action 'update-older) :first-entry-skip)
                    ((eq update-action 'update-star) :star-entry-skip))))
    (setf (elfeed-meta feed key-name) skip)))

(defmacro elfeed-protocol-ttrss-with-fetch (host-url method data &rest body)
  "Just like `elfeed-with-fetch' but special for ttrss HTTP request.
HOST-URL is the host name of Tiny Tiny RSS server, METHOD could be
\"GET\" or \"POST\", DATA is in JSON string format.  Optional argument
BODY is the rest Lisp code after operation finished."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl) ; capture current value in closure
          (url (concat host-url elfeed-protocol-ttrss-api-base))
          (headers (elfeed-protocol-ttrss--init-headers))
          (no-auth-url (elfeed-protocol-no-auth-url url))
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
                    (elfeed-protocol-ttrss--parse-result ,@body)
                    (unless use-curl
                      (kill-buffer)))))))
     (if use-curl
         (elfeed-curl-enqueue no-auth-url cb :headers headers
                              :method ,method :data ,data)
       (let ((url-request-extra-headers headers)
             (url-request-method ,method)
             (url-request-data ,data))
         (url-retrieve no-auth-url cb () t t)))))

(defmacro elfeed-protocol-ttrss--parse-result (&rest body)
  "Parse ttrss api result JSON buffer.
Will eval rest BODY expressions at end."
  (declare (indent defun))
  `(let* ((result (json-read))
          (api-status (map-elt result 'status))
          (content (map-elt result 'content)))
     (if (eq api-status elfeed-protocol-ttrss-api-status-err)
         (elfeed-log 'error "elfeed-protocol-ttrss: %s" (map-elt content 'error))
       ,@body)))

(defmacro elfeed-protocol-ttrss-fetch-prepare (host-url &rest body)
  "Ensure logged in and feed list updated before expressions.
HOST-URL is the host name of Tiny Tiny RSS server.  And will eval rest
BODY expressions after login.  The success session id will saved to
`elfeed-protocol-ttrss-sid'"
  (declare (indent defun))
  `(if elfeed-protocol-ttrss-sid
       (let* ((data-list-isloggedin `(("op" . "isLoggedIn")
                                      ("sid" . ,elfeed-protocol-ttrss-sid))))
         (elfeed-log 'debug "elfeed-protocol-ttrss: check is logged in")
         (elfeed-protocol-ttrss-with-fetch
           ,host-url "GET" (json-encode-alist data-list-isloggedin)
           (if (eq (map-elt content 'status) ':json-false)
               (elfeed-protocol-ttrss--login
                ,host-url
                (lambda ()
                  (elfeed-protocol-ttrss--update-feed-list
                   ,host-url (lambda () ,@body))))
             (elfeed-protocol-ttrss--update-feed-list
              ,host-url (lambda () ,@body)))))
     (elfeed-protocol-ttrss--login
      ,host-url
      (lambda ()
        (elfeed-protocol-ttrss--update-feed-list
         ,host-url (lambda () ,@body))))))

(defun elfeed-protocol-ttrss--login (host-url &optional callback)
  "Login remote Tiny Tiny RSS server.
The success session id will saved to
`elfeed-protocol-ttrss-sid'.  HOST-URL is the target Tiny Tiny RSS
server url, and will call CALLBACK after login."
  (elfeed-log 'debug "elfeed-protocol-ttrss: login")
  (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
         (user (elfeed-protocol-meta-user proto-id))
         (password (elfeed-protocol-meta-password proto-id))
         (data-list `(("op" . "login")
                      ("user" . ,user)
                      ("password" . ,password)))
         (data (json-encode-alist data-list)))
    (elfeed-protocol-ttrss-with-fetch
      host-url "GET" data
      (setq elfeed-protocol-ttrss-sid (map-elt content 'session_id))
      (when callback (funcall callback)))))

(defun elfeed-protocol-ttrss--update-feed-list (host-url &optional callback)
  "Update Tiny Tiny RSS server feeds list.
HOST-URL is the host name of Tiny Tiny RSS server.  Will call CALLBACK
at end."
  (elfeed-log 'debug "elfeed-protocol-ttrss: update feed list")
  (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
         (data-list `(("op" . "getFeeds")
                      ("sid" . ,elfeed-protocol-ttrss-sid)
                      ("cat_id" . "-3")))
         (data (json-encode-alist data-list)))
    (elfeed-protocol-ttrss-with-fetch
      host-url "GET" data
      (elfeed-protocol-ttrss--parse-feeds host-url content)
      (when callback (funcall callback)))))

(defun elfeed-protocol-ttrss--parse-feeds (host-url content)
  "Parse the feeds JSON buffer and fill results to db.
HOST-URL is the host name of Tiny Tiny RSS server.  CONTENT is the
result JSON content by http request.  Return
`elfeed-protocol-ttrss-feeds'."
  (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
         (feeds content))
    (puthash proto-id feeds elfeed-protocol-ttrss-feeds)
    (cl-loop for feed across feeds do
             (let* ((feed-url (map-elt feed 'feed_url))
                    (feed-id (elfeed-protocol-format-subfeed-id
                              proto-id feed-url))
                    (feed-title (map-elt feed 'title))
                    (feed-db (elfeed-db-get-feed feed-id)))
               (setf (elfeed-feed-url feed-db) feed-id
                     (elfeed-feed-title feed-db) feed-title)))
    elfeed-protocol-ttrss-feeds))

(defun elfeed-protocol-ttrss--get-subfeed-url (host-url feed-id)
  "Get sub feed url for the ttrss protocol feed HOST-URL and FEED-ID."
  (let* ((url (catch 'found
                (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
                       (feeds (gethash proto-id elfeed-protocol-ttrss-feeds))
                       (length (length feeds)))
                  (dotimes (i length)
                    (let* ((feed (elt feeds i))
                           (id (map-elt feed 'id))
                           (url (map-elt feed 'feed_url)))
                      (when (eq id feed-id)
                        (throw 'found url))))))))
    (unless url
      (elfeed-log 'error "elfeed-protocol-ttrss: no subfeed for feed id %s" feed-id))
    url))

(defun elfeed-protocol-ttrss--get-subfeed-id (host-url feed-url)
  "Get sub feed id the ttrss protocol feed HOST-URL and FEED-URL."
  (let* ((id (catch 'found
               (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
                      (feeds (gethash proto-id elfeed-protocol-ttrss-feeds))
                      (length (length feeds)))
                 (dotimes (i length)
                   (let* ((feed (elt feeds i))
                          (id (map-elt feed 'id))
                          (url (map-elt feed 'feed_url)))
                     (when (string= url feed-url)
                       (throw 'found id))))))))
    (unless id
      (elfeed-log 'error "elfeed-protocol-ttrss: no subfeed for feed url %s" feed-url))
    id))

(defun elfeed-protocol-ttrss--get-subfeed-id-by-title (host-url feed-title)
  "Get sub feed id the ttrss protocol feed HOST-URL and FEED-TITLE."
  (let* ((id (catch 'found
               (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
                      (feeds (gethash proto-id elfeed-protocol-ttrss-feeds))
                      (length (length feeds)))
                 (dotimes (i length)
                   (let* ((feed (elt feeds i))
                          (id (map-elt feed 'id))
                          (title (map-elt feed 'title)))
                     (when (string= title feed-title)
                       (throw 'found id))))))))
    (unless id
      (elfeed-log 'error "elfeed-protocol-ttrss: no subfeed for feed title %s" feed-title))
    id))

(defun elfeed-protocol-ttrss-entry-p (entry)
  "Check if specific ENTRY is fetched from Tiny Tiny RSS."
  (let* ((proto-id (elfeed-protocol-entry-protocol-id entry))
         (proto-type (when proto-id (elfeed-protocol-type proto-id))))
    (string= proto-type "ttrss")))

(defun elfeed-protocol-ttrss--parse-entries (host-url content &optional mark-state update-action callback)
  "Parse the entries JSON buffer and fill results to elfeed db.
HOST-URL is the host name of Tiny Tiny RSS server.  CONTENT is the result JSON
content by http request.  If MARK-STATE is nil, then just not update
:last-entry-skip, :first-entry-skip or :star-entry-skip values.  UPDATE-ACTION
could be update, update-older or update-star.  If CALLBACK is not nil, will call
it with the result entries as argument.  Return parsed entries."
  (if (> (hash-table-count elfeed-protocol-ttrss-feeds) 0)
      (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
             (entry-skip (elfeed-protocol-ttrss-get-entry-skip proto-id update-action))
             (max-entry-id -1)
             (unread-num 0)
             (starred-num 0)
             (begin-time (time-to-seconds))
             (headlines content)
             entries)
        (elfeed-log 'debug "elfeed-protocol-ttrss: %s, parsing entries, entry-skip: %d" update-action entry-skip)
        (setq entries
              (cl-loop for headline across headlines
                       when
                       (pcase-let* (((map ('feed_id feed-id)
                                          ('feed_title feed-title))
                                     headline)
                                    (feed-id
                                     (if (null feed-id)
                                         (elfeed-protocol-ttrss--get-subfeed-id-by-title host-url feed-title)
                                       (if (stringp feed-id) (string-to-number feed-id) feed-id))))
                         feed-id)
                       collect
                       (pcase-let* (((map id ('link entry-url) title
                                          author ('updated pub-date) ('content body)
                                          ('tags ttrss-tags) attachments
                                          ('feed_id feed-id)
                                          ('feed_title feed-title)
                                          )
                                     headline)
                                    (guid-hash (elfeed-generate-id body))
                                    (feed-id
                                     (if (null feed-id)
                                         (elfeed-protocol-ttrss--get-subfeed-id-by-title host-url feed-title)
                                       (if (stringp feed-id) (string-to-number feed-id) feed-id)))
                                    (feed-url
                                     (if (null feed-id)
                                         ""
                                       (elfeed-protocol-ttrss--get-subfeed-url host-url feed-id)))
                                    (unread (not (eq (map-elt headline 'unread)
                                                     ':json-false)))
                                    (starred (not (eq (map-elt headline 'marked)
                                                      ':json-false)))
                                    (published (not (eq (map-elt headline 'published)
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
                                              (push elfeed-protocol-ttrss-star-tag fixtags))
                                            (when published
                                              (push elfeed-protocol-ttrss-publish-tag fixtags))
                                            (when elfeed-protocol-ttrss-fetch-tags
                                              (dotimes (i (length ttrss-tags))
                                                (let ((tag (elt ttrss-tags i)))
                                                  (unless (string-empty-p tag)
                                                    (push (intern tag) fixtags)))))
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
                                               ;; :enclosures nil ;; ;;TODO: fill attachments
                                               :content body
                                               :content-type 'html
                                               :meta `(,@(when author
                                                           (list :author author))
                                                       ,@(list :protocol-id proto-id
                                                               :id id
                                                               :guid-hash guid-hash
                                                               :feed-id feed-id)))))
                         (when unread (setq unread-num (+ unread-num 1)))
                         (when starred (setq starred-num (+ starred-num 1)))

                         ;; force override unread and star tags without repeat sync operation
                         (when original
                           (if unread (elfeed-tag-1 original 'unread)
                             (elfeed-untag-1 original 'unread))
                           (if starred (elfeed-tag-1 original elfeed-protocol-ttrss-star-tag)
                             (elfeed-untag-1 original elfeed-protocol-ttrss-star-tag))
                           (if published (elfeed-tag-1 original elfeed-protocol-ttrss-publish-tag)
                             (elfeed-untag-1 original elfeed-protocol-ttrss-publish-tag)))

                         (when (> id max-entry-id)
                           (setq max-entry-id id))

                         (dolist (hook elfeed-new-entry-parse-hook)
                           (run-hook-with-args hook :ttrss headline db-entry))
                         db-entry)))
        (elfeed-db-add entries)
        (when callback (funcall callback entries))

        ;; update last entry skip count
        (when mark-state
          (if (>= entry-skip 0)
              (cond
               ((or (eq update-action 'update) (eq update-action 'update-star))
                (elfeed-protocol-ttrss-set-entry-skip
                 proto-id update-action (+ entry-skip (length entries))))
               ((eq update-action 'update-older)
                (let* ((skip (max 0 (- entry-skip (length entries)))))
                  (elfeed-protocol-ttrss-set-entry-skip
                   proto-id update-action skip))))
            (cond
             ((eq update-action 'update)
              (elfeed-protocol-ttrss-set-entry-skip proto-id update-action max-entry-id)
              ;; set :first-entry-skip same with :last-entry-skip
              (elfeed-protocol-ttrss-set-entry-skip proto-id 'update-older max-entry-id))
             ((eq update-action 'update-older)
              (elfeed-protocol-ttrss-set-entry-skip proto-id update-action max-entry-id))
             ((eq update-action 'update-star)
              (elfeed-protocol-ttrss-set-entry-skip proto-id update-action (length entries))))))

        (elfeed-log 'debug "elfeed-protocol-ttrss: %s, parsed %d entries(%d unread, %d starred) with %fs, entry-skip: %d"
                    update-action (length entries) unread-num starred-num
                    (- (time-to-seconds) begin-time)
                    (elfeed-protocol-ttrss-get-entry-skip proto-id update-action))
        entries)
    (progn
      (elfeed-log 'error "elfeed-protocol-ttrss: elfeed-protocol-ttrss-feeds is nil, please call elfeed-protocol-ttrss--update-feed-list first")
      nil)))

(defun elfeed-protocol-ttrss--do-update (host-url action &optional arg callback)
  "Real ttrss protocol updating operations.
HOST-URL is the host name of Tiny Tiny RSS server, and user field authentication
info is always required so could find the related protocol feed id correctly,
for example \"https://user:pass@myhost.com\". ACTION could be init, update,
update-older, update-star and update-subfeed. For init, will fetch unread,
starred and latest entries. For update, update-older and update-star, will fetch
entries after the skipped count, the ARG is the skip count. And for
update-subfeed, will fetch latest entries for special feed, the ARG is the feed
id.  If CALLBACK is not nil, will call it with the result entries as argument."
  (elfeed-log 'debug "elfeed-protocol-ttrss: update entries with action %s, arg %s" action arg)
  (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
         (data-list-base `(("op" . "getHeadlines")
                           ("sid" . ,elfeed-protocol-ttrss-sid)
                           ("show_content" . "1")
                           ("include_attachments" . "1")
                           ("order_by" . "date_reverse")
                           ("limit" . ,elfeed-protocol-ttrss-maxsize)))
         (data-list-starred (append data-list-base
                                    `(("feed_id" .
                                       ,elfeed-protocol-ttrss-api-feed-id-starred)
                                      ("view_mode" .
                                       ,elfeed-protocol-ttrss-api-view-mode-all-articles))))
         (data-list-unread (append data-list-base
                                   `(("feed_id" .
                                      ,elfeed-protocol-ttrss-api-feed-id-all-articles)
                                     ("view_mode" .
                                      ,elfeed-protocol-ttrss-api-view-mode-unread))))
         (data-list-all (append data-list-base
                                `(("feed_id" .
                                   ,elfeed-protocol-ttrss-api-feed-id-all-articles)
                                  ("view_mode" .
                                   ,elfeed-protocol-ttrss-api-view-mode-all-articles)))))
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (cond
     ;; initial sync, fetch starred, unread and latest entries
     ((eq action 'init)
      (elfeed-protocol-ttrss-set-entry-skip proto-id 'update -1)
      (elfeed-protocol-ttrss-set-entry-skip proto-id 'update-older -1)
      (elfeed-protocol-ttrss-set-entry-skip proto-id 'update-star -1)
      (elfeed-protocol-ttrss-with-fetch
        host-url "GET" (json-encode-alist data-list-starred)
        (elfeed-protocol-ttrss--parse-entries host-url content t 'update-star callback)
        (elfeed-protocol-ttrss-with-fetch
          host-url "GET" (json-encode-alist data-list-unread)
          (elfeed-protocol-ttrss--parse-entries host-url content t 'update callback)
          (run-hook-with-args 'elfeed-update-hooks host-url))))
     ;; update entries
     ((or (eq action 'update) (eq action 'update-older) (eq action 'update-star))
      (let* ((feed-id (if (eq action 'update-star)
                          elfeed-protocol-ttrss-api-feed-id-starred
                        elfeed-protocol-ttrss-api-feed-id-all-articles))
             (data-list-skip (append data-list-base
                                     `(("feed_id" . ,feed-id)
                                       ("view_mode" .
                                        ,elfeed-protocol-ttrss-api-view-mode-all-articles)
                                       ("skip" . ,arg)))))
             (elfeed-protocol-ttrss-with-fetch
               host-url "GET" (json-encode-alist data-list-skip)
               (elfeed-protocol-ttrss--parse-entries host-url content t action callback)
               (run-hook-with-args 'elfeed-update-hooks host-url))))
     ;; update entries for special sub feed
     ((eq action 'update-subfeed)
      (let* ((feed-id arg)
             (data-list-feed (append data-list-base
                                     `(("feed_id" . ,feed-id)
                                       ("view_mode" .
                                        ,elfeed-protocol-ttrss-api-view-mode-all-articles)))))
        (elfeed-protocol-ttrss-with-fetch
          host-url "GET" (json-encode-alist data-list-feed)
          (elfeed-protocol-ttrss--parse-entries host-url content nil 'update callback)
          (run-hook-with-args 'elfeed-update-hooks host-url)))))))

(defun elfeed-protocol-ttrss-reinit (host-url)
  "Retry initial sync operation.
Will fetch unread, starred and latest entries from Tiny Tiny RSS.
HOST-URL is the host name of Tiny Tiny RSS server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-ttrss-fetch-prepare
    host-url
    (elfeed-protocol-ttrss--do-update host-url 'init)))

(defun elfeed-protocol-ttrss-update-older (host-url)
  "Fetch older entries.
HOST-URL is the host name of Tiny Tiny RSS server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
         (skip (elfeed-protocol-ttrss-get-entry-skip proto-id 'update-older)))
    (setq skip (max 0 (- skip (min elfeed-protocol-ttrss-api-max-limit
                                   elfeed-protocol-ttrss-maxsize))))
    (elfeed-protocol-ttrss-fetch-prepare
      host-url
      (elfeed-protocol-ttrss--do-update host-url 'update-older skip))))

(defun elfeed-protocol-ttrss-update-star (host-url)
  "Fetch starred entries.
For Tiny Tiny RSS only allow fetch Maximize 200 entries each time, so if your
own much more starred entries, just run this function to fetch them all.
HOST-URL is the host name of Tiny Tiny RSS server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
         (skip (elfeed-protocol-ttrss-get-entry-skip proto-id 'update-star)))
    (elfeed-protocol-ttrss-fetch-prepare
      host-url
      (elfeed-protocol-ttrss--do-update host-url 'update-star skip))))

(defun elfeed-protocol-ttrss-join-ids-to-str (separate &rest ids)
  "Convert article ids to string format.
SEPARATE is the string to be insert between each id, IDS is the target
id array."
  (cl-labels ((recurser
               (ids)
               (cond ((> (length ids) 1)
                      (append (list (number-to-string (car ids)) separate)
                              (recurser (cdr ids))))
                     (t (cons (number-to-string (car ids)) nil)))))
    (apply 'concat (recurser ids))))


(defun elfeed-protocol-ttrss--update-article (host-url entries field mode)
  "Notify multiple entries to be read/unread/starred/unstarred.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the
target entry objects.  FIELD could be 0, 1, 2, 3 which means starred,
published, unread, and article note.  MODE could be 0, 1, 2 which
means set to false, set to true and toggle."
  (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
         (ids (cl-loop for entry in entries collect
                       (when (elfeed-protocol-ttrss-entry-p entry)
                         (elfeed-meta entry :id))))
         (data-list `(("op" . "updateArticle")
                      ("sid" . ,elfeed-protocol-ttrss-sid)
                      ("article_ids" .
                       ,(apply #'elfeed-protocol-ttrss-join-ids-to-str "," ids))
                      ("field" . ,field)
                      ("mode" . ,mode)))
         (data (json-encode-alist data-list)))
    (when ids
      (elfeed-protocol-ttrss-with-fetch
        host-url "POST" data))))

(defun elfeed-protocol-ttrss-mark-read (host-url entries)
  "Notify multiple entries to be read.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the target entry objects."
  (elfeed-protocol-ttrss--update-article
   host-url entries elfeed-protocol-ttrss-api-update-article-field-unread 0))

(defun elfeed-protocol-ttrss-mark-unread (host-url entries)
  "Notify multiple entries to be unread.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the target entry objects."
  (elfeed-protocol-ttrss--update-article
   host-url entries elfeed-protocol-ttrss-api-update-article-field-unread 1))

(defun elfeed-protocol-ttrss-mark-star (host-url entries)
  "Notify multiple entries to be starred.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the target entry objects."
  (elfeed-protocol-ttrss--update-article
   host-url entries elfeed-protocol-ttrss-api-update-article-field-starred 1))

(defun elfeed-protocol-ttrss-mark-unstar (host-url entries)
  "Notify multiple entries to be unstarred.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the target entry objects."
  (elfeed-protocol-ttrss--update-article
   host-url entries elfeed-protocol-ttrss-api-update-article-field-starred 0))

(defun elfeed-protocol-ttrss-mark-publish (host-url entries)
  "Notify multiple entries to be published.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the target entry objects."
  (elfeed-protocol-ttrss--update-article
   host-url entries elfeed-protocol-ttrss-api-update-article-field-published 1))

(defun elfeed-protocol-ttrss-mark-unpublish (host-url entries)
  "Notify multiple entries to be unpublished.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the target entry objects."
  (elfeed-protocol-ttrss--update-article
   host-url entries elfeed-protocol-ttrss-api-update-article-field-published 0))

(defun elfeed-protocol-ttrss-sync-tag (host-url entries tag action)
  "Sync unread starred and published tag states to Tiny Tiny RSS server.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the
target entry objects.  TAG is the action tag, for example unread,
`elfeed-protocol-ttrss-star-tag' and
`elfeed-protocol-ttrss-publish-tag', ACTION could be add or remove."
  (when entries
    (cond
     ((eq action 'add)
      (cond
       ((eq tag 'unread) (elfeed-protocol-ttrss-mark-unread host-url entries))
       ((eq tag elfeed-protocol-ttrss-star-tag)
        (elfeed-protocol-ttrss-mark-star host-url entries))
       ((eq tag elfeed-protocol-ttrss-publish-tag)
        (elfeed-protocol-ttrss-mark-publish host-url entries))))
     ((eq action 'remove)
      (cond
       ((eq tag 'unread) (elfeed-protocol-ttrss-mark-read host-url entries))
       ((eq tag elfeed-protocol-ttrss-star-tag)
        (elfeed-protocol-ttrss-mark-unstar host-url entries))
       ((eq tag elfeed-protocol-ttrss-publish-tag)
        (elfeed-protocol-ttrss-mark-unpublish host-url entries)))))))

(defun elfeed-protocol-ttrss-pre-tag (host-url entries &rest tags)
  "Sync unread, starred and published states before tags added.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the
target entry objects.  TAGS is the tags are adding now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-ttrss-sync-tag host-url entries-modified tag 'add))))

(defun elfeed-protocol-ttrss-pre-untag (host-url entries &rest tags)
  "Sync unread, starred and published states before tags removed.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the
target entry objects.  TAGS is the tags are removing now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-ttrss-sync-tag host-url entries-modified tag 'remove))))

(defun elfeed-protocol-ttrss-update-subfeed (host-url feed-url &optional callback)
  "Update entries under special sub feed in Tiny Tiny RSS.
HOST-URL is the host name of Tiny Tiny RSS server, FEED-URL is the
target sub feed url, if CALLBACK is not nil will call it with the
result entries as argument."
  (interactive)
  (let* ((feed-id (elfeed-protocol-ttrss--get-subfeed-id host-url feed-url)))
    (when feed-id
      (elfeed-protocol-ttrss-fetch-prepare
        host-url
        (elfeed-protocol-ttrss--do-update host-url 'update-subfeed feed-id callback)))))

(defun elfeed-protocol-ttrss-update (host-or-subfeed-url &optional callback)
  "Tiny Tiny RSS protocol updater.
HOST-OR-SUBFEED-URL could be the host name of Tiny Tiny RSS server,
and user field authentication info is always required so could find
the related protocol feed id correctly, for example
\"https://user@myhost.com\".  And HOST-OR-SUBFEED-URL also could be the
sub feed url, too, for example
\"https://user@myhost.com::https://subfeed.com\".  If first time run,
it will initial sync operation, or will only fetch the updated entries
since last modified. if CALLBACK is not nil will call it with the
result entries as argument"
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((host-url (elfeed-protocol-host-url host-or-subfeed-url))
         (feed-url (elfeed-protocol-subfeed-url host-or-subfeed-url)))
    (if feed-url (elfeed-protocol-ttrss-update-subfeed host-url feed-url callback)
      (let* ((proto-id (elfeed-protocol-ttrss-id host-url))
             (last-entry-skip (elfeed-protocol-ttrss-get-entry-skip proto-id 'update)))
        (elfeed-protocol-ttrss-fetch-prepare
          host-url
          (if (>= last-entry-skip 0)
              (elfeed-protocol-ttrss--do-update host-url 'update last-entry-skip callback)
            (elfeed-protocol-ttrss--do-update host-url 'init nil callback)))))))

(provide 'elfeed-protocol-ttrss)

;;; elfeed-protocol-ttrss.el ends here
