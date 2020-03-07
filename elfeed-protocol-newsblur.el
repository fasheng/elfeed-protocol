;;; elfeed-protocol-newsblur.el --- NewsBlur protocol for elfeed -*- lexical-binding: t; -*-

;;; Commentary:
;; NewsBlur protocol for elfeed.

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'subr-x)
(require 'elfeed)
(require 'elfeed-protocol-common)

;;; Code:

(defcustom elfeed-protocol-newsblur-maxpages 20
  "Maximize page size for each request."
  :group 'elfeed-protocol
  :type 'integer)

(defcustom elfeed-protocol-newsblur-star-tag 'star
  "Default star tag for NewsBlur entry.
If one entry set or remove the tag,
then the starred state in NewsBlur will be synced, too."
  :group 'elfeed-protocol
  :type 'symbol)

(defcustom elfeed-protocol-newsblur-fetch-tags t
  "Determine if fetch entry tags from NewsBlur."
  :group 'elfeed-protocol
  :type 'boolean)

(defvar elfeed-protocol-newsblur-feeds (make-hash-table :test 'equal)
  "Feed list from NewsBlur, will be filled before updating operation.")

(defconst elfeed-protocol-newsblur-api-login "/api/login")
(defconst elfeed-protocol-newsblur-api-reader-feeds "/reader/feeds")
(defconst elfeed-protocol-newsblur-api-reader-river-stories "/reader/river_stories?include_story_content=true&read_filter=unread&order=newest&include_hidden=true&page=%s")
(defconst elfeed-protocol-newsblur-api-reader-feed "/reader/feed/%s??include_story_content=true&read_filter=all&order=newest&include_hidden=true&page=%s")
(defconst elfeed-protocol-newsblur-api-reader-starred-stories "/reader/starred_stories?page=%s")
(defconst elfeed-protocol-newsblur-api-reader-mark-story-read "/reader/mark_story_hashes_as_read")
(defconst elfeed-protocol-newsblur-api-reader-mark-story-unread "/reader/mark_story_hash_as_unread")
(defconst elfeed-protocol-newsblur-api-reader-mark-story-starred "/reader/mark_story_hash_as_starred")
(defconst elfeed-protocol-newsblur-api-reader-mark-story-unstarred "/reader/mark_story_hash_as_unstarred")

(defun elfeed-protocol-newsblur-id (url)
  "Get newsblur protocol id with URL."
  (elfeed-protocol-id "newsblur" url))

(defmacro elfeed-protocol-newsblur-with-fetch (url method data &rest body)
  "Just like `elfeed-with-fetch' but special for NewsBlur HTTP request.
URL is the target url to request, METHOD could be \"GET\" or \"POST\",
DATA is in JSON string format.  Optional argument BODY is the rest
Lisp code after operation finished."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl) ; capture current value in closure
          (headers `(("User-Agent" . ,elfeed-user-agent)))
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
                      (elfeed-log 'debug "elfeed-protocol-newsblur: %s" (buffer-string)))
                    (elfeed-protocol-newsblur--parse-result ,@body)
                    (unless use-curl
                      (kill-buffer)))))))
     (if use-curl
         (elfeed-curl-enqueue no-auth-url cb :headers headers
                              :method ,method :data ,data)
       (let ((url-request-extra-headers headers)
             (url-request-method ,method)
             (url-request-data ,data))
         (url-retrieve no-auth-url cb () t t)))))

(defmacro elfeed-protocol-newsblur--parse-result (&rest body)
  "Parse newsblur api result JSON buffer.
Will eval rest BODY expressions at end."
  (declare (indent defun))
  `(let* ((result (json-read))
          (errors (map-elt result 'errors)))
     (if errors
         (elfeed-log 'error "elfeed-protocol-newsblur: %s" errors)
       ,@body)))

(defmacro elfeed-protocol-newsblur-fetch-prepare (host-url &rest body)
  "Ensure logged in and feed list updated before expressions.
HOST-URL is the host name of NewsBlur server.  And will eval rest
BODY expressions after login."
  (declare (indent defun))
  `(elfeed-protocol-newsblur--update-feed-list
    ,host-url
    (lambda (need-login)
      (if need-login
          (elfeed-protocol-newsblur--login
           ,host-url
           (lambda ()
             (elfeed-protocol-newsblur--update-feed-list
              ,host-url
              (lambda (need-login)
                (if need-login
                    (elfeed-log 'error "elfeed-protocol-newsblur: login failed, ensure setup cookie for curl like this (setq elfeed-curl-extra-arguments '(\"-c\" \"/tmp/newsblur-cookie\" \"-b\" \"/tmp/newsblur-cookie\"))")
                  ,@body)))))
        ,@body))))

(defun elfeed-protocol-newsblur--login (host-url &optional callback)
  "Login remote NewsBlur server.
HOST-URL is the host name of NewsBlur server.  Will call CALLBACK
after login."
  (elfeed-log 'debug "elfeed-protocol-newsblur: login")
  (let* ((proto-id (elfeed-protocol-newsblur-id host-url))
         (user (elfeed-protocol-meta-user proto-id))
         (password (elfeed-protocol-meta-password proto-id))
         (url (concat host-url elfeed-protocol-newsblur-api-login))
         (data (format "username=%s&password=%s" user password)))
    (elfeed-protocol-newsblur-with-fetch
      url "POST" data
      (when callback (funcall callback)))))

(defun elfeed-protocol-newsblur--update-feed-list (host-url &optional callback)
  "Update Tiny Tiny RSS server feeds list.
HOST-URL is the host name of NewsBlur server.  Will call CALLBACK at
end with argument NEED-LOGIN."
  (elfeed-log 'debug "elfeed-protocol-newsblur: update feed list")
  (let* ((url (concat host-url elfeed-protocol-newsblur-api-reader-feeds))
         (data "include_favicons=false"))
    (elfeed-protocol-newsblur-with-fetch
      url "GET" data
      (let* ((authenticated (map-elt result 'authenticated))
             (need-login (eq authenticated ':json-false)))
        (unless need-login
          (elfeed-protocol-newsblur--parse-feeds host-url result))
        (when callback (funcall callback need-login))))))

(defun elfeed-protocol-newsblur--parse-feeds (host-url content)
  "Parse the feeds JSON buffer and fill results to db.
HOST-URL is the host name of NewsBlur server.  CONTENT is the
result JSON content by http request.  Return
`elfeed-protocol-newsblur-feeds'."
  (let* ((proto-id (elfeed-protocol-newsblur-id host-url))
         (feeds (map-elt content 'feeds)))
    (puthash proto-id feeds elfeed-protocol-newsblur-feeds)
    (cl-loop for feed in feeds do
             (let* ((feed-url (map-elt feed 'feed_link))
                    (feed-id (elfeed-protocol-format-subfeed-id
                              proto-id feed-url))
                    (feed-title (map-elt feed 'feed_title))
                    (feed-db (elfeed-db-get-feed feed-id)))
               (setf (elfeed-feed-url feed-db) feed-id
                     (elfeed-feed-title feed-db) feed-title)))
    (elfeed-log 'debug "elfeed-protocol-newsblur: found %s feeds" (length feeds))
    elfeed-protocol-newsblur-feeds))

(defun elfeed-protocol-newsblur--get-subfeed-url (host-url feed-id)
  "Get sub feed url for the newsblur protocol feed HOST-URL and FEED-ID."
  (let* ((url (catch 'found
                (let* ((proto-id (elfeed-protocol-newsblur-id host-url))
                       (feeds (gethash proto-id elfeed-protocol-newsblur-feeds))
                       (length (length feeds)))
                  (dotimes (i length)
                    (let* ((feed (elt feeds i))
                           (id (map-elt feed 'id))
                           (url (map-elt feed 'feed_link)))
                      (when (eq id feed-id)
                        (throw 'found url))))))))
    (unless url
      (setq url elfeed-protocol-unknown-feed-url)
      (elfeed-log 'warn "elfeed-protocol-newsblur: no subfeed for feed id %s, fallback to unknown feed" feed-id))
    url))

(defun elfeed-protocol-newsblur--get-subfeed-id (host-url feed-url)
  "Get sub feed id the newsblur protocol feed HOST-URL and FEED-URL."
  (let* ((id (catch 'found
               (let* ((proto-id (elfeed-protocol-newsblur-id host-url))
                      (feeds (gethash proto-id elfeed-protocol-newsblur-feeds))
                      (length (length feeds)))
                 (dotimes (i length)
                   (let* ((feed (elt feeds i))
                          (id (map-elt feed 'id))
                          (url (map-elt feed 'feed_link)))
                     (when (string= url feed-url)
                       (throw 'found id))))))))
    (unless id
      (elfeed-log 'error "elfeed-protocol-newsblur: no subfeed for feed url %s" feed-url))
    id))

(defun elfeed-protocol-newsblur-entry-p (entry)
  "Check if specific ENTRY is fetched from NewsBlur."
  (let* ((proto-id (elfeed-protocol-entry-protocol-id entry))
         (proto-type (when proto-id (elfeed-protocol-type proto-id))))
    (string= proto-type "newsblur")))

(defun elfeed-protocol-newsblur--parse-entries (host-url content &optional mark-state callback)
  "Parse the entries JSON buffer and fill results to elfeed db.
HOST-URL is the host name of NewsBlur server.  CONTENT is the result
JSON content by http request.  If MARK-STATE is nil, then just not
update :last-modifed value.  If CALLBACK is not nil, will call it with
the result entries as argument.  Return parsed entries."
  (if (> (hash-table-count elfeed-protocol-newsblur-feeds) 0)
      (let* ((proto-id (elfeed-protocol-newsblur-id host-url))
             (begin-time (time-to-seconds))
             (max-last-modified (elfeed-protocol-get-last-modified proto-id))
             (headlines (map-elt content 'stories))
             entries)
        (elfeed-log 'debug "elfeed-protocol-newsblur: parsing entries, last-modified: %d"
                    (elfeed-protocol-get-last-modified proto-id))
        (setq entries
              (cl-loop for headline across headlines collect
                       (pcase-let* (((map ('story_hash id) ('story_permalink entry-url)
                                          ('story_title title) ('story_authors author)
                                          ('story_content body) ('story_tags newsblur-tags)
                                          ('story_feed_id feed-id) ('guid_hash guid-hash)
                                          ;; ('image_urls image-urls)
                                          )
                                     headline)
                                    (pub-date (string-to-number (map-elt headline 'story_timestamp)))
                                    (feed-url (elfeed-protocol-newsblur--get-subfeed-url host-url feed-id))
                                    (unread (eq (map-elt headline 'read_status) 0))
                                    (starred (and (map-elt headline 'starred)
                                                  (not (eq (map-elt headline 'starred)
                                                           ':json-false))))
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
                                              (push elfeed-protocol-newsblur-star-tag fixtags))
                                            (when elfeed-protocol-newsblur-fetch-tags
                                              (dotimes (i (length newsblur-tags))
                                                (let ((tag (elt newsblur-tags i)))
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
                                               ;; :enclosures nil ;TODO: fill image-urls mime
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
                           (if starred (elfeed-tag-1 original elfeed-protocol-newsblur-star-tag)
                             (elfeed-untag-1 original elfeed-protocol-newsblur-star-tag)))

                         ;; calculate the last modified time
                         (when (> pub-date max-last-modified)
                           (setq max-last-modified pub-date))

                         (dolist (hook elfeed-new-entry-parse-hook)
                           (run-hook-with-args hook :newsblur headline db-entry))
                         db-entry)))
        (elfeed-db-add entries)
        (when callback (funcall callback entries))

        ;; update last modified time
        (when (and mark-state (> max-last-modified 0))
          (elfeed-protocol-set-last-modified proto-id max-last-modified))

        (elfeed-log 'debug "elfeed-protocol-newsblur: parsed %s entries with %fs, last-modified: %d"
                    (length entries) (- (time-to-seconds) begin-time)
                    (elfeed-protocol-get-last-modified proto-id))
        entries)
    (progn
      (elfeed-log 'error "elfeed-protocol-newsblur: elfeed-protocol-newsblur-feeds is nil, please call elfeed-protocol-newsblur--update-feed-list first")
      nil)))

(defun elfeed-protocol-newsblur--do-update (host-url action &optional arg callback)
  "Real newsblur protocol updating operations.
HOST-URL is the host name of NewsBlur server, and user field
authentication info is always required so could find the related
protocol feed id correctly, for example
\"https://user:pass@myhost.com\". ACTION could be init, update and
update-subfeed. For init, will fetch starred and recent pages
entries. For update, will fetch recent pages entries, the ARG is the
page number. And for update-subfeed, will fetch entries for special
feed, the ARG is the feed id.  If CALLBACK is not nil, will call it
with the result entries as argument."
  (elfeed-log 'debug "elfeed-protocol-newsblur: update entries with action %s, arg %s" action arg)
  (let* ((proto-id (elfeed-protocol-newsblur-id host-url)))
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (cond
     ;; init
     ((eq action 'init)
      (elfeed-protocol-set-last-modified proto-id 0)
      (elfeed-protocol-clean-pending-ids proto-id)
      (dotimes (i elfeed-protocol-newsblur-maxpages)
        (elfeed-protocol-newsblur-with-fetch
          (concat host-url (format
                            elfeed-protocol-newsblur-api-reader-starred-stories (1+ i)))
          "GET" nil
          (elfeed-protocol-newsblur--parse-entries host-url result t callback)
          (run-hook-with-args 'elfeed-update-hooks host-url))
        (elfeed-protocol-newsblur-with-fetch
          (concat host-url (format
                            elfeed-protocol-newsblur-api-reader-river-stories (1+ i)))
          "GET" nil
          (elfeed-protocol-newsblur--parse-entries host-url result t callback)
          (run-hook-with-args 'elfeed-update-hooks host-url))))
     ;; update entries
     ((eq action 'update)
      (dotimes (i elfeed-protocol-newsblur-maxpages)
        (elfeed-protocol-newsblur-with-fetch
          (concat host-url (format
                            elfeed-protocol-newsblur-api-reader-river-stories (1+ i)))
          "GET" nil
          (elfeed-protocol-newsblur--parse-entries host-url result t callback)
          (run-hook-with-args 'elfeed-update-hooks host-url))))
     ;; update entries for special sub feed
     ((eq action 'update-subfeed)
      (dotimes (i elfeed-protocol-newsblur-maxpages)
        (elfeed-protocol-newsblur-with-fetch
          (concat host-url (format elfeed-protocol-newsblur-api-reader-feed arg (1+ i)))
          "GET" nil
          (elfeed-protocol-newsblur--parse-entries host-url result t callback)
          (run-hook-with-args 'elfeed-update-hooks host-url)))))))

(defun elfeed-protocol-newsblur-reinit (host-url)
  "Retry initial sync operation.
Will fetch starred and recent entries from NewsBlur.  HOST-URL is the
host name of NewsBlur server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-newsblur-fetch-prepare
        host-url
        (elfeed-protocol-newsblur--do-update host-url 'init nil)))

(defun elfeed-protocol-newsblur-build-story-hashes-args (entries)
  "Build story hashes arguments for ENTRIES."
  (let* (args)
    (dolist (entry entries)
      (let* ((arg (format "story_hash=%s" (elfeed-meta entry :id))))
        (if args
          (setq args (concat args "&" arg))
        (setq args arg))))
    args))

(defun elfeed-protocol-newsblur-update-entry-state (host-url sub-url id)
  "Notify entries to be read/unread/starred/unstarred.
HOST-URL is the host name of NewsBlur server.  SUB-URL is the endpoint
url.  ID is the target entry id."
  (elfeed-protocol-newsblur-with-fetch
    (concat host-url sub-url)
    "POST"
    (format "story_hash=%s" id)))

(defun elfeed-protocol-newsblur-mark-read (host-url id)
  "Notify multiple entries to be read.
HOST-URL is the host name of NewsBlur server.  ID is the target entry id."
  (elfeed-log 'debug "elfeed-protocol-newsblur: mark read, id: %s" id)
  (elfeed-protocol-newsblur-update-entry-state
   host-url elfeed-protocol-newsblur-api-reader-mark-story-read id))

(defun elfeed-protocol-newsblur-mark-unread (host-url id)
  "Notify multiple entries to be unread.
HOST-URL is the host name of NewsBlur server.  ID is the target entry id."
  (elfeed-log 'debug "elfeed-protocol-newsblur: mark unread, id: %s" id)
  (elfeed-protocol-newsblur-update-entry-state
   host-url elfeed-protocol-newsblur-api-reader-mark-story-unread id))

(defun elfeed-protocol-newsblur-mark-starred (host-url id)
  "Notify multiple entries to be starred.
HOST-URL is the host name of NewsBlur server.  ID is the target entry id."
  (elfeed-log 'debug "elfeed-protocol-newsblur: mark starred, id: %s" id)
  (elfeed-protocol-newsblur-update-entry-state
   host-url elfeed-protocol-newsblur-api-reader-mark-story-starred id))

(defun elfeed-protocol-newsblur-mark-unstarred (host-url id)
  "Notify multiple entries to be unstarred.
HOST-URL is the host name of NewsBlur server.  ID is the target entry id."
  (elfeed-log 'debug "elfeed-protocol-newsblur: mark unstarred, id: %s" id)
  (elfeed-protocol-newsblur-update-entry-state
   host-url elfeed-protocol-newsblur-api-reader-mark-story-unstarred id))

(defun elfeed-protocol-newsblur-sync-pending-ids (host-url)
  "Sync pending read/unread/starred/unstarred entry states to NewsBlur server.
HOST-URL is the host name of NewsBlur server."
  (let* ((proto-id (elfeed-protocol-newsblur-id host-url))
         (pending-read-ids (elfeed-protocol-get-pending-ids proto-id :pending-read))
         (pending-unread-ids (elfeed-protocol-get-pending-ids proto-id :pending-unread))
         (pending-starred-ids (elfeed-protocol-get-pending-ids proto-id :pending-starred))
         (pending-unstarred-ids (elfeed-protocol-get-pending-ids proto-id :pending-unstarred)))
    (dolist (id pending-read-ids) (elfeed-protocol-newsblur-mark-read host-url id))
    (dolist (id pending-unread-ids) (elfeed-protocol-newsblur-mark-unread host-url id))
    (dolist (id pending-starred-ids) (elfeed-protocol-newsblur-mark-starred host-url id))
    (dolist (id pending-unstarred-ids) (elfeed-protocol-newsblur-mark-unstarred host-url id))
    (elfeed-protocol-clean-pending-ids proto-id)))

(defun elfeed-protocol-newsblur-append-pending-id (host-url entry tag action)
  "Sync unread starred and published tag states to NewsBlur server.
HOST-URL is the the host name of NewsBlur server.  ENTRY is the target entry
object.  TAG is the action tag, for example unread, and
`elfeed-protocol-newsblur-star-tag', ACTION could be add or remove."
  (when (elfeed-protocol-newsblur-entry-p entry)
    (let* ((proto-id (elfeed-protocol-newsblur-id host-url))
           (id (elfeed-meta entry :id)))
      (cond
       ((eq action 'add)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-unread (list id))
          (elfeed-protocol-remove-pending-ids proto-id :pending-read (list id)))
         ((eq tag elfeed-protocol-newsblur-star-tag)
          (elfeed-protocol-append-pending-ids proto-id :pending-starred (list id))
          (elfeed-protocol-remove-pending-ids proto-id :pending-unstarred (list id)))))
       ((eq action 'remove)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-read (list id))
          (elfeed-protocol-remove-pending-ids proto-id :pending-unread (list id)))
         ((eq tag elfeed-protocol-newsblur-star-tag)
          (elfeed-protocol-append-pending-ids proto-id :pending-unstarred (list id))
          (elfeed-protocol-remove-pending-ids proto-id :pending-starred (list id)))))))))

(defun elfeed-protocol-newsblur-pre-tag (host-url entries &rest tags)
  "Sync unread, starred and published states before tags added.
HOST-URL is the host name of NewsBlur server.  ENTRIES is the
target entry objects.  TAGS is the tags are adding now."
  (dolist (tag tags)
    (cl-loop for entry in entries
             unless (elfeed-tagged-p tag entry)
             do (elfeed-protocol-newsblur-append-pending-id host-url entry tag 'add)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-newsblur-sync-pending-ids host-url)))

(defun elfeed-protocol-newsblur-pre-untag (host-url entries &rest tags)
  "Sync unread, starred and published states before tags removed.
HOST-URL is the host name of NewsBlur server.  ENTRIES is the
target entry objects.  TAGS is the tags are removing now."
  (dolist (tag tags)
    (cl-loop for entry in entries
             when (elfeed-tagged-p tag entry)
             do (elfeed-protocol-newsblur-append-pending-id host-url entry tag 'remove)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-newsblur-sync-pending-ids host-url)))

(defun elfeed-protocol-newsblur-update-subfeed (host-url feed-url &optional callback)
  "Update entries under special sub feed in NewsBlur.
HOST-URL is the host name of NewsBlur server, FEED-URL is the
target sub feed url, if CALLBACK is not nil will call it with the
result entries as argument."
  (interactive)
  (let* ((feed-id (elfeed-protocol-newsblur--get-subfeed-id host-url feed-url)))
    (when feed-id
      (elfeed-protocol-newsblur-fetch-prepare
        host-url
        (elfeed-protocol-newsblur--do-update host-url 'update-subfeed feed-id callback)))))

(defun elfeed-protocol-newsblur-update (host-or-subfeed-url &optional callback)
  "NewsBlur protocol updater.
HOST-OR-SUBFEED-URL could be the host name of NewsBlur server, and
user field authentication info is always required so could find the
related protocol feed id correctly, for example
\"https://user@myhost.com\".  And HOST-OR-SUBFEED-URL also could be
the sub feed url, too, for example
\"https://user@myhost.com::https://subfeed.com\".  If CALLBACK is not
nil will call it with the result entries as argument"
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((host-url (elfeed-protocol-host-url host-or-subfeed-url))
         (feed-url (elfeed-protocol-subfeed-url host-or-subfeed-url))
         (proto-id (elfeed-protocol-newsblur-id host-url))
         (last-modified (elfeed-protocol-get-last-modified proto-id)))
    (elfeed-protocol-add-unknown-feed proto-id) ; add unknown feed for fallback
    (elfeed-protocol-newsblur-sync-pending-ids host-url)
    (if feed-url (elfeed-protocol-newsblur-update-subfeed host-url feed-url callback)
      (elfeed-protocol-newsblur-fetch-prepare
        host-url
        (if (> last-modified 0)
            (elfeed-protocol-newsblur--do-update host-url 'update nil callback)
          (elfeed-protocol-newsblur--do-update host-url 'init nil callback))))))

(provide 'elfeed-protocol-newsblur)

;;; elfeed-protocol-newsblur.el ends here
