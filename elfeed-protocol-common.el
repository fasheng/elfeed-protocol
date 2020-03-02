;;; elfeed-protocol-common.el --- elfeed-protocol variables and help functions -*- lexical-binding: t; -*-

;;; Commentary:
;; elfeed-protocol variables and help functions

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'elfeed)

(defvar elfeed-protocol-log-trace nil
  "Show more logs than debug level.")

(defvar elfeed-protocol-lazy-sync nil
  "If not nil, will synchronize read/unread/starred/unstarred states to remote
server in next update operation.")

(defconst elfeed-protocol-unknown-feed-url "unknown-feed")
(defconst elfeed-protocol-unknown-feed-title "Unknown Feed")

(defun elfeed-protocol-add-unknown-feed (proto-id)
  "Add unknown feed for fallback."
  (let* ((feed-url elfeed-protocol-unknown-feed-url)
         (feed-id (elfeed-protocol-format-subfeed-id
                   proto-id feed-url))
         (feed-title elfeed-protocol-unknown-feed-title)
         (feed-db (elfeed-db-get-feed feed-id)))
    (setf (elfeed-feed-url feed-db) feed-id
          (elfeed-feed-title feed-db) feed-title)))

(defun elfeed-protocol-feed-p (url-or-feed)
  "Check if a URL-OR-FEED contain extra protocol."
  (let ((feed-url (if (elfeed-feed-p url-or-feed)
                 (or (elfeed-feed-url url-or-feed)
                     (elfeed-feed-id url-or-feed))
               url-or-feed)))
    (eq 'string (type-of (elfeed-protocol-type feed-url)))))

(defun elfeed-protocol-type (feed-url)
  "Get the protocol type in FEED-URL.
For example \"owncloud+https://user@pass:host.com:443\" will return
\"owncloud\". If there is no valid protocol type in FEED-URL, just return nil."
  (let* ((urlobj (url-generic-parse-url feed-url))
         (type (url-type urlobj))
         (list (when type (split-string type "+"))))
    (when (and list (eq 2 (length list)))
      (elt list 0))))

(defun elfeed-protocol-url (feed-url)
  "Get the protocol url in FEED-URL.
For example \"owncloud+https://user@pass:host.com:443\" will return
\"https://user@pass:host.com:443\". If there is no valid protocol type in
FEED-URL, just return nil."
  (let ((proto-type (elfeed-protocol-type feed-url)))
    (when proto-type
      (replace-regexp-in-string
       (regexp-quote (concat proto-type  "+")) "" feed-url))))

(defun elfeed-protocol-id (proto-type url)
  "Build a protocol id for PROTO-TYPE and URL."
  (concat proto-type "+" (elfeed-protocol-no-password-url url)))

(defun elfeed-protocol-no-password-url (url)
  "Remove password field in URL is exists, user field will not change."
  (let* ((urlobj (url-generic-parse-url url))
         (password (url-password urlobj)))
    (if password
        (replace-regexp-in-string
         (concat "\\(" ":" (regexp-quote password) "\\).*\\'") "" url nil nil 1)
      url)))

(defun elfeed-protocol-no-auth-url (url)
  "Remove user and password fields in URL if exists."
  (let* ((urlobj (url-generic-parse-url url))
         (user (url-user urlobj))
         (password (if (url-password urlobj) (url-password urlobj) nil)))
    (if password
        (replace-regexp-in-string
         (concat "\\(" (regexp-quote user) "\\(:" (regexp-quote password)
                 "\\)?@\\).*\\'") "" url nil nil 1)
      url)))

(defun elfeed-protocol-entry-protocol-id (entry)
  "Return protocol url id for specific ENTRY."
  (elfeed-meta entry :protocol-id))

(defun elfeed-protocol-format-subfeed-id (proto-id feed-url)
  "Build feed id for entry.
Which just concat PROTO-ID and FEED-URL, for example
\"owncloud+http://user@myhost.com::http://example.com/rss\""
  (concat proto-id "::" feed-url))

(defun elfeed-protocol-host-url (url)
  "Get host url from the feed id style URL."
  (let ((list (split-string url "::")))
    (if (eq 2 (length list))
        (elt list 0)
      url)))

(defun elfeed-protocol-subfeed-url (url)
  "Get sub feed url from the feed id style URL."
  (let ((list (split-string url "::")))
    (when (eq 2 (length list))
      (elt list 1))))

(defun elfeed-protocol-subfeed-p (url)
  "Check if a URL contain sub feed url."
  (eq 'string (type-of (elfeed-protocol-subfeed-url url))))

(defun elfeed-protocol-meta-feed (proto-id)
  "Get meta protocol feed object in `elfeed-feeds' for PROTO-ID."
  (catch 'found
    (dolist (feed elfeed-feeds)
      (let* ((feed-url (cl-typecase feed
                          (list (when (stringp (car feed)) (car feed)))
                          (string feed)))
             (feed-proto-id (elfeed-protocol-no-password-url feed-url)))
        (when (string-match (concat "^" (regexp-quote feed-proto-id)) proto-id)
          (throw 'found feed))))))

(defun elfeed-protocol-meta-url (proto-id)
  "Get meta protocol feed url in `elfeed-feeds' for PROTO-ID."
  (let* ((feed (elfeed-protocol-meta-feed proto-id))
         (feed-url (cl-typecase feed
                     (list (when (stringp (car feed)) (car feed)))
                     (string feed))))
    feed-url))

(defun elfeed-protocol-meta-data (proto-id prop)
  "Get meta property data in `elfeed-feeds` for PROTO-ID.
PROP could be :password, :autotags etc."
  (let* ((feed (elfeed-protocol-meta-feed proto-id))
         (proto-props (when (listp feed) (cdr feed))))
    (plist-get proto-props prop)))

(defun elfeed-protocol-meta-user (proto-id)
  "Get user property data in `elfeed-feeds` for PROTO-ID."
  (let* ((proto-url (elfeed-protocol-meta-url proto-id))
         (urlobj (url-generic-parse-url (elfeed-protocol-url proto-url))))
    (url-user urlobj)))

(defun elfeed-protocol-meta-password (proto-id)
  "Get :password property data in `elfeed-feeds` for PROTO-ID.
Will try to get password in url, password filed, passowrd file and
.authinfo one by one."
  (let* ((proto-url (elfeed-protocol-meta-url proto-id))
         (urlobj (url-generic-parse-url (elfeed-protocol-url proto-url)))
         (meta-pass (elfeed-protocol-meta-data proto-id :password)))
    (cond
     ((url-password urlobj) (url-password urlobj))

     ((and meta-pass (stringp meta-pass))
      meta-pass)

     ((and meta-pass (functionp meta-pass))
      (funcall meta-pass))

     ((and meta-pass (listp meta-pass) (functionp (car meta-pass)))
      (eval meta-pass))

     ((elfeed-protocol-meta-data proto-id :password-file)
      (elfeed-protocol-get-string-from-file
       (elfeed-protocol-meta-data proto-id :password-file)))

     ((elfeed-protocol-meta-data proto-id :use-authinfo)
      (require 'auth-source)
      (let* ((auth-info (auth-source-search :host (url-host urlobj)
                                            :port (url-port urlobj)
                                            :user (url-user urlobj)))
             (secret (plist-get (car auth-info) :secret)))
        (if (functionp secret) (funcall secret) secret))))))

(defun elfeed-protocol-get-string-from-file (path)
  "Return file content in PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun elfeed-protocol-meta-autotags (proto-id)
  "Get :autotags property data in `elfeed-feeds` for PROTO-ID."
  (elfeed-protocol-meta-data proto-id :autotags))

(defun elfeed-protocol-feed-autotags (proto-id url-or-feed)
  "Return autotags for protocol feed.
Similar with `elfeed-feed-autotags' but `elfeed-feeds' overrode by `:autotags'
item in protocol properties.  PROTO-ID is the protocol id and URL-OR-FEED is the
traget child feed url under protocol feed"
  (let ((elfeed-feeds (elfeed-protocol-meta-autotags proto-id)))
    (elfeed-feed-autotags url-or-feed)))

(defun elfeed-protocol-get-feed-meta-data (proto-id key)
  "Get meta data in feed db.
PROTO-ID is the target protocol feed id.  KEY is the key name."
  (let* ((feed (elfeed-db-get-feed proto-id)))
    (elfeed-meta feed key)))

(defun elfeed-protocol-set-feed-meta-data (proto-id key value)
  "Get meta data in feed db.
PROTO-ID is the target protocol feed id.  KEY is the key name.  VALUE is the
target value."
  (let* ((feed (elfeed-db-get-feed proto-id)))
    (setf (elfeed-meta feed key) value)))

(defun elfeed-protocol-get-last-modified (proto-id)
  "Get last entry modified time.
PROTO-ID is the target protocol feed id.  If not initialized just return 0.  The
last modified time was saved in elfeed db as a mock feed."
  (let* ((last-modified (elfeed-protocol-get-feed-meta-data proto-id :last-modified)))
    (if last-modified
        last-modified
      0)))
(defun elfeed-protocol-set-last-modified (proto-id last-modified)
  "Set last entry modified time.
PROTO-ID is the target protocol feed id.  LAST-MODIFIED is the target value."
  (elfeed-protocol-set-feed-meta-data proto-id :last-modified last-modified))

(defun elfeed-protocol-get-first-entry-id (proto-id)
  "Get first entry id.
PROTO-ID is the target protocol feed id.  If not initialized, just return -1."
  (let* ((last-entry-id (elfeed-protocol-get-feed-meta-data proto-id :first-entry-id)))
    (if last-entry-id
        last-entry-id
      -1)))
(defun elfeed-protocol-set-first-entry-id (proto-id first-entry-id)
  "Set first entry id to elfeed db.
PROTO-ID is the target protocol feed id.  FIRST-ENTRY-ID is the target value."
  (elfeed-protocol-set-feed-meta-data proto-id :first-entry-id first-entry-id))

(defun elfeed-protocol-get-last-entry-id (proto-id)
  "Get last entry id.
PROTO-ID is the target protocol feed id.  If not initialized, just return -1."
  (let* ((last-entry-id (elfeed-protocol-get-feed-meta-data proto-id :last-entry-id)))
    (if last-entry-id
        last-entry-id
      -1)))
(defun elfeed-protocol-set-last-entry-id (proto-id last-entry-id)
  "Set last entry id to elfeed db.
PROTO-ID is the target protocol feed id.  LAST-ENTRY-ID is the target value."
  (elfeed-protocol-set-feed-meta-data proto-id :last-entry-id last-entry-id))

(defun elfeed-protocol-get-pending-ids (proto-id key)
  "Get read/unread/starred/unstarred pending ids that to synchronize later.
PROTO-ID is the target protocol feed id.  KEY could be :pending-read,
:pending-unread, :pending-starred, :pending-unstarred, :pending-published and
:pending-unpublished."
  (interactive (list (completing-read "Protocol Feed: " (elfeed-protocol-feed-list))
                     (intern (completing-read
                              "Key name: "
                              '(:pending-read :pending-unread :pending-starred :pending-unstarred
                                              :pending-published :pending-unpublished)))))
  (let* ((pending-ids (elfeed-protocol-get-feed-meta-data proto-id key)))
    (if (> (length pending-ids) 0)
        pending-ids
      nil)))
(defun elfeed-protocol-set-pending-ids (proto-id key ids)
  "Set read/unread/starred/unstarred pending ids that to synchronize later.
PROTO-ID is the target protocol feed id.  KEY could be :pending-read,
:pending-unread, :pending-starred, :pending-unstarred, :pending-published and
:pending-unpublished. IDS is the id list."
  (interactive (list (completing-read "Protocol Feed: " (elfeed-protocol-feed-list))
                     (intern (completing-read
                              "Key name: "
                              '(:pending-read :pending-unread :pending-starred :pending-unstarred
                                              :pending-published :pending-unpublished)))))
  (elfeed-protocol-set-feed-meta-data proto-id key ids))

(defun elfeed-protocol-append-pending-ids (proto-id key ids)
  "Append pending read/unread/starred/unstarred ids that to synchronize later.
PROTO-ID is the target protocol feed id.  KEY could be :pending-read,
:pending-unread, :pending-starred and :pending-unstarred. IDS is the id list to
append."
  (let* ((pending-ids (elfeed-protocol-get-pending-ids proto-id key)))
    (dolist (id ids)
      (cl-pushnew id pending-ids))
    (elfeed-protocol-set-pending-ids proto-id key pending-ids)))
(defun elfeed-protocol-remove-pending-ids (proto-id key ids)
  "Remove pending read/unread/starred/unstarred ids that to synchronize later.
PROTO-ID is the target protocol feed id.  KEY could be :pending-read,
:pending-unread, :pending-starred and :pending-unstarred. IDS is the id list to
remove."
  (let* ((pending-ids (elfeed-protocol-get-pending-ids proto-id key)))
    (dolist (id ids)
      (setq pending-ids (delete id pending-ids)))
    (elfeed-protocol-set-pending-ids proto-id key pending-ids)))

(defun elfeed-protocol-clean-pending-ids (proto-id)
  "Clean pending read/unread/starred/unstarred entry states.
PROTO-ID is the target protocol feed id."
  (elfeed-protocol-set-pending-ids proto-id :pending-read nil)
  (elfeed-protocol-set-pending-ids proto-id :pending-unread nil)
  (elfeed-protocol-set-pending-ids proto-id :pending-starred nil)
  (elfeed-protocol-set-pending-ids proto-id :pending-unstarred nil)
  (elfeed-protocol-set-pending-ids proto-id :pending-published nil)
  (elfeed-protocol-set-pending-ids proto-id :pending-unpublished nil))

(defun elfeed-protocol-generate-ids-str (separate start end)
  "Generate article ids string from START id to END id.
SEPARATE is the string to be insert between each id."
  (string-trim-right (cl-loop for id from start to end concat (format "%d%s" id separate)) separate))

(defun elfeed-protocol-join-ids-to-str (separate &rest ids)
  "Convert article ids to string format, for example from (1 2) to \"1,2\".
SEPARATE is the string to be insert between each id, IDS is the target id array."
  (string-trim-right (cl-loop for id in ids concat (format "%d%s" id separate)) separate))

(defun elfeed-protocol-split-ids-sub-size (separate ids sub-size)
  "Convert article ids to sub string list, for example from \"1,2,3\" to (\"1,2\" \"3\") if sub-size is 2.
SEPARATE is the separate string. IDS is the a comma-separated string of item
ids. SUB-SIZE is the item size to split for each request."
  (let* ((ids-list (split-string ids separate))
         (size (length ids-list))
         (cycles (max 1 (ceiling (/ (float size) sub-size)))))
    (cl-loop for i from 0 to (1- cycles) collect
             (string-trim-right
              (cl-loop for j from (* i sub-size) to (1- (min size (* (1+ i) sub-size))) concat
                       (format "%s%s" (elt ids-list j) separate)) separate))))

(defun elfeed-protocol-build-entry-groups (entries)
  "Split ENTRIES to groups with the same protocol url id."
  (let* ((entry-groups (make-hash-table :test 'equal)))
    (cl-loop for entry in entries
             do (progn
                  (let ((proto-id (elfeed-protocol-entry-protocol-id entry)))
                    (when proto-id
                      (puthash proto-id (append
                                         (gethash proto-id entry-groups) (list entry))
                               entry-groups)))))
    entry-groups))

(defun elfeed-protocol-feed-list ()
  "Get protocol feed list."
  (let* ((feed-url-list (cl-loop for feed in elfeed-feeds
                                 when (listp feed) collect (car feed)
                                 else collect feed)))
    (cl-loop for url in feed-url-list
             when (elfeed-protocol-type url) collect url)))

(defun elfeed-protocol-normal-feed-list ()
  "Get normal none protocol feed list."
  (let* ((feed-url-list (cl-loop for feed in elfeed-feeds
                                 when (listp feed) collect (car feed)
                                 else collect feed)))
    (cl-loop for url in feed-url-list
             unless (elfeed-protocol-type url) collect url)))

(provide 'elfeed-protocol-common)

;;; elfeed-protocol-common.el ends here
