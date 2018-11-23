;;; elfeed-protocol.el --- Provide owncloud/ttrss protocols for elfeed -*- lexical-binding: t; -*-

;; Author: Xu Fasheng <fasheng.xu@gmail.com>
;; URL: https://github.com/fasheng/elfeed-protocol
;; Version: 0.3.2
;; Package-Version: 20170501.1349
;; Package-Requires : ((emacs "24.4") (elfeed "2.1.1") (cl-lib "0.5"))
;; Keywords: news

;;; Commentary:
;; elfeed-protocol provide extra protocols to make self-hosting RSS
;; readers like ownCloud News, Tiny TIny RSS and NewsBlur works with
;; elfeed.  See the README for full documentation.
;;
;; Usage:
;;
;;   ;; curl recommend
;;   (setq elfeed-use-curl t)
;;   (elfeed-set-timeout 36000)
;;   (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate
;;
;;   ;; setup extra protocol feeds
;;   (setq elfeed-feeds '(
;;                        ;; format 1
;;                        "owncloud+https://user1:pass1@myhost.com"
;;
;;                        ;; format 2, for password with special characters
;;                        ("owncloud+https://user2@myhost.com"
;;                         :password "password/with|special@characters:")
;;
;;                        ;; format 3, for password in file
;;                        ("owncloud+https://user3@myhost.com"
;;                         :password-file "~/.password")
;;
;;                        ;; format 4, for password in .authinfo, ensure (auth-source-search :host "myhost.com" :port "443" :user "user4") exists
;;                        ("owncloud+https://user4@myhost.com"
;;                         :use-authinfo t)
;;
;;                        ;; format 5, for password in gnome-keyring
;;                        ("owncloud+https://user5@myhost.com"
;;                         :password (shell-command-to-string "secret-tool lookup attribute value"))
;;
;;                        ;; format 5, for password in pass(1), using password-store.el
;;                        ("owncloud+https://user5@myhost.com"
;;                         :password (password-store-get "owncloud/app-pass"))
;;
;;                        ;; use autotags
;;                        ("owncloud+https://user6@myhost.com"
;;                         :password "password"
;;                         :autotags '(("example.com" comic)))))
;;   (elfeed-protocol-enable)

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-protocol-owncloud)
(require 'elfeed-protocol-ttrss)
(require 'elfeed-protocol-newsblur)

(defgroup elfeed-protocol ()
  "Provide extra protocol for elfeed."
  :group 'comm)

(defcustom elfeed-protocol-list ()
  "List of all registered extra protocols in Elfeed.

Could be modified by `elfeed-protocol-register' and
`elfeed-protocol-unregister'.

For example,
  (elfeed-protocol-register \"owncloud\" 'elfeed-protocol-owncloud-update)
  (elfeed-protocol-unregister \"owncloud\")"
  :group 'elfeed-protocol
  :type '(repeat (cons string symbol)))

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

(defun elfeed-protocol-update-func (proto-type)
  "Get update function for special PROTO-TYPE."
  (plist-get (cdr (assoc proto-type elfeed-protocol-list)) ':update))

(defun elfeed-protocol-pre-tag-func (proto-type)
  "Get pre-tag function for special PROTO-TYPE."
  (plist-get (cdr (assoc proto-type elfeed-protocol-list)) ':pre-tag))

(defun elfeed-protocol-pre-untag-func (proto-type)
  "Get pre-untag function for special PROTO-TYPE."
  (plist-get (cdr (assoc proto-type elfeed-protocol-list)) ':pre-untag))

(defun elfeed-protocol-register (proto-type proto-funcs)
  "Register PROTO-TYPE to `elfeed-protocol-list'.
PROTO-FUNCS is a function list for :update :pre-tag(optinal)
and :pre-untag(optinal) ,

For example:

  (list :update 'elfeed-protocol-xxx-update
        :pre-tag 'elfeed-protocol-xxx-pre-tag
        :pre-untag 'elfeed-protocol-xxx-pre-untag)"
  (if (elfeed-protocol-update-func proto-type)
      (setf (cdr (assoc proto-type elfeed-protocol-list)) proto-funcs)
    (add-to-list 'elfeed-protocol-list (cons proto-type proto-funcs))))

(defun elfeed-protocol-unregister (proto-type)
  "Unregister a protocol named PROTO-TYPE from `elfeed-protocol-list'."
  (setq elfeed-protocol-list
        (delq (assoc proto-type elfeed-protocol-list) elfeed-protocol-list)))

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
         (password (if (url-password urlobj) (url-password urlobj) "")))
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

(defun elfeed-protocol-get-last-modified (proto-id)
  "Get last entry modified time.
PROTO-ID is the target protocol feed id.  If not initialized just return 0.  The
last modified time was saved in elfeed db as a mock feed."
  (let* ((feed (elfeed-db-get-feed proto-id))
         (last-modified (elfeed-meta feed :last-modified)))
    (if last-modified
        last-modified
      0)))

(defun elfeed-protocol-set-last-modified (proto-id last-modified)
  "Set last entry modified time.
PROTO-ID is the target protocol feed id.  LAST-MODIFIED is the target value."
  (let* ((feed (elfeed-db-get-feed proto-id)))
    (setf (elfeed-meta feed :last-modified) last-modified)))

(defun elfeed-protocol-get-first-entry-id (proto-id)
  "Get first entry id.
PROTO-ID is the target protocol feed id.  If not initialized, just return -1."
  (let* ((feed (elfeed-db-get-feed proto-id))
         (last-entry-id (elfeed-meta feed :first-entry-id)))
    (if last-entry-id
        last-entry-id
      -1)))

(defun elfeed-protocol-set-first-entry-id (proto-id first-entry-id)
  "Set first entry id to elfeed db.
PROTO-ID is the target protocol feed id.  FIRST-ENTRY-ID is the target value."
  (let* ((feed (elfeed-db-get-feed proto-id)))
    (setf (elfeed-meta feed :first-entry-id) first-entry-id)))

(defun elfeed-protocol-get-last-entry-id (proto-id)
  "Get last entry id.
PROTO-ID is the target protocol feed id.  If not initialized, just return -1."
  (let* ((feed (elfeed-db-get-feed proto-id))
         (last-entry-id (elfeed-meta feed :last-entry-id)))
    (if last-entry-id
        last-entry-id
      -1)))

(defun elfeed-protocol-set-last-entry-id (proto-id last-entry-id)
  "Set last entry id to elfeed db.
PROTO-ID is the target protocol feed id.  LAST-ENTRY-ID is the target value."
  (let* ((feed (elfeed-db-get-feed proto-id)))
    (setf (elfeed-meta feed :last-entry-id) last-entry-id)))

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

(defun elfeed-protocol-on-tag-add (entries tags)
  "Dispatch for tags added.
Will split ENTRIES to groups and dispatched TAGS by different protocols."
  (let* ((entry-groups (elfeed-protocol-build-entry-groups entries)))
    (maphash (lambda (proto-id proto-entries)
               (let* ((proto-type (elfeed-protocol-type proto-id))
                      (proto-url (elfeed-protocol-meta-url proto-id))
                      (host-url (elfeed-protocol-url proto-url))
                      (pre-tag-func (elfeed-protocol-pre-tag-func proto-type)))
                 (when pre-tag-func
                   (apply pre-tag-func host-url proto-entries tags))))
             entry-groups)))

(defun elfeed-protocol-on-tag-remove (entries tags)
  "Dispatch for tags removed.
Will split ENTRIES to groups and dispatched TAGS by different protocols."
  (let* ((entry-groups (elfeed-protocol-build-entry-groups entries)))
    (maphash (lambda (proto-id proto-entries)
               (let* ((proto-type (elfeed-protocol-type proto-id))
                      (proto-url (elfeed-protocol-meta-url proto-id))
                      (host-url (elfeed-protocol-url proto-url))
                      (pre-untag-func (elfeed-protocol-pre-untag-func proto-type)))
                 (when pre-untag-func
                   (apply pre-untag-func host-url proto-entries tags))))
             entry-groups)))

(defun elfeed-protocol-advice-update-feed (orig-func url)
  "Advice for `elfeed-update-feed` to update protocol feed correctly.
ORIG-FUNC and URL are the needed arguments."
  (interactive (list (completing-read "Feed: " (elfeed-feed-list))))
  (if (elfeed-protocol-feed-p url)
      (let* ((proto-type (elfeed-protocol-type url))
             (update-func (elfeed-protocol-update-func proto-type)))
        (if update-func
            (progn
              (unless elfeed--inhibit-update-init-hooks
                (run-hooks 'elfeed-update-init-hooks))
              (funcall update-func (elfeed-protocol-url url))
              (run-hook-with-args 'elfeed-update-hooks url))
          (elfeed-log 'error "elfeed-protocol: there is not updater for protocol %s"
                      proto-type)))
    (funcall orig-func url)))

(defun elfeed-protocol-advice-feed-list ()
  "Advice for `elfeed-feed-list' to avoid error checking on protocol feeds."
  (cl-loop for feed in elfeed-feeds
           when (listp feed) collect (car feed)
           else collect feed))

(defun elfeed-protocol-advice-feed-autotags (orig-func url-or-feed)
  "Advice for `elfeed-feed-autotags` to get protocol feed autotags correctly.
ORIG-FUNC and URL-OR-FEED are the needed arguments."
  (let* ((url (if (elfeed-feed-p url-or-feed)
                  (or (elfeed-feed-url url-or-feed)
                      (elfeed-feed-id url-or-feed))
                url-or-feed))
         (proto-autotags (when (elfeed-protocol-subfeed-p url)
                           (let* ((proto-id (elfeed-protocol-host-url url))
                                  (subfeed-url (elfeed-protocol-subfeed-url url)))
                             (elfeed-protocol-feed-autotags proto-id subfeed-url)))))
    (if proto-autotags
        proto-autotags
      (funcall orig-func url))))

;;;###autoload
(defun elfeed-protocol-enable ()
  "Enable hooks and advices for elfeed-protocol."
  (interactive)
  (advice-add 'elfeed-feed-list :override #'elfeed-protocol-advice-feed-list)
  (advice-add 'elfeed-update-feed :around #'elfeed-protocol-advice-update-feed)
  (advice-add 'elfeed-feed-autotags :around #'elfeed-protocol-advice-feed-autotags)
  (add-hook 'elfeed-tag-hooks 'elfeed-protocol-on-tag-add)
  (add-hook 'elfeed-untag-hooks 'elfeed-protocol-on-tag-remove)
  (elfeed-protocol-register "owncloud" (list :update 'elfeed-protocol-owncloud-update
                                             :pre-tag 'elfeed-protocol-owncloud-pre-tag
                                             :pre-untag 'elfeed-protocol-owncloud-pre-untag))
  (elfeed-protocol-register "ttrss" (list :update 'elfeed-protocol-ttrss-update
                                          :pre-tag 'elfeed-protocol-ttrss-pre-tag
                                          :pre-untag 'elfeed-protocol-ttrss-pre-untag))
  (elfeed-protocol-register "newsblur" (list :update 'elfeed-protocol-newsblur-update
                                             :pre-tag 'elfeed-protocol-newsblur-pre-tag
                                             :pre-untag 'elfeed-protocol-newsblur-pre-untag)))

;;;###autoload
(defun elfeed-protocol-disable ()
  "Disable hooks and advices for elfeed-protocol."
  (interactive)
  (advice-remove 'elfeed-feed-list #'elfeed-protocol-advice-feed-list)
  (advice-remove 'elfeed-update-feed #'elfeed-protocol-advice-update-feed)
  (advice-remove 'elfeed-feed-autotags #'elfeed-protocol-advice-feed-autotags)
  (remove-hook 'elfeed-tag-hooks 'elfeed-protocol-on-tag-add)
  (remove-hook 'elfeed-untag-hooks 'elfeed-protocol-on-tag-remove)
  (elfeed-protocol-unregister "owncloud")
  (elfeed-protocol-unregister "ttrss")
  (elfeed-protocol-unregister "newsblur"))

(provide 'elfeed-protocol)

;;; elfeed-protocol.el ends here
