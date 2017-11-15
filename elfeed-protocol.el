;;; elfeed-protocol.el --- Provide ownCloud and other protocols for elfeed -*- lexical-binding: t; -*-

;; Author: Xu Fasheng <fasheng.xu@gmail.com>
;; URL: https://github.com/fasheng/elfeed-protocol
;; Version: 0.3.2
;; Package-Version: 20170501.1349
;; Package-Requires : ((emacs "24.4") (elfeed "2.1.1") (cl-lib "0.5"))
;; Keywords: news

;;; Commentary:
;; elfeed-protocol provide extra protocols to make self-hosting RSS
;; readers like ownCloud News works with elfeed.  See the README for
;; full documentation.
;;
;; Usage:
;;
;;   ;; curl recommend
;;   (setq elfeed-use-curl t)
;;   (elfeed-set-timeout 36000)
;;   (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate
;;
;;   ;; setup extra protocol feeds
;;   (require 'elfeed-protocol)
;;   (setq elfeed-feeds (list
;;                       "owncloud+https://user1:pass1@myhost.com"
;;                       (list "owncloud+https://user2@myhost.com"
;;                             :password "password/with|special@characters:"
;;                             :autotags '(("example.com" comic))
;;                             )))
;;   (elfeed-protocol-enable)

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-owncloud)

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

(defun elfeed-protocol-feed-p (feed-url)
  "Check if a FEED-URL contain extra protocol."
  (eq 'string (type-of (elfeed-protocol-type feed-url))))

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
  (cdr (assoc proto-type elfeed-protocol-list)))

(defun elfeed-protocol-register (proto-type update-func)
  "Register PROTO-TYPE with UPDATE-FUNC to `elfeed-protocol-list'."
  (if (elfeed-protocol-update-func proto-type)
      (setf (cdr (assoc proto-type elfeed-protocol-list)) update-func)
    (add-to-list 'elfeed-protocol-list (cons proto-type update-func))))

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
  "Remove user and password fields in URL is exists."
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

(defun elfeed-protocol-format-entry-feed-id (proto-id feed-url)
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

(defun elfeed-protocol-feed-url (url)
  "Get feed url from the feed id style URL."
  (let ((list (split-string url "::")))
    (when (eq 2 (length list))
      (elt list 1))))

(defun elfeed-protocol-id-to-feed (proto-id)
  "Get related feed object in `elfeed-feeds' through the PROTO-ID."
  (catch 'found
    (dolist (feed elfeed-feeds)
      (let* ((feed-url (cl-typecase feed
                          (list (when (stringp (car feed)) (car feed)))
                          (string feed)))
             (feed-proto-id (elfeed-protocol-no-password-url feed-url)))
        (when (string-match (concat "^" (regexp-quote feed-proto-id)) proto-id)
          (throw 'found feed))))))

(defun elfeed-protocol-id-to-url (proto-id)
  "Get related feed protocol url in `elfeed-feeds' through the PROTO-ID."
  (let* ((feed (elfeed-protocol-id-to-feed proto-id))
         (feed-url (cl-typecase feed
                     (list (when (stringp (car feed)) (car feed)))
                     (string feed))))
    feed-url))

(defun elfeed-protocol-feed-autotags (proto-id url-or-feed)
  "Return autotags for protocol feed.
Similar with `elfeed-feed-autotags' but `elfeed-feeds' overrode by `:autotags'
item in protocol properties.  PROTO-ID is the protocol id and URL-OR-FEED is the
traget child feed url under protocol feed"
  (let* ((feed (elfeed-protocol-id-to-feed proto-id))
         (proto-props (when (listp feed) (cdr feed)))
         (elfeed-feeds (plist-get proto-props :autotags)))
    (elfeed-feed-autotags url-or-feed)))

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
                      (proto-url (elfeed-protocol-id-to-url proto-id))
                      (url (elfeed-protocol-url proto-url)))
                 (apply (intern (concat "elfeed-protocol-" proto-type "-pre-tag"))
                        url proto-entries tags)))
             entry-groups)))

(defun elfeed-protocol-on-tag-remove (entries tags)
  "Dispatch for tags removed.
Will split ENTRIES to groups and dispatched TAGS by different protocols."
  (let* ((entry-groups (elfeed-protocol-build-entry-groups entries)))
    (maphash (lambda (proto-id proto-entries)
               (let* ((proto-type (elfeed-protocol-type proto-id))
                      (proto-url (elfeed-protocol-id-to-url proto-id))
                      (url (elfeed-protocol-url proto-url)))
                 (apply (intern (concat "elfeed-protocol-" proto-type "-pre-untag"))
                        url proto-entries tags)))
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
          (elfeed-log 'error "There is not updater for protocol %s"
                      proto-type)))
    (funcall orig-func url)))

(defun elfeed-protocol-advice-feed-list ()
  "Advice for `elfeed-feed-list' to avoid error checking on protocol feeds."
  (cl-loop for feed in elfeed-feeds
           when (listp feed) collect (car feed)
           else collect feed))

;;;###autoload
(defun elfeed-protocol-enable ()
  "Enable hooks and advices for elfeed-protocol."
  (interactive)
  (advice-add 'elfeed-feed-list :override #'elfeed-protocol-advice-feed-list)
  (advice-add 'elfeed-update-feed :around #'elfeed-protocol-advice-update-feed)
  (add-hook 'elfeed-tag-hooks 'elfeed-protocol-on-tag-add)
  (add-hook 'elfeed-untag-hooks 'elfeed-protocol-on-tag-remove)
  (elfeed-protocol-register "owncloud" 'elfeed-protocol-owncloud-update))

;;;###autoload
(defun elfeed-protocol-disable ()
  "Disable hooks and advices elfeed-protocol."
  (interactive)
  (advice-remove 'elfeed-feed-list #'elfeed-protocol-advice-feed-list)
  (advice-remove 'elfeed-update-feed #'elfeed-protocol-advice-update-feed)
  (remove-hook 'elfeed-tag-hooks 'elfeed-protocol-on-tag-add)
  (remove-hook 'elfeed-untag-hooks 'elfeed-protocol-on-tag-remove)
  (elfeed-protocol-unregister "owncloud"))

(provide 'elfeed-protocol)

;;; elfeed-protocol.el ends here
