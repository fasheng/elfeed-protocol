;;; elfeed-protocol.el --- Provide fever/newsblur/owncloud/ttrss protocols for elfeed -*- lexical-binding: t; -*-

;; Author: Xu Fasheng <fasheng[AT]fasheng.info>
;; URL: https://github.com/fasheng/elfeed-protocol
;; Version: 0.3.2
;; Package-Version: 20170501.1349
;; Package-Requires : ((emacs "24.4") (elfeed "2.1.1") (cl-lib "0.5"))
;; Keywords: news

;;; Commentary:
;; elfeed-protocol provide extra protocols to make self-hosting RSS
;; readers like Fever, NewsBlur, ownCloud News and Tiny TIny RSS work
;; with elfeed.  See the README for full documentation.
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
;;                        "owncloud+https://user:pass@myhost.com"
;;
;;                        ;; format 2, for password with special characters
;;                        ("owncloud+https://user@myhost.com"
;;                         :password "password/with|special@characters:")
;;
;;                        ;; format 3, for password in file
;;                        ("owncloud+https://user@myhost.com"
;;                         :password-file "~/.password")
;;
;;                        ;; format 4, for password in .authinfo, ensure (auth-source-search :host "myhost.com" :port "443" :user "user4") exists
;;                        ("owncloud+https://user@myhost.com"
;;                         :use-authinfo t)
;;
;;                        ;; format 5, for password in gnome-keyring
;;                        ("owncloud+https://user@myhost.com"
;;                         :password (shell-command-to-string "secret-tool lookup attribute value"))
;;
;;                        ;; format 6, for password in pass(1), using password-store.el
;;                        ("owncloud+https://user@myhost.com"
;;                         :password (password-store-get "owncloud/app-pass"))
;;
;;                        ;; use autotags
;;                        ("owncloud+https://user@myhost.com"
;;                         :password "password"
;;                         :autotags '(("example.com" comic)))))
;;   (elfeed-protocol-enable)

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-protocol-common)
(require 'elfeed-protocol-fever)
(require 'elfeed-protocol-newsblur)
(require 'elfeed-protocol-owncloud)
(require 'elfeed-protocol-ttrss)

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
  (elfeed-protocol-register "fever" (list :update 'elfeed-protocol-fever-update
                                          :pre-tag 'elfeed-protocol-fever-pre-tag
                                          :pre-untag 'elfeed-protocol-fever-pre-untag))
  (elfeed-protocol-register "newsblur" (list :update 'elfeed-protocol-newsblur-update
                                             :pre-tag 'elfeed-protocol-newsblur-pre-tag
                                             :pre-untag 'elfeed-protocol-newsblur-pre-untag))
  (elfeed-protocol-register "owncloud" (list :update 'elfeed-protocol-owncloud-update
                                             :pre-tag 'elfeed-protocol-owncloud-pre-tag
                                             :pre-untag 'elfeed-protocol-owncloud-pre-untag))
  (elfeed-protocol-register "ttrss" (list :update 'elfeed-protocol-ttrss-update
                                          :pre-tag 'elfeed-protocol-ttrss-pre-tag
                                          :pre-untag 'elfeed-protocol-ttrss-pre-untag)))

;;;###autoload
(defun elfeed-protocol-disable ()
  "Disable hooks and advices for elfeed-protocol."
  (interactive)
  (advice-remove 'elfeed-feed-list #'elfeed-protocol-advice-feed-list)
  (advice-remove 'elfeed-update-feed #'elfeed-protocol-advice-update-feed)
  (advice-remove 'elfeed-feed-autotags #'elfeed-protocol-advice-feed-autotags)
  (remove-hook 'elfeed-tag-hooks 'elfeed-protocol-on-tag-add)
  (remove-hook 'elfeed-untag-hooks 'elfeed-protocol-on-tag-remove)
  (elfeed-protocol-unregister "fever")
  (elfeed-protocol-unregister "newsblur")
  (elfeed-protocol-unregister "owncloud")
  (elfeed-protocol-unregister "ttrss"))

(provide 'elfeed-protocol)

;;; elfeed-protocol.el ends here
