;;; elfeed-protocol.el --- Provide fever/newsblur/owncloud/ttrss protocols for elfeed -*- lexical-binding: t; -*-

;; Author: Xu Fasheng <fasheng[AT]fasheng.info>
;; URL: https://github.com/fasheng/elfeed-protocol
;; Version: 0.3.2
;; Package-Version: 20170501.1349
;; Package-Requires : ((emacs "24.4") (elfeed "4.0.0") (cl-lib "0.5"))
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
;;   (setq elfeed-feeds '(("owncloud+https://user@myhost.com"
;;                                  :password "my-password")))
;;
;;   ;; enable elfeed-protocol
;;   (elfeed-protocol-enable)

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-protocol-common)

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

(defcustom elfeed-protocol-enabled-protocols '(fever newsblur owncloud ttrss)
  "Protocols that should always be loaded together when elfeed-protocol-enable.

You can also use this system to load external packages (i.e. neither elfeed-protocol
core protocols, nor external protocols).  Just add symbols
to the end of the list.  If the package is called elfeed-protocol-xyz.el, then you need
to add the symbol `xyz', and the package must have a call to:

(provide \\='elfeed-protocol-xyz)"
  :group 'elfeed-protocol
  :type '(repeat symbol))

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

(defun elfeed-protocol-fetcher (url cb)
  "New fetcher hook for `elfeed-fetch-functions` after `elfeed` 20260518.1004 to replace old advice for `elfeed-update-feed`."
  (when (elfeed-protocol-feed-p url)
    (let* ((proto-type (elfeed-protocol-type url))
           (update-func (elfeed-protocol-update-func proto-type)))
      (if update-func
          (progn
            (funcall update-func (elfeed-protocol-url url))
            (funcall cb :success))
        (elfeed-log 'error "elfeed-protocol: there is not updater for protocol %s"
                    proto-type)
        (funcall cb :error)))
    t))

;;;###autoload
(defun elfeed-protocol-enable ()
  "Enable hooks and advices for elfeed-protocol."
  (interactive)
  ;; Migrate metadata from elfeed-db :feeds field to :protocol-feeds
  (dolist (proto-id (elfeed-protocol-feed-list))
    (let* ((old-metadata (elfeed-meta--plist (elfeed-db-get-feed proto-id)))
           (cur-metadata (elfeed-protocol-get-db-feed-meta-all proto-id)))
      (when (and old-metadata (not cur-metadata))
        (elfeed-log 'info "elfeed-protocol: Migrate metadata for %s %s" proto-id old-metadata)
        (elfeed-protocol-set-db-feed-meta-all proto-id old-metadata))))

  (add-hook 'elfeed-fetch-functions #'elfeed-protocol-fetcher)
  (add-hook 'elfeed-tag-hooks #'elfeed-protocol-on-tag-add)
  (add-hook 'elfeed-untag-hooks #'elfeed-protocol-on-tag-remove)
  (dolist (protocol elfeed-protocol-enabled-protocols)
    (let ((feature (intern (concat "elfeed-protocol-" (symbol-name protocol)))))
      (if (require feature nil t)
          (elfeed-protocol-register
           (symbol-name protocol)
           (list :update (intern (concat (symbol-name feature) "-update"))
                 :pre-tag (intern (concat (symbol-name feature) "-pre-tag"))
                 :pre-untag (intern (concat (symbol-name feature) "-pre-untag"))))
        (error "Problems while trying to load feature `%s'" feature)))))

;;;###autoload
(defun elfeed-protocol-disable ()
  "Disable hooks and advices for elfeed-protocol."
  (interactive)
  (remove-hook 'elfeed-fetch-functions #'elfeed-protocol-fetcher)
  (remove-hook 'elfeed-tag-hooks #'elfeed-protocol-on-tag-add)
  (remove-hook 'elfeed-untag-hooks #'elfeed-protocol-on-tag-remove)
  (dolist (protocol elfeed-protocol-enabled-protocols)
    (elfeed-protocol-unregister (symbol-name protocol))))

(provide 'elfeed-protocol)

;;; elfeed-protocol.el ends here
