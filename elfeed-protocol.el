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

(defcustom elfeed-protocol-work-with-others
  t
  "If non-nil, will add some hack code to work together with elfeed-org and elfeed-summary."
  :group 'elfeed-protocol
  :type 'boolean)

(defvar elfeed-protocol-orig-feeds nil
  "Store original content of `elfeed-feeds' before `elfeed-org' or
other extensions modifying it.")

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

(defun elfeed-protocol-advice-rmh-elfeed-org-process (orig-func files tree-id)
  "Advice for `rmh-elfeed-org-process' to keep the original
`elfeed-feeds' exists."
  (unless elfeed-protocol-orig-feeds
    (setq elfeed-protocol-orig-feeds elfeed-feeds))
  (funcall orig-func files tree-id)
  (when elfeed-protocol-orig-feeds
    (setq elfeed-feeds (append elfeed-protocol-orig-feeds elfeed-feeds))))

(defun elfeed-protocol-advice-rmh-elfeed-org-export-feed (headline)
  "Advice for `rmh-elfeed-org-export-feed', add elfeed-protocol ID as suffix and add `:no-update' option to each feed."
  (let* ((url (car headline))
         (elfeed-feeds (if elfeed-protocol-orig-feeds elfeed-protocol-orig-feeds elfeed-feeds))
         (proto-id (car (elfeed-protocol-feed-list))))
    (when proto-id
      (setcar headline (elfeed-protocol-format-subfeed-id proto-id url))
      (setcdr headline (append '(:no-update t) (cdr headline))))))

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

  ;; Notice user switch back to elfeed-feeds
  (when (and (boundp 'elfeed-protocol-feeds) (> (length elfeed-protocol-feeds) 0))
    (elfeed-log 'warn "elfeed-protocol: elfeed-protocol-feeds is not empty! Well, since 1.0.0, with the help of elfeed 4.0.0, elfeed-protocol switch back to elfeed-feeds again, please setup it instead of elfeed-protocol-feeds. And it will work together with extensions like elfeed-org and elfeed-summary without any aditional setup. More information to see the README"))

  (add-hook 'elfeed-fetch-functions #'elfeed-protocol-fetcher)
  (add-hook 'elfeed-tag-hooks #'elfeed-protocol-on-tag-add)
  (add-hook 'elfeed-untag-hooks #'elfeed-protocol-on-tag-remove)
  (when elfeed-protocol-work-with-others
    (advice-add 'rmh-elfeed-org-process :around #'elfeed-protocol-advice-rmh-elfeed-org-process)
    (advice-add 'rmh-elfeed-org-export-feed :before #'elfeed-protocol-advice-rmh-elfeed-org-export-feed)
    (setq elfeed-summary-skip-sync-tag ':no-update))
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
  (when elfeed-protocol-work-with-others
    (advice-remove 'rmh-elfeed-org-process #'elfeed-protocol-advice-rmh-elfeed-org-process)
    (advice-remove 'rmh-elfeed-org-export-feed #'elfeed-protocol-advice-rmh-elfeed-org-export-feed))
  (dolist (protocol elfeed-protocol-enabled-protocols)
    (elfeed-protocol-unregister (symbol-name protocol))))

(provide 'elfeed-protocol)

;;; elfeed-protocol.el ends here
