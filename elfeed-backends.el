;;; elfeed-backends.el --- Provide extra bakcends to make such like ownCloud News works with elfeed -*- lexical-binding: t; -*-

;; Author: Xu Fasheng <fasheng.xu@gmail.com>
;; URL: https://github.com/fasheng/elfeed-backends
;; Version: 0.3
;; Package-Version: 20170501.1349
;; Keywords: elfeed

;;; Commentary:

;; elfeed-backends provide extra backends to make self-hosting RSS
;; readers like ownCloud News works with elfeed. See the README for
;; full documentation.
;;
;; Usage:
;;
;;   (require 'elfeed-backends)
;;   (elfeed-backends-enable)
;;   (setq elfeed-backends-source 'ocnews)
;;   (setq elfeed-backends-ocnews-url "http://127.0.0.1:8080")
;;   (setq elfeed-backends-ocnews-username "user")
;;   (setq elfeed-backends-ocnews-password "password")
;;   (setq elfeed-use-curl t)
;;   (elfeed-set-timeout 36000)
;;   (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate

;;; Code:

(require 'elfeed)
(require 'elfeed-backends-ocnews)

(defgroup elfeed-backends ()
  "Provide extra backends for elfeed."
  :group 'comm)

(defcustom elfeed-backends-source 'local
  "elfeed backends source type, could be local and ocnews. local means the
default origin backend, ocnews means the ownCloud News backend."
  :group 'elfeed-backends
  :type '(choice (const local) (const ocnews)))

(defcustom elfeed-backends-ignore-tag-action nil
  "elfeed-backends will override `elfeed-tag' and `elfeed-untag' to sync
unread/starred tag states to remote service, if
`elfeed-backends-ignore-tag-action' not nil, such operation will be skipped. You
may not like to set it manually."
  :group 'elfeed-backends
  :type 'bool)

(defun elfeed-backends-is-status-error (status use-curl)
  "Check if HTTP request returned status means a error."
  (or (and use-curl (null status)) ; nil = error
      (and (not use-curl) (eq (car status) :error))))

;;; Override elfeed origin functions

(defun elfeed-backends-update ()
  "Override elfeed-update to make elfeed-backends works."
  (interactive)
  (elfeed-log 'info "Elfeed update: %s"
              (format-time-string "%B %e %Y %H:%M:%S %Z"))
  (let ((elfeed--inhibit-update-init-hooks t))
    (cond
     ((eq elfeed-backends-source 'ocnews) (elfeed-backends-ocnews-update))
     ((eq elfeed-backends-source 'local)
      (mapc #'elfeed-update-feed (elfeed--shuffle (elfeed-feed-list))))))
  (run-hooks 'elfeed-update-init-hooks)
  (elfeed-db-save))

(defun elfeed-backends-tag (entry &rest tags)
  "Override `elfeed-tag' to sync unread/starred states to remote server if necessary."
  (unless elfeed-backends-ignore-tag-action
    (let* ((current (elfeed-entry-tags entry))
           (add-unread (memq 'unread tags))
           (add-star (memq elfeed-backends-ocnews-star-tag tags))
           (unread-modified (and add-unread (not (memq 'unread current))))
           (star-modified (and add-star
                               (not (memq elfeed-backends-ocnews-star-tag current)))))
      (when (elfeed-backends-ocnews-is-ocnews-entry entry)
        (when unread-modified
          (elfeed-backends-ocnews-mark-unread entry))
        (when star-modified
          (elfeed-backends-ocnews-mark-star entry))))))

(defun elfeed-backends-untag (entry &rest tags)
  "Override `elfeed-untag' to sync unread/starred states to remote server if necessary."
  (unless elfeed-backends-ignore-tag-action
    (let* ((current (elfeed-entry-tags entry))
           (remove-unread (memq 'unread tags))
           (remove-star (memq elfeed-backends-ocnews-star-tag tags))
           (unread-modified (and remove-unread (memq 'unread current)))
           (star-modified (and remove-star
                               (memq elfeed-backends-ocnews-star-tag current))))
      (when (elfeed-backends-ocnews-is-ocnews-entry entry)
        (when unread-modified
          (elfeed-backends-ocnews-mark-read entry))
        (when star-modified
          (elfeed-backends-ocnews-mark-unstar entry))))))

(defun elfeed-backends-search-fetch (orig-func &rest args)
  "Override `elfeed-search-fetch'. Skip elfeed-search-fetch-visible operation
for elfeed-backends."
  (if (eq elfeed-backends-source 'local)
      (apply orig-func args)
    (elfeed-update)))

(defun elfeed-backends-search-yank (orig-func &rest args)
  "Overrid `elfeed-search-yank'. Sync multiple entries tags to
backends server in one operation"
  (let ((elfeed-backends-ignore-tag-action t)
        (tag (car args)))
    (elfeed-backends-ocnews-sync-tag-multi (elfeed-search-selected) 'unread 'remove)
    (apply orig-func args)))

(defun elfeed-backends-search-tag-all (orig-func &rest args)
  "Overrid `elfeed-search-tag-all'. Sync multiple entries tags to
backends server in one operation"
  (let ((elfeed-backends-ignore-tag-action t)
        (tag (car args)))
    (elfeed-backends-ocnews-sync-tag-multi (elfeed-search-selected) tag 'add)
    (apply orig-func args)))

(defun elfeed-backends-search-untag-all (orig-func &rest args)
  "Overrid `elfeed-search-untag-all'. Sync multiple entries tags to
backends server in one operation"
  (let ((elfeed-backends-ignore-tag-action t)
        (tag (car args)))
    (elfeed-backends-ocnews-sync-tag-multi (elfeed-search-selected) tag 'remove)
    (apply orig-func args)))

(defun elfeed-backends-search-toggle-all (orig-func &rest args)
  "Overrid `elfeed-search-toggle-all'. Sync multiple entries tags to
backends server in one operation"
  (let ((elfeed-backends-ignore-tag-action t)
        (tag (car args)))
    (elfeed-backends-ocnews-sync-tag-multi (elfeed-search-selected) tag 'toggle)
    (apply orig-func args)))

;;;###autoload
(defun elfeed-backends-enable ()
  "Enable hooks and advices for elfeed-backends."
  (interactive)
  (advice-add 'elfeed-update :override #'elfeed-backends-update)
  (advice-add 'elfeed-tag :before #'elfeed-backends-tag)
  (advice-add 'elfeed-untag :before #'elfeed-backends-untag)
  (advice-add 'elfeed-search-fetch :around #'elfeed-backends-search-fetch)
  (advice-add 'elfeed-search-yank :around #'elfeed-backends-search-yank)
  (advice-add 'elfeed-search-tag-all :around #'elfeed-backends-search-tag-all)
  (advice-add 'elfeed-search-untag-all :around #'elfeed-backends-search-untag-all)
  (advice-add 'elfeed-search-toggle-all :around #'elfeed-backends-search-toggle-all))

;;;###autoload
(defun elfeed-backends-disable ()
  "Disable hooks and advices elfeed-backends."
  (interactive)
  (advice-remove 'elfeed-update #'elfeed-backends-update)
  (advice-remove 'elfeed-tag #'elfeed-backends-tag)
  (advice-remove 'elfeed-untag #'elfeed-backends-untag)
  (advice-remove 'elfeed-search-fetch #'elfeed-backends-search-fetch)
  (advice-remove 'elfeed-search-yank #'elfeed-backends-search-yank)
  (advice-remove 'elfeed-search-tag-all #'elfeed-backends-search-tag-all)
  (advice-remove 'elfeed-search-untag-all #'elfeed-backends-search-untag-all)
  (advice-remove 'elfeed-search-toggle-all #'elfeed-backends-search-toggle-all))

(provide 'elfeed-backends)

;;; elfeed-backends.el ends here
