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

(defadvice elfeed-update (around elfeed-backends-update)
  "Override elfeed-update to make elfeed-backends works."
  (interactive)
  (elfeed-log 'info "Elfeed update: %s"
              (format-time-string "%B %e %Y %H:%M:%S %Z"))
  (let ((elfeed--inhibit-update-init-hooks t))
    (cond
     ((eq elfeed-feed-source 'ocnews) (elfeed-ocnews-update))
     ((eq elfeed-feed-source 'local)
      (mapc #'elfeed-update-feed (elfeed--shuffle (elfeed-feed-list))))))
  (run-hooks 'elfeed-update-init-hooks)
  (elfeed-db-save))

;;TODO: remove
;; (defun elfeed-tag-1 (entry &rest tags)
;;   "Add TAGS to ENTRY. In fact it's the origin `elfeed-tag'."
;;   (let ((current (elfeed-entry-tags entry)))
;;     (setf (elfeed-entry-tags entry)
;;           (elfeed-normalize-tags (append tags current)))))

;; (defun elfeed-untag-1 (entry &rest tags)
;;   "Remove TAGS from ENTRY. In fact it's the origin `elfeed-untag'."
;;   (setf (elfeed-entry-tags entry)
;;         (cl-loop for tag in (elfeed-entry-tags entry)
;;                  unless (memq tag tags) collect tag)))

(defadvice elfeed-tag (before elfeed-backends-tag (entry &rest tags))
  "Override `elfeed-tag' to sync unread/starred states to remote server if necessary."
  (unless elfeed-backends-ignore-tag-action
    (let* ((current (elfeed-entry-tags entry))
           (add-unread (memq 'unread tags))
           (add-star (memq elfeed-ocnews-star-tag tags))
           (unread-modified (and add-unread (not (memq 'unread current))))
           (star-modified (and add-star
                               (not (memq elfeed-ocnews-star-tag current)))))
      (when (elfeed-ocnews-is-ocnews-entry entry)
        (when unread-modified
          (elfeed-ocnews-mark-unread entry))
        (when star-modified
          (elfeed-ocnews-mark-star entry))))))

(defadvice elfeed-untag (before elfeed-backends-untag (entry &rest tags))
  "Override `elfeed-untag' to sync unread/starred states to remote server if necessary."
  (unless elfeed-backends-ignore-tag-action
    (let* ((current (elfeed-entry-tags entry))
           (remove-unread (memq 'unread tags))
           (remove-star (memq elfeed-ocnews-star-tag tags))
           (unread-modified (and remove-unread (memq 'unread current)))
           (star-modified (and remove-star
                               (memq elfeed-ocnews-star-tag current))))
      (when (elfeed-ocnews-is-ocnews-entry entry)
        (when unread-modified
          (elfeed-ocnews-mark-read entry))
        (when star-modified
          (elfeed-ocnews-mark-unstar entry)))
    (apply 'elfeed-untag-1 entry tags))))

(defadvice elfeed-search-fetch (around elfeed-backends-search-fetch (prefix))
  "Override `elfeed-search-fetch'. Skip elfeed-search-fetch-visible operation
for elfeed-backends."
  (if (eq elfeed-feed-source 'local)
      ad-do-it
    (elfeed-update)))

(defadvice elfeed-search-yank (around elfeed-backends-search-yank)
  "Overrid `elfeed-search-yank'. Sync multiple entries tags to
backends server in one operation"
  (let ((elfeed-backends-ignore-tag-action t))
    (elfeed-ocnews-sync-tag-multi (elfeed-search-selected) 'unread 'remove)
    ad-do-it))

(defadvice elfeed-search-tag-all (around elfeed-backends-search-tag-all (tag))
  "Overrid `elfeed-search-tag-all'. Sync multiple entries tags to
backends server in one operation"
  (let ((elfeed-backends-ignore-tag-action t))
    (elfeed-ocnews-sync-tag-multi (elfeed-search-selected) tag 'add)
    ad-do-it))

(defadvice elfeed-search-untag-all (around elfeed-backends-search-untag-all (tag))
  "Overrid `elfeed-search-untag-all'. Sync multiple entries tags to
backends server in one operation"
  (let ((elfeed-backends-ignore-tag-action t))
    (elfeed-ocnews-sync-tag-multi (elfeed-search-selected) tag 'remove)
    ad-do-it))

(defadvice elfeed-search-toggle-all (around elfeed-backends-search-toggle-all (tag))
  "Overrid `elfeed-search-toggle-all'. Sync multiple entries tags to
backends server in one operation"
  (let ((elfeed-backends-ignore-tag-action t))
    (elfeed-ocnews-sync-tag-multi (elfeed-search-selected) tag 'toggle)
    ad-do-it))

;;;###autoload
(defun elfeed-backends-enable ()
  "Enable elfeed-backends."
  (interactive)
  (advice-add 'elfeed-update :around #'elfeed-backends-update)
  (advice-add 'elfeed-tag :before #'elfeed-backends-tag)
  (advice-add 'elfeed-untag :before #'elfeed-backends-untag)
  (advice-add 'elfeed-search-fetch :around #'elfeed-backends-search-fetch)
  (advice-add 'elfeed-search-yank :around #'elfeed-backends-search-yank)
  (advice-add 'elfeed-search-tag-all :around #'elfeed-backends-search-tag-all)
  (advice-add 'elfeed-search-untag-all :around #'elfeed-backends-search-untag-all)
  (advice-add 'elfeed-search-toggle-all :around #'elfeed-backends-search-toggle-all)
  )

(defun elfeed-backends-disable ()
  "Disable elfeed-backends."
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
