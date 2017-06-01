;;; elfeed-sources.el --- Provide extra bakcends to make such like ownCloud News works with elfeed -*- lexical-binding: t; -*-

;; Author: Xu Fasheng <fasheng.xu@gmail.com>
;; URL: https://github.com/fasheng/elfeed-sources
;; Version: 0.3.2
;; Package-Version: 20170501.1349
;; Package-Requires : ((elfeed "2.1.1") (cl-lib "0.5"))
;; Keywords: elfeed

;;; Commentary:

;; elfeed-sources provide extra sources to make self-hosting RSS
;; readers like ownCloud News works with elfeed. See the README for
;; full documentation.
;;
;; Usage:
;;
;;   ;; curl recommend
;;   (setq elfeed-use-curl t)
;;   (elfeed-set-timeout 36000)
;;   (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate
;;
;;   ;; setup owncloud news as source
;;   (require 'elfeed-sources)
;;   (setq elfeed-sources-type 'ocnews)
;;   (setq elfeed-sources-ocnews-url "http://127.0.0.1:8080")
;;   (setq elfeed-sources-ocnews-username "user")
;;   (setq elfeed-sources-ocnews-password "password")
;;   (elfeed-sources-enable)

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-sources-ocnews)

(defgroup elfeed-sources ()
  "Provide extra sources for elfeed."
  :group 'comm)

(defcustom elfeed-sources-type 'ocnews
  "elfeed extra source type, could be ocnews. ocnews means the ownCloud News source."
  :group 'elfeed-sources
  :type '(choice (const ocnews)))

(defun elfeed-sources-on-tag-add (entries tags)
  "Dispatch for tags added."
  (cond
   ((eq elfeed-sources-type 'ocnews)
    (apply #'elfeed-sources-ocnews-pre-tag entries tags))))

(defun elfeed-sources-on-tag-remove (entries tags)
  "Dispatch for tags removed."
  (cond
   ((eq elfeed-sources-type 'ocnews)
    (apply #'elfeed-sources-ocnews-pre-untag entries tags))))

(defun elfeed-sources-update-advice ()
  "Advice for `elfeed-update' to make elfeed-sources works."
  (interactive)
  (elfeed-log 'info "Elfeed update: %s"
              (format-time-string "%B %e %Y %H:%M:%S %Z"))
  (let ((elfeed--inhibit-update-init-hooks t))
    (cond
     ((eq elfeed-sources-type 'ocnews) (elfeed-sources-ocnews-update-all))))
  (run-hooks 'elfeed-update-init-hooks)
  (elfeed-db-save))

(defun elfeed-sources-update-feed-advice (url)
  "Advice for `elfeed-update-feed' to make elfeed-sources works."
  (interactive)
  (cond
   ((eq elfeed-sources-type 'ocnews) (elfeed-sources-ocnews-update-feed url)))
  (run-hook-with-args 'elfeed-update-hooks url))

;;;###autoload
(defun elfeed-sources-enable ()
  "Enable hooks and advices for elfeed-sources."
  (interactive)
  (advice-add 'elfeed-update :override #'elfeed-sources-update-advice)
  (advice-add 'elfeed-update-feed :override #'elfeed-sources-update-feed-advice)
  (add-hook 'elfeed-tag-hooks 'elfeed-sources-on-tag-add)
  (add-hook 'elfeed-untag-hooks 'elfeed-sources-on-tag-remove))

;;;###autoload
(defun elfeed-sources-disable ()
  "Disable hooks and advices elfeed-sources."
  (interactive)
  (advice-remove 'elfeed-update #'elfeed-sources-update-advice)
  (advice-remove 'elfeed-update-feed :override #'elfeed-sources-update-feed-advice)
  (remove-hook 'elfeed-tag-hooks 'elfeed-sources-on-tag-add)
  (remove-hook 'elfeed-untag-hooks 'elfeed-sources-on-tag-remove))

(provide 'elfeed-sources)

;;; elfeed-sources.el ends here
