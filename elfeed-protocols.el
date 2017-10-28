;;; elfeed-protocol.el --- Provide extra bakcends to make such like ownCloud News works with elfeed -*- lexical-binding: t; -*-

;; Author: Xu Fasheng <fasheng.xu@gmail.com>
;; URL: https://github.com/fasheng/elfeed-protocol
;; Version: 0.3.2
;; Package-Version: 20170501.1349
;; Package-Requires : ((elfeed "2.1.1") (cl-lib "0.5"))
;; Keywords: elfeed

;;; Commentary:

;; elfeed-protocol provide extra protocol to make self-hosting RSS
;; readers like ownCloud News works with elfeed. See the README for
;; full documentation.
;;
;; Usage: TODO
;;
;;   ;; curl recommend
;;   (setq elfeed-use-curl t)
;;   (elfeed-set-timeout 36000)
;;   (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate
;;
;;   ;; setup owncloud news as source
;;   (require 'elfeed-protocol)
;;   (setq elfeed-protocols-type 'owncloud)
;;   (setq elfeed-owncloud-url "http://127.0.0.1:8080")
;;   (setq elfeed-owncloud-username "user")
;;   (setq elfeed-owncloud-password "password")
;;   (elfeed-protocols-enable)

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-owncloud)

(defgroup elfeed-protocols ()
  "Provide extra protocol for elfeed."
  :group 'comm)

;; TODO
(defcustom elfeed-protocols-type 'owncloud
  "elfeed extra source type, could be owncloud. owncloud means the ownCloud News source."
  :group 'elfeed-protocols
  :type '(choice (const owncloud)))

(defun elfeed-protocols-on-tag-add (entries tags)
  "Dispatch for tags added."
  (cond
   ((eq elfeed-protocols-type 'owncloud)
    (apply #'elfeed-owncloud-pre-tag entries tags))))

(defun elfeed-protocols-on-tag-remove (entries tags)
  "Dispatch for tags removed."
  (cond
   ((eq elfeed-protocols-type 'owncloud)
    (apply #'elfeed-owncloud-pre-untag entries tags))))

(defun elfeed-protocols-update-advice ()
  "Advice for `elfeed-update' to make elfeed-protocol works."
  (interactive)
  (elfeed-log 'info "Elfeed update: %s"
              (format-time-string "%B %e %Y %H:%M:%S %Z"))
  (let ((elfeed--inhibit-update-init-hooks t))
    (cond
     ((eq elfeed-protocols-type 'owncloud) (elfeed-owncloud-update-all))))
  (run-hooks 'elfeed-update-init-hooks)
  (elfeed-db-save))

(defun elfeed-protocols-update-feed-advice (url)
  "Advice for `elfeed-update-feed' to make elfeed-protocol works."
  (interactive)
  (cond
   ((eq elfeed-protocols-type 'owncloud) (elfeed-owncloud-update-feed url)))
  (run-hook-with-args 'elfeed-update-hooks url))

;;;###autoload
(defun elfeed-protocols-enable ()
  "Enable hooks and advices for elfeed-protocol."
  (interactive)
  (advice-add 'elfeed-update :override #'elfeed-protocols-update-advice)
  (advice-add 'elfeed-update-feed :override #'elfeed-protocols-update-feed-advice)
  (add-hook 'elfeed-tag-hooks 'elfeed-protocols-on-tag-add)
  (add-hook 'elfeed-untag-hooks 'elfeed-protocols-on-tag-remove))

;;;###autoload
(defun elfeed-protocols-disable ()
  "Disable hooks and advices elfeed-protocol."
  (interactive)
  (advice-remove 'elfeed-update #'elfeed-protocols-update-advice)
  (advice-remove 'elfeed-update-feed #'elfeed-protocols-update-feed-advice)
  (remove-hook 'elfeed-tag-hooks 'elfeed-protocols-on-tag-add)
  (remove-hook 'elfeed-untag-hooks 'elfeed-protocols-on-tag-remove))

(provide 'elfeed-protocol)

;;; elfeed-protocol.el ends here
