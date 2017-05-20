;;; elfeed-backends.el --- Provide extra bakcends to make such like ownCloud News works with elfeed -*- lexical-binding: t; -*-

;; Author: Xu Fasheng <fasheng.xu@gmail.com>
;; URL: https://github.com/fasheng/elfeed-backends
;; Version: 0.3.2
;; Package-Version: 20170501.1349
;; Package-Requires : ((elfeed "2.1.1") (cl-lib "0.5"))
;; Keywords: elfeed

;;; Commentary:

;; elfeed-backends provide extra backends to make self-hosting RSS
;; readers like ownCloud News works with elfeed. See the README for
;; full documentation.
;;
;; Usage:
;;
;;   (require 'elfeed-backends)
;;   (elfeed-backends-enable 'ocnews)
;;   (setq elfeed-backends-ocnews-url "http://127.0.0.1:8080")
;;   (setq elfeed-backends-ocnews-username "user")
;;   (setq elfeed-backends-ocnews-password "password")
;;   (setq elfeed-use-curl t)
;;   (elfeed-set-timeout 36000)
;;   (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-backends-ocnews)

(defgroup elfeed-backends ()
  "Provide extra backends for elfeed."
  :group 'comm)

(defun elfeed-backends-on-tag-add (entries tags)
  "Dispatch for tags added."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
      (cond
       ((eq elfeed-update-function 'elfeed-backends-ocnews-update)
        (elfeed-backends-ocnews-sync-tag-multi entries-modified tag 'add))))))

(defun elfeed-backends-on-tag-remove (entries tags)
  "Dispatch for tags removed."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry)))
      (cond
       ((eq elfeed-update-function 'elfeed-backends-ocnews-update)
        (elfeed-backends-ocnews-sync-tag-multi entries-modified tag 'remove))))))

;;;###autoload
(defun elfeed-backends-enable (backend)
  "Enable hooks and advices for elfeed-backends. BACKEND could be
ocnews(ownCloud News backend)."
  (interactive)
  (setq elfeed-update-function (cond
                                ((eq backend 'ocnews)
                                 'elfeed-backends-ocnews-update)))
  (add-hook 'elfeed-tag-hooks 'elfeed-backends-on-tag-add)
  (add-hook 'elfeed-untag-hooks 'elfeed-backends-on-tag-remove))

;;;###autoload
(defun elfeed-backends-disable ()
  "Disable hooks and advices elfeed-backends."
  (interactive)
  (setq elfeed-update-function 'elfeed-update-func-default)
  (remove-hook 'elfeed-tag-hooks 'elfeed-backends-on-tag-add)
  (remove-hook 'elfeed-untag-hooks 'elfeed-backends-on-tag-remove))

(provide 'elfeed-backends)

;;; elfeed-backends.el ends here
