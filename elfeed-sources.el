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
;;   (require 'elfeed-sources)
;;   (elfeed-sources-enable 'ocnews)
;;   (setq elfeed-sources-ocnews-url "http://127.0.0.1:8080")
;;   (setq elfeed-sources-ocnews-username "user")
;;   (setq elfeed-sources-ocnews-password "password")
;;   (setq elfeed-use-curl t)
;;   (elfeed-set-timeout 36000)
;;   (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-sources-ocnews)

(defun elfeed-sources-on-tag-add (entries &rest tags)
  "Dispatch for tags added."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
      (cond
       ((eq elfeed-update-function 'elfeed-sources-ocnews-update)
        (elfeed-sources-ocnews-sync-tag-multi entries-modified tag 'add))))))

(defun elfeed-sources-on-tag-remove (entries &rest tags)
  "Dispatch for tags removed."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry)))
      (cond
       ((eq elfeed-update-function 'elfeed-sources-ocnews-update)
        (elfeed-sources-ocnews-sync-tag-multi entries-modified tag 'remove))))))

;;;###autoload
(defun elfeed-sources-enable (source)
  "Enable hooks and advices for elfeed-sources. SOURCE could be
ocnews(ownCloud News source)."
  (interactive)
  (setq elfeed-update-function (cond
                                ((eq source 'ocnews)
                                 'elfeed-sources-ocnews-update)))
  (add-hook 'elfeed-tag-hooks 'elfeed-sources-on-tag-add)
  (add-hook 'elfeed-untag-hooks 'elfeed-sources-on-tag-remove))

;;;###autoload
(defun elfeed-sources-disable ()
  "Disable hooks and advices elfeed-sources."
  (interactive)
  (setq elfeed-update-function 'elfeed-update-func-default)
  (remove-hook 'elfeed-tag-hooks 'elfeed-sources-on-tag-add)
  (remove-hook 'elfeed-untag-hooks 'elfeed-sources-on-tag-remove))

(provide 'elfeed-sources)

;;; elfeed-sources.el ends here
