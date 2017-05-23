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
;;   (setq elfeed-methods elfeed-sources-ocnews-methods)
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

(provide 'elfeed-sources)

;;; elfeed-sources.el ends here
