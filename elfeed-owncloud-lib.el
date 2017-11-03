;;; elfeed-owncloud-lib.el --- Provide ownCloud News protocol for elfeed -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'elfeed)

(defun elfeed-owncloud-proto-id-generic (proto-type url)
  "Remove password filed in url and build a protocol url as id."
  (concat proto-type "+" (elfeed-owncloud-no-password-url url)))

(defun elfeed-owncloud-proto-id (url)
  (elfeed-owncloud-proto-id-generic "owncloud" url))

(defun elfeed-owncloud-no-password-url (url)
  "Remove password field in url is exists, user field will not change."
  (let* ((urlobj (url-generic-parse-url url))
         (password (url-password urlobj)))
    (if password
        (replace-regexp-in-string
         (concat "\\(" ":" (regexp-quote password) "\\).*\\'") "" url nil nil 1)
      url)))

(defun elfeed-owncloud-no-auth-url (url)
  "Remove user and password fields in url is exists."
  (let* ((urlobj (url-generic-parse-url url))
         (user (url-user urlobj))
         (password (if (url-password urlobj) (url-password urlobj) "")))
    (if password
        (replace-regexp-in-string
         (concat "\\(" (regexp-quote user) "\\(:" (regexp-quote password)
                 "\\)?@\\).*\\'") "" url nil nil 1)
      url)))

(defun elfeed-owncloud-entry-protocol-id (entry)
  "Return protocol url id for specific ENTRY."
  (elfeed-meta entry :protocol-id))

(defun elfeed-owncloud-format-entry-feed-id (proto-id feed-url)
  "Build feed id for entries."
  (concat proto-id "::" feed-url))

(defun elfeed-owncloud-proto-host-url (url)
  "Get host url from the feed id style url."
  (let ((list (split-string url "::")))
    (if (eq 2 (length list))
        (elt list 0)
      url)))

(defun elfeed-owncloud-proto-feed-url (url)
  "Get feed url from the feed id style url."
  (let ((list (split-string url "::")))
    (when (eq 2 (length list))
      (elt list 1))))

(defun elfeed-owncloud-proto-id-to-feed (proto-id)
  "Get related feed object in `elfeed-feeds' through the protocol id."
  (catch 'found
    (dolist (feed elfeed-feeds)
      (let* ((feed-url (cl-typecase feed
                          (list (when (stringp (car feed)) (car feed)))
                          (string feed)))
             (feed-proto-id (elfeed-owncloud-no-password-url feed-url)))
        (when (string-match (concat "^" (regexp-quote feed-proto-id)) proto-id)
          (throw 'found feed))))))

(defun elfeed-owncloud-proto-id-to-url (proto-id)
  "Get related feed protocol url in `elfeed-feeds' through the protocol id."
  (let* ((feed (elfeed-owncloud-proto-id-to-feed proto-id))
         (feed-url (cl-typecase feed
                     (list (when (stringp (car feed)) (car feed)))
                     (string feed))))
    feed-url))

(defun elfeed-owncloud-feed-autotags (proto-id url-or-feed)
  "Return tags to automatically apply to all entries from URL-OR-FEED under
special protocol feed. Similar with `elfeed-feed-autotags' but `elfeed-feeds'
overrode by `:autotags' item in protocol properties."
  (let* ((feed (elfeed-owncloud-proto-id-to-feed proto-id))
         (proto-props (when (listp feed) (cdr feed)))
         (elfeed-feeds (plist-get proto-props :autotags)))
    (elfeed-feed-autotags url-or-feed)))

(defun elfeed-owncloud-proto-feed-list ()
  (cl-loop for url in (elfeed--shuffle (elfeed-feed-list))
           when (elfeed-protocol-feed-p url) collect url))

(provide 'elfeed-owncloud-lib)

;;; elfeed-owncloud-lib.el ends here
