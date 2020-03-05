(load-file "elfeed-protocol-fever.el")
(load-file "elfeed-protocol-newsblur.el")
(load-file "elfeed-protocol-owncloud.el")
(load-file "elfeed-protocol-ttrss.el")
(load-file "elfeed-protocol.el")

(elfeed-protocol-enable)

;;; copy from elfeed/tests/elfeed-db-tests.el
(defmacro with-elfeed-test (&rest body)
  "Run BODY with a fresh, empty database that will be destroyed on exit."
  (declare (indent defun))
  `(let* ((elfeed-db nil)
          (elfeed-db-feeds nil)
          (elfeed-db-entries nil)
          (elfeed-db-index nil)
          (elfeed-feeds nil)
          (temp-dir (make-temp-file "elfeed-test-" t))
          (elfeed-db-directory temp-dir)
          (elfeed-new-entry-hook nil)
          (elfeed-db-update-hook nil)
          (elfeed-log-level 'debug)
          (elfeed-initial-tags '(unread)))
     (when (get-buffer elfeed-log-buffer-name)
       (kill-buffer elfeed-log-buffer-name))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir :recursive))))

(defmacro with-fixture (fixture-path &rest body)
  "Run BODY with the contents of the file specified by
FIXTURE-PATH inserted into a temporary buffer."
  (declare (indent defun))
  `(with-temp-buffer
     (insert-file-contents ,fixture-path)
     (goto-char (point-min))
     ,@body))

(defun dump-elfeed-log ()
  "Print *elfeed-log* buffer content."
  (with-current-buffer elfeed-log-buffer-name (message "%s" (buffer-string))))
