(load-file "elfeed-owncloud.el")
(load-file "elfeed-protocols.el")

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
          (elfeed-initial-tags '(unread)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir :recursive))))
