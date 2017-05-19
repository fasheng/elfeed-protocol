;;; elfeed-backends-tests.el --- tests for elfeed -*- lexical-binding: t; -*-

;; emacs -batch -Q -L .. -L . -l elfeed-backends-tests.el -f ert-run-tests-batch

(require 'ert)
(require 'elfeed)
(require 'elfeed-backends-ocnews-tests)

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

(provide 'elfeed-backends-tests)

;;; elfeed-backends-tests.el ends here
