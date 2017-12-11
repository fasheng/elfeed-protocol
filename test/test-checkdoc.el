(add-to-list 'load-path default-directory)

(defvar elfeed-protocol-sources (file-expand-wildcards "*.el"))

;; test byte-comple
;; (mapc #'byte-compile-file `(,cdnjs-el))

;; test checkdoc
(dolist (source elfeed-protocol-sources)
  (with-current-buffer (find-file-noselect source)
    (let ((checkdoc-diagnostic-buffer "*warn*"))
      (checkdoc-current-buffer t))))
