(eval-when-compile
  (require 'cl)
  (require 'cl-lib)
  (require 'subr-x))

(defsubst env|expand-var (name)
  (expand-file-name name (expand-file-name "var" user-emacs-directory)))
(defsubst env|expand-etc (name)
  (expand-file-name name (expand-file-name "etc" user-emacs-directory)))

(defvar env|all-docs (make-hash-table))
(defvar env|all-variables (make-hash-table))
(defvar env|current-layer nil)

(defun env|open ()
  (clrhash env|all-docs)
  (clrhash env|all-variables)
  (setq env|current-layer nil))

(defun env|close (file)
  (setq file (expand-file-name file (file-name-directory load-file-name)))
  (let ((backup-enable-predicate (lambda (name) nil)))
    (with-temp-buffer
      (maphash (lambda (layer-name layer-doc)
                 (setq layer-doc (car layer-doc))
                 (insert (format ";; For '%s'\n" layer-name))
                 (insert (env|format-message layer-doc) "\n")
                 (dolist (var (gethash layer-name env|all-variables))
                   (insert (pp-to-string `(defvar ,@var)))))
               env|all-docs)
      (insert (format "\n\n(provide '%s)\n" (file-name-base file)))
      (write-file file)
      (byte-compile-file file))))

(cl-defmacro env|layer ((name &optional doc-string) &rest body)
  (when (stringp name) (setq name (intern name)))
  (setq env|current-layer name)
  (env|put doc-string env|all-docs)
  `(progn ,@body))

(cl-defun env|wrap-name
    (name &optional (prefix "") (suffix "") (layer env|current-layer))
  (intern (format "%s|%s%s%s" layer prefix name suffix)))

(cl-defun env|put (v table &optional (k env|current-layer))
  (let ((val (gethash k table)))
    (puthash k (if val (cons v val) `(,v)) table)))

(defun env|format-message (msg)
  (if msg (concat ";; " (replace-regexp-in-string
                         "\n" "\n;; "
                         (string-trim-right msg)))
    ""))

(cl-defmacro ref (name &optional (layer env|current-layer))
  (env|wrap-name name "" "" layer))

(cl-defmacro var (name form &optional doc)
  (let ((var-sym (env|wrap-name name)))
    `(progn
       (setq ,var-sym ,form)
       (message "`%s' => %s" ',var-sym ,var-sym)
       (env|put (list ',var-sym ,var-sym ,doc) env|all-variables))))

(provide 'env-defs)
