(eval-when-compile
  (require 'cl)
  (require 'cl-lib)
  (require 'subr-x))

(defsubst env|expand-var (name)
  (expand-file-name name (expand-file-name "var" user-emacs-directory)))
(defsubst env|expand-etc (name)
  (expand-file-name name (expand-file-name "etc" user-emacs-directory)))

(defvar env|all-variables nil)
(defvar env|current-layer nil)

(defun env|write-file (file)
  (setq file (expand-file-name file (file-name-directory load-file-name)))
  (let ((backup-enable-predicate (lambda (name) nil)))
    (with-temp-buffer
      (dolist (declaration env|all-variables)
        (insert (pp-to-string `(defvar ,@declaration))))
      (insert (format "\n\n(provide '%s)\n" (file-name-base file)))
      (write-file file)
      (byte-compile-file file))))

(cl-defmacro var (var-sym &key (value t) (doc ""))
  `(progn
     (setq ,var-sym ,value)
     (message "`%s' => %s" ',var-sym ,var-sym)
     (add-to-list 'env|all-variables (list ',var-sym ,var-sym ,doc))))

(defun find-library-in-directory (name dir)
  (setq dir (expand-file-name dir))
  (let ((files (directory-files dir))
        file
        lib-path)
    (while (and (not lib-path) files)
      (setq file (pop files))
      (unless (member file '("." ".."))
        (setq lib-path
              (if (file-directory-p file)
                  (ignore-errors (find-library-in-directory name file))
                (and (string= file name)
                     (expand-file-name file dir))))))))

(defun utf8-locale-p (v)
  "Return whether locale string V relates to a utf-8 locale."
  (and v (string-match "utf-8" v)))

(defun python-has-module (modules &optional script)
  "Check python whether has MODULES"
  (shell-command-to-string
   (format "python3 %s %s"
           (or script (env|expand-etc "has-module.py"))
           (string-join (mapcar (lambda (x)
                                  (if (symbolp x) (symbol-name x) x)) modules)
                        " "))))

(provide 'env-defs)
