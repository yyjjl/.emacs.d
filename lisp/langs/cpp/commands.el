;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-cpp/electric-star (-arg)
  (interactive "*P")
  (if (eq (char-before) ?\/)
      (progn
        (self-insert-command (prefix-numeric-value -arg))
        (insert "  */")
        (backward-char 3)
        (indent-according-to-mode))
    (call-interactively 'self-insert-command)))

;;;###autoload
(defun ymacs-cpp/get-include-paths ()
  (interactive)
  (message "%s" (shell-command-to-string (expand-etc! "scripts/C_include_paths"))))

;;;###autoload
(defun ymacs-cpp/format-dwim (-whole-buffer)
  (interactive "P")
  (cond
   (-whole-buffer
    (lsp-format-buffer))
   ((region-active-p)
    (lsp-format-region (region-beginning) (region-end)))
   (t
    (if-let (bounds (or (bounds-of-thing-at-point 'sexp)
                        (bounds-of-thing-at-point 'defun)))
        (lsp-format-region (car bounds) (cdr bounds))
      (message "No sexp or defun found, please use C-u prefix")))))


(defun ymacs-cpp//select-clangd-by-version ()
  (interactive)
  (let (versions)
    (dolist (path (directory-files (expand-cache! "lsp/clangd") :full))
      (let ((basename (file-name-nondirectory path)))
        (unless (or (equal basename ".")
                    (equal basename "..")
                    (not (file-directory-p path)))
          (let ((parts (split-string basename "_")))
            (when (and (equal (length parts) 2)
                       (equal (nth 0 parts) "clangd"))
              (push (nth 1 parts) versions))))))
    (custom-set-variables
     `(lsp-clangd-version ,(completing-read! "Select clangd version:" (nreverse (sort versions #'string<)))))
    (custom-save-all)))


;;;###autoload
(defun ymacs-cpp/update-clangd ()
  (interactive)
  (let ((default-directory (expand-etc! "setup")))
    (run-compilation!
     :name "update-clangd"
     :command "python3 update_clangd.py"
     :callback
     (lambda (_)
       (ymacs-cpp//select-clangd-by-version))
     :error-callback
     (lambda (msg)
       (message "ERROR %s" msg)))))
