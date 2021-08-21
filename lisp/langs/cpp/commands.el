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
