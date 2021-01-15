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
