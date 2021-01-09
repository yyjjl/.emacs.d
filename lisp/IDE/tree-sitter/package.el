;;; -*- lexical-binding: t; -*-

(require-packages! tree-sitter-langs)

(after! tree-sitter
  (require 'tree-sitter-langs)

  (ymacs-editor//add-toggles
   "Global" t
   '("H" (if tree-sitter-hl-mode
             (tree-sitter-mode -1)
           (tree-sitter-hl-mode 1))
     "Tree Sitter" :toggle tree-sitter-hl-mode)))

(define-hook! ymacs-tree-sitter|setup (prog-mode-hook)
  (when (buffer-enable-rich-feature-p)
    (ignore-errors (tree-sitter-hl-mode 1))))

(defun ymacs-tree-sitter/toggle ()
  (interactive)
  (if (memq 'ymacs-tree-sitter|setup prog-mode-hook)
      (progn
        (remove-hook 'prog-mode-hook #'ymacs-tree-sitter|setup)
        (message "TreeSitter is disabled globally"))
    (add-hook 'prog-mode-hook #'ymacs-tree-sitter|setup)
    (message "TreeSitter is enabled globally")))
