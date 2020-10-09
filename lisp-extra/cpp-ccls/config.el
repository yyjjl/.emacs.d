;; -*- lexical-binding:t -*-

(after! ccls
  (define-key! :map c++-mode-map
    ("M-s c" . ccls-call-hierarchy)
    ("M-s m" . ccls-member-hierarchy)
    ("M-s i" . ccls-inheritance-hierarchy)
    ("C-c j" :map ymacs-cpp-ccls-jump-map))

  (ymacs-hydra-add-toggle-column
   '((and lsp-mode (derived-mode-p 'c-mode 'c++-mode))
     "CCLS"
     (("h n" (progn
               (ccls--clear-sem-highlights)
               (setq ccls-sem-highlight-method nil))
       "no sem-highlight"
       :toggle (not ccls-sem-highlight-method))
      ("h o" (progn
               (ccls--clear-sem-highlights)
               (setq ccls-sem-highlight-method 'overlay))
       "sem-highlight with overlays"
       :toggle (eq ccls-sem-highlight-method 'overlay))
      ("h f" (progn
               (ccls--clear-sem-highlights)
               (setq ccls-sem-highlight-method 'font-lock))
       "sem-highlight with font-lock"
       :toggle (eq ccls-sem-highlight-method 'font-lock)))))

  (aset ccls-sem-macro-faces 0 'font-lock-builtin-face)
  (aset ccls-sem-variable-faces 0 'cpp-variable-face)

  (setq ccls-executable ymacs-ccls-path)
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (setq ccls-sem-highlight-method 'font-lock))

(after! ccls-tree
  (define-key! :map ccls-tree-mode-map
    ("o" . ccls-tree-press)
    ("." . ccls-tree-expand-or-set-root)
    ("^" . ccls-tree-collapse-or-select-parent)
    ("j" . next-line)
    ("k" . previous-line)))

(after! ccls
  (ymacs-lsp//set-simple-install-fn
   'ccls
   (expand-etc! "setup/install_ccls.py")))
