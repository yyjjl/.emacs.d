;;; -*- lexical-binding: t; -*-

(add-hook 'after-init-hook #'ymacs-hideshow/persistent-mode)

(after! hideshow
  (advice-add #'goto-line :after #'ymacs-hideshow//auto-expand)
  (advice-add #'xref-find-definitions :after #'ymacs-hideshow//auto-expand))
