;;; -*- lexical-binding: t; -*-

(add-hook 'after-init-idle-hook #'ymacs-hideshow/persistent-mode)

(after! hideshow
  (advice-add #'goto-line :after #'ymacs-hideshow//auto-expand)
  (advice-add #'find-tag :after #'ymacs-hideshow//auto-expand))
