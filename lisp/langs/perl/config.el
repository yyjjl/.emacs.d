;; -*- lexical-binding:t -*-

(after! cperl-mode
  (advice-add 'cperl-indent-command :around #'indent-for-tab-command@smart)

  (define-key! :map cperl-mode-map
    ("C-c C-b" . ymacs-perl/perltidy-format)))
