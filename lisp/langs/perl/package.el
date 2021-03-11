;; -*- lexical-binding:t -*-

;; perltidy
;; Devel::REPL

(defalias 'perl-mode 'cperl-mode)

(executable! perltidy)

(defvar ymacs-perl-perltidy-options nil)
(defvar ymacs-perl-shell-path (expand-etc! "scripts/perli"))

(eval-when-has-feature! debug
  (add-to-list 'ymacs-debugger-alist '(cperl-mode perldb :gud t)))

(after! cperl-mode
  (advice-add 'cperl-indent-command :around #'indent-for-tab-command@smart)

  (define-key! :map cperl-mode-map
    ("C-c C-b" . ymacs-perl/perltidy-format)))
