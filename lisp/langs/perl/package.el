;; -*- lexical-binding:t -*-

;; perltidy
;; Devel::REPL

(defalias 'perl-mode 'cperl-mode)

(executable! perltidy)

(defvar ymacs-perl-perltidy-options nil)
(defvar ymacs-perl-shell-path (expand-etc! "scripts/perli"))

(eval-when-has-feature! debug
  (add-to-list 'ymacs-debugger-alist '(cperl-mode perldb :gud t)))
