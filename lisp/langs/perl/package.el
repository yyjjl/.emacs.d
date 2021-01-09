;; -*- lexical-binding:t -*-

;; perltidy
;; Devel::REPL

(defalias 'perl-mode 'cperl-mode)

(executable! perltidy)

(defvar ymacs-perl-perltidy-options nil)
(defvar ymacs-perl-shell-path (expand-etc! "scripts/perli"))
