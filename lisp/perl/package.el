;; -*- lexical-binding:t -*-

(defalias 'perl-mode 'cperl-mode)

;; perltidy
;; Devel::REPL

(defvar ymacs-perl-perltidy-path "perltidy")
(defvar ymacs-perl-perltidy-options nil)
(defvar ymacs-perl-shell-path (expand-etc! "bin/perli"))
