;; -*- lexical-binding:t -*-

(defalias 'perl-mode 'cperl-mode)

;; perltidy
;; Devel::REPL

(defvar perl-perltidy-path "perltidy")
(defvar perl-perltidy-options nil)
(defvar perl-shell-path (expand-etc! "bin/perli"))

(config! cperl-mode
  :bind
  (:map cperl-mode-map
   ("<tab>" . indent-for-tab-command)
   ("C-c b" . perl/perltidy-format)
   ("C-c C-z" . run-perl)
   ([f5] . perldb))

  :init
  (setq
   ;; highlight all scalar variables not just the instantiation
   cperl-highlight-variables-indiscriminately t
   ;; 4 spaces is the standard indentation
   cperl-indent-level 4
   ;; indent the closing paren back four spaces
   cperl-close-paren-offset -4
   ;; if a statement continues indent it to four spaces
   cperl-continued-statement-offset 4
   ;; parentheses are indented with the block and not with scope
   cperl-indent-parens-as-block t)

  :hook
  (setup
   :define (cperl-mode-hook)
   (when emacs-use-gtags-p
     (ggtags-mode 1)
     (setq ggtags-local-libpath (expand-var! "perl-modules"))))

  :config
  (font-lock-add-keywords 'cperl-mode
                          '(("\\_<say\\_>" . cperl-nonoverridable-face)))
  ;; Use less horrible colors for cperl arrays and hashes
  (set-face-attribute 'cperl-array-face nil
                      :foreground "#DD7D0A"
                      :background 'unspecified
                      :weight 'unspecified)
  (set-face-attribute 'cperl-hash-face nil
                      :foreground "OrangeRed3"
                      :background 'unspecified
                      :weight 'bold))

(provide 'init-perl5)
