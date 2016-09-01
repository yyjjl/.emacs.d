;; I use ccl, `C-h v slime-read-interactive-args RET` for details
;; you need install the programy ccl, of cours
(setq inferior-lisp-program "sbcl")

(with-eval-after-load 'slime
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-complete-symbol*-fancy t)
  (slime-setup '(slime-fancy slime-company))
  (local-set-key (kbd "C-'") 'counsel-cl))

(provide 'init-slime)
