(cond
 (spelling-has-aspell-p
  (setq ispell-program-name "aspell"))
 (spelling-has-hunspell-p
  (setq ispell-program-name "hunspell")
  ;; Just reset dictionary to the safe one "en_US" for hunspell.  if
  ;; we need use different dictionary, we specify it in command line
  ;; arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)
    (message "You need install either aspell or hunspell for ispell")))

(with-eval-after-load 'ispell
  ;; if (aspell installed) { use aspell}
  ;; else if (hunspell installed) { use hunspell }
  (defun spelling%detect-ispell-args (&optional $run-together)
    "If RUN-TOGETHER is true, spell check the CamelCase words"
    (when (and (bound-and-true-p ispell-program-name)
               (string-match "aspell$" ispell-program-name))
      (let ((args '("--sug-mode=ultra" "--lang=en_US")))
        (if $run-together
            (append args '("--run-together"
                           "--run-together-limit=16"
                           "--run-together-min=2"))
          args))))

  ;; `ispell-extra-args' is the command arguments which will *always*
  ;; be used when start ispell process
  (setq-default ispell-extra-args (spelling%detect-ispell-args t))

  (defun spelling*set-extra-args ($fn &rest $args)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      ;; Use emacs original arguments
      ;; Donot use together
      (setq ispell-extra-args (spelling%detect-ispell-args))
      (apply $fn $args)
      ;; Restore our own ispell arguments
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))
  (advice-add 'ispell-word :around #'spelling*set-extra-args)
  (advice-add 'flyspell-auto-correct-word :around #'spelling*set-extra-args))

(with-eval-after-load 'flyspell
  (defun spelling*turn-off-fly ()
    (remove-hook 'post-command-hook (function flyspell-post-command-hook) t)
    (remove-hook 'pre-command-hook (function flyspell-pre-command-hook) t)
    (remove-hook 'after-change-functions 'flyspell-after-change-function t)
    (remove-hook 'hack-local-variables-hook
                 #'flyspell-hack-local-variables-hook t))
  (advice-add 'flyspell-mode-on :after #'spelling*turn-off-fly)

  ;; flyspell set up for web-mode
  (defun web|flyspell-verify ()
    (let ((f (get-text-property (- (point) 1) 'face)))
      (not (memq f '(web-mode-html-attr-value-face
                     web-mode-html-tag-face
                     web-mode-html-attr-name-face
                     web-mode-constant-face
                     web-mode-doctype-face
                     web-mode-keyword-face
                     web-mode-comment-face ;; focus on get html label right
                     web-mode-function-name-face
                     web-mode-variable-name-face
                     web-mode-css-property-name-face
                     web-mode-css-selector-face
                     web-mode-css-color-face
                     web-mode-type-face
                     web-mode-block-control-face)))))
  (put 'web-mode 'flyspell-mode-predicate 'web|flyspell-verify)

  ;;  `flyspell' setup for js2-mode
  (defun js2|flyspell-verify ()
    (let* ((f (get-text-property (- (point) 1) 'face)))
      ;; Only words with following font face will be checked
      (memq f '(js2-function-call
                js2-function-param
                js2-object-property
                font-lock-variable-name-face
                font-lock-string-face
                font-lock-function-name-face
                font-lock-builtin-face
                rjsx-tag
                rjsx-attr))))
  (put 'js2-mode 'flyspell-mode-predicate 'js2|flyspell-verify)

  (setq flyspell-issue-message-flag nil
        flyspell-large-region 1)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-buffer))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(provide 'init-spelling)
