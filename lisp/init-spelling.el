(cond
 (spelling|has-aspell-p
  (setq ispell-program-name "aspell"))
 (spelling|has-hunspell-p
  (setq ispell-program-name "hunspell")
  ;; Just reset dictionary to the safe one "en_US" for hunspell.  if
  ;; we need use different dictionary, we specify it in command line
  ;; arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)
    (message "You need install either aspell or hunspell for ispell")))

(with-eval-after-load 'flyspell
  (defhook spelling|disable-on-the-fly (flyspell-mode-hook)
    "Do not check spelling when typing."
    (remove-hook 'post-command-hook #'flyspell-post-command-hook t)
    (remove-hook 'pre-command-hook #'flyspell-pre-command-hook t)
    (remove-hook 'after-change-functions #'flyspell-after-change-function t))

  (setq flyspell-large-region 1)
  (defun flyspell-dwim ()
    "Check spelling manually."
    (interactive)
    (if (region-active-p)
        (flyspell-region (region-beginning) (region-end))
      (flyspell-region (line-beginning-position)
                       (line-end-position))))

  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-dwim)
  ;; Better performance
  (setq flyspell-issue-message-flag nil)

  ;; if (aspell installed) { use aspell}
  ;; else if (hunspell installed) { use hunspell }
  (defun spelling|detect-ispell-args (&optional RUN-TOGETHER)
    "If RUN-TOGETHER is true, spell check the CamelCase words"
    (when (and (bound-and-true-p ispell-program-name)
               (string-match "aspell$" ispell-program-name))
      (let ((args '("--sug-mode=ultra" "--lang=en_US")))
        (if RUN-TOGETHER
            (append args '("--run-together"
                           "--run-together-limit=16"
                           "--run-together-min=2"))
          args))))

  ;; `ispell-extra-args' is the command arguments which will *always*
  ;; be used when start ispell process
  (setq-default ispell-extra-args (spelling|detect-ispell-args t))

  (defun spelling|set-extra-args (orig-fun &rest args)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      ;; Use emacs original arguments
      ;; Donot use together
      (setq ispell-extra-args (spelling|detect-ispell-args))
      (apply orig-fun args)
      ;; Restore our own ispell arguments
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))
  (advice-add 'ispell-word :around #'spelling|set-extra-args)
  (advice-add 'flyspell-auto-correct-word :around #'spelling|set-extra-args))

;; Add auto spell-checking in comments and strings for all programming
;; language modes
(when (bound-and-true-p ispell-program-name)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(provide 'init-spelling)
