(require-packages! ispell flyspell)

(autoload 'hydra-flyspell/body "autoloads/spelling" nil t)

(defun spelling//detect-ispell-args (&optional -run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words"
  (when (and (bound-and-true-p ispell-program-name)
             (string-match "aspell$" ispell-program-name))
    (let ((args '("--sug-mode=ultra" "--lang=en_US")))
      (if -run-together
          (append args '("--run-together"
                         "--run-together-limit=16"
                         "--run-together-min=2"))
        args))))

(config! ispell
  :advice
  (:around ispell-word
   :use set-extra-args
   :define (-fn &rest -args)
   (let ((old-ispell-extra-args ispell-extra-args))
     (ispell-kill-ispell t)
     ;; Use emacs original arguments
     ;; Donot use together
     (setq ispell-extra-args (spelling//detect-ispell-args))
     (apply -fn -args)
     ;; Restore our own ispell arguments
     (setq ispell-extra-args old-ispell-extra-args)
     (ispell-kill-ispell t)))

  :config
  (cond
   (emacs-use-aspell-p
    (setq ispell-program-name "aspell"))
   (emacs-use-hunspell-p
    (setq ispell-program-name "hunspell")
    ;; Just reset dictionary to the safe one "en_US" for hunspell.  if
    ;; we need use different dictionary, we specify it in command line
    ;; arguments
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
   (t (setq ispell-program-name nil)))

  ;; `ispell-extra-args' is the command arguments which will *always*
  ;; be used when start ispell process
  (setq-default ispell-extra-args (spelling//detect-ispell-args t)))

(config! flyspell
  :advice
  (:around flyspell-auto-correct-word :name ispell*around-set-extra-args)
  :config
  (setq flyspell-large-region 1)
  ;; Better performance
  (setq flyspell-issue-message-flag nil))

(global-set-key (kbd "C-\"") #'hydra-flyspell/body)

(provide 'init-spelling)
