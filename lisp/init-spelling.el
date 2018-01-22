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
  (advice-add 'ispell-word :around #'spelling*set-extra-args))

(with-eval-after-load 'flyspell
  (setq flyspell-large-region 1)
  ;; Better performance
  (setq flyspell-issue-message-flag nil)

  (advice-add 'flyspell-auto-correct-word :around #'spelling*set-extra-args)

  (defun flyspell-goto-previous-error ()
    "Go to previous spelling error."
    (interactive)
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)))

  (defun flyspell-dwim (&optional $whole-buffer)
    "Check spelling manually."
    (interactive "p")
    (when-let ((mode-predicate
                (or (get major-mode 'flyspell-mode-predicate)
                    (and (derived-mode-p 'prog-mode)
                         'flyspell-generic-progmode-verify))))
      (setq flyspell-generic-check-word-predicate mode-predicate))
    (cond ($whole-buffer
           (call-interactively #'flyspell-buffer))
          ((region-active-p)
           (flyspell-region (region-beginning) (region-end)))
          (t
           (flyspell-region (line-beginning-position)
                            (line-end-position))))))

(defhydra hydra-flyspell
  (:color blue :hint nil
          :body-pre
          (unless (featurep 'flyspell)
            (require 'flyspell nil :no-error)))
  "
_SPC_ line-or-region _b_ buffer  _p_ previous-error _n_ next-error _q_ quit
"
  ("b" (flyspell-dwim :whole-buffer) :exit nil)
  ("SPC" (flyspell-dwim) :exit nil)
  ("p" flyspell-goto-previous-error :exit nil)
  ("n" flyspell-goto-next-error :exit nil)
  ("q" nil :exit t)
  ("RET" nil :exit t))

(global-set-key (kbd "C-\"") #'hydra-flyspell/body)

(provide 'init-spelling)
