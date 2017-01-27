(defun after-init-function ()
  (session-initialize)
  ;; (turnon-keyfreq-mode)

  (ivy-mode 1)
  (counsel-mode 1)

  (which-key-mode)

  (popwin-mode 1)  ;; enable popwin-mode

  (global-git-gutter-mode 1)
  (global-linum-mode 1)
  (global-company-mode 1)
  (global-flycheck-mode 1)
  (global-subword-mode 1)
  (global-hi-lock-mode 1)
  (global-auto-revert-mode)
  (column-number-mode 1)
  (global-page-break-lines-mode)

  ;; make zsh work correctly in emacs
  (setq system-uses-terminfo nil)
  (fcitx-aggressive-setup)

  ;; may take a long time
  (elpy-enable)

  (winner-mode 1)
  (message "Emacs setup time: %s" (emacs-init-time))
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
  (bind-keys ("C-x C-m" . execute-extended-command)
             ("<backtab>" . company-complete)
             ("C-r" . isearch-backward-regexp)
             ("C-M-r" . isearch-backeard)
             ("C-c c" . comment-region)
             ("C-c u" . uncomment-region)
             ("C-x R" . rename-this-file-and-buffer)
             ("C-x D" . delete-this-file)
             ("C-x c" . cleanup-buffer-safe)
             ("C-=" . text-scale-increase)
             ("C--" . text-scale-decrease)
             ;; flyspell
             ("C-c s" . flyspell-auto-correct-word)
             ("C-c 4" . ispell-word)
             ("C-c q" . auto-fill-mode)
             ("C-x F" . ffip)
             ("C-x C-b" . ibuffer)

             ;; narrow
             ("C-x , ," . narrow-to-defun)
             ("C-x , SPC" . widen)
             ("C-x , p" . narrow-to-page)
             ("C-x , r" . narrow-to-region)
             ("C-x m" . show-messages-buffer)
             ("M-/" . hippie-expand)
             ("RET" . newline-and-indent)
             ("M-'" . tiny-expand)
             ("M--" . er/expand-region)
             ("C-?" . goto-last-change)
             ([f6] . toggle-company-ispell)
             ([f7] . create-scratch-buffer)))

(add-hook 'after-init-hook 'after-init-function)

(provide 'after-init)
