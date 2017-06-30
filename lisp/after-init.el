(defun after-init-function ()
  (session-initialize)

  (ivy-mode 1)
  (counsel-mode 1)
  (projectile-mode 1)
  (counsel-projectile-on)

  (which-key-mode)

  (yas-global-mode 1)

  ;; enable popwin-mode
  (popwin-mode 1)

  ;; global-modes
  (global-git-gutter-mode 1)
  (global-linum-mode 1)
  (global-company-mode 1)
  (global-flycheck-mode 1)
  (global-subword-mode 1)
  (global-hi-lock-mode 1)
  (global-auto-revert-mode 1)
  (column-number-mode 1)
  (global-page-break-lines-mode 1)
  ;; (global-whitespace-newline-mode 1)
  ;; make zsh work correctly in emacs
  (setq system-uses-terminfo nil)

  (when use-fcitx-setup-p
    (fcitx-aggressive-setup))

  (elpy-enable)
  (remove-hook  'python-mode-hook 'elpy-mode)

  (winner-mode 1)
  ;; modeline
  (window-numbering-mode 1)
  (setq-default mode-line-format
                '("%e" (:eval (generate-mode-line))))

  (message "Emacs setup time: %s" (emacs-init-time))
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur))

(add-hook 'after-init-hook 'after-init-function)

(defun comment-currnet-line (&optional arg)
  (save-excursion
    (beginning-of-line)
    (set-mark-command nil)
    (end-of-line)
    (if arg
        (call-interactively #'uncomment-region)
      (call-interactively #'comment-region))))

(defun comment-region-or-line ()
  (interactive "*")
  (if mark-active
      (call-interactively 'comment-region)
    (comment-currnet-line)))

(defun uncomment-region-or-line ()
  (interactive "*")
  (if mark-active
      (call-interactively 'uncomment-region)
    (comment-currnet-line t)))

(bind-keys ("C-}" . force-company-yasnippet)
           ("<backtab>" . company-complete)
           ("C-r" . isearch-backward-regexp)
           ("C-M-r" . isearch-backeard)
           ("C-c c" . comment-region-or-line)
           ("C-c u" . uncomment-region-or-line)
           ("C-x R" . rename-this-file-and-buffer)
           ("C-x D" . delete-this-file)
           ("C-x W" . copy-this-file-to-new-file)
           ("C-x C" . cleanup-buffer-safe)
           ("C-=" . text-scale-increase)
           ("C--" . text-scale-decrease)
           ;; flyspell
           ("C-c 4" . ispell-word)
           ("C-c q" . auto-fill-mode)
           ("C-x C-b" . ibuffer)
           ("C-x m" . show-messages-buffer)
           ("M-/" . hippie-expand)
           ("RET" . newline-and-indent)
           ("M-'" . tiny-expand)
           ("M--" . er/expand-region)
           ([remap kill-ring-save] . easy-kill)
           ([f6] . toggle-company-ispell)
           ([f7] . create-scratch-buffer)
           ([f5] . gdb)
           ;; buffer-mode
           ("C-c w i" . buf-move-up)
           ("C-c w k" . buf-move-down)
           ("C-c w j" . buf-move-left)
           ("C-c w l" . buf-move-right)
           ;; ace and avy
           ("C-x o" . ace-window)
           ("C-x n n" . narrow-or-widen-dwim)
           ("C-:" . avy-goto-char)
           ("C-\"" . avy-goto-char-2)
           ("M-g l" . avy-goto-line)
           ("M-g w" . avy-goto-word-1)
           ("M-g y" . avy-copy-line))

(provide 'after-init)