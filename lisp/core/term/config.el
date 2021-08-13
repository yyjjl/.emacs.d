;;; -*- lexical-binding: t; -*-

(after! term
  (define-key! :map term-raw-map
    ([remap term-send-raw] . ymacs-term/conditional-send-raw))

  (define-key! :map term-mode-map
    ("M-o" . ymacs-term/switch)))

(after! compile
  (define-key! :map compilation-shell-minor-mode-map
    ("M-}" . ymacs-term/next)
    ("M-{" . ymacs-term/prev)))

(after! vterm
  (setq vterm-buffer-name-string nil)
  (setq vterm-kill-buffer-on-exit nil)

  (define-key! :map vterm-copy-mode-map
    ("C-c C-k" . vterm-copy-mode))

  (define-key! :map vterm-mode-map
    ("M-}" . ymacs-term/next)
    ("M-{" . ymacs-term/prev)
    ("M-o" . ymacs-term/switch)
    ("M-N" . ymacs-term/set-extra-name)
    ("M-y" . ymacs-term/yank-pop)
    ("C-k" . ymacs-term/kill-line)
    ("C-s" . ymacs-term/line)
    ("C-c C-j" . vterm-copy-mode)
    ("C-c C-l" . vterm-copy-mode)
    ("<C-backspace>" . vterm-send-meta-backspace)
    ("C-S-t" . ymacs-term/pop-shell-here)))

(after! comint
  (setq comint-prompt-read-only t)

  (define-key! :map comint-mode-map
    ("M-o" . ymacs-term/switch)
    ("M-h" . consult-history)))

(after! shell
  (define-key! :map shell-mode-map
    ("C-r" . comint-history-isearch-backward-regexp)
    ("M-}" . ymacs-term/next)
    ("M-{" . ymacs-term/prev)
    ("M-o" . ymacs-term/switch)
    ("M-N" . ymacs-term/set-extra-name)
    ("C-S-t" . ymacs-term/pop-shell-here)))
