;;; -*- lexical-binding: t; -*-

(after! compile
  (define-key! :map compilation-shell-minor-mode-map
    ("M-}" . ymacs-term/next)
    ("M-{" . ymacs-term/prev)))

(after! ghostel
  ;; Let our exit hook (see functions.el / config below) decide the buffer's
  ;; fate through `ymacs-term-exit-action', instead of ghostel killing it.
  (setq ghostel-kill-buffer-on-exit nil)

  (add-hook 'ghostel-exit-functions #'ymacs-term//ghostel-exit)

  ;; Ghostel's semi-char mode forwards most keys to the terminal, so our
  ;; window / navigation keys must be bound in `ghostel-semi-char-mode-map'
  ;; to win over the per-key `ghostel--send-event' bindings.
  (define-key! :map ghostel-semi-char-mode-map
    ("M-0" . ymacs-editor/goto-char-or-minibuffer)
    ("M-1" . ymacs-editor/aw-select-window)
    ("M-2" . ymacs-editor/aw-select-window)
    ("M-3" . ymacs-editor/aw-select-window)
    ("M-4" . ymacs-editor/aw-select-window)
    ("M-5" . ymacs-editor/aw-select-window)
    ("M-6" . ymacs-editor/aw-select-window)
    ("M-}" . ymacs-term/next)
    ("M-{" . ymacs-term/prev)
    ("M-o" . ymacs-term/switch)
    ("C-s" . ymacs-term/line)
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
    ("C-S-t" . ymacs-term/pop-shell-here)))
