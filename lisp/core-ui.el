(require 'core-theme)
(require 'core-mode-line)

(blink-cursor-mode -1)
(tooltip-mode -1)

(setq-default window-divider-default-places t)
(setq-default window-divider-default-bottom-width 1)
(setq-default window-divider-default-right-width 1)
(window-divider-mode 1)

(setq show-paren-when-point-inside-paren t)

(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'empty-line
    [#b00000000
     #b00000000
     #b01110001
     #b11011011
     #b10001110
     #b00000000
     #b00000000
     #b00000000]
    nil nil 'center)
  (set-fringe-bitmap-face 'empty-line 'empty-line-face)
  (define-fringe-bitmap 'right-curly-arrow
    [#b11111111
     #b11111111
     #b00000011
     #b00000011
     #b00000011
     #b00000011
     #b00000011
     #b00000011])
  (define-fringe-bitmap 'left-curly-arrow
    [#b11000000
     #b11000000
     #b11000000
     #b11000000
     #b11000000
     #b11000000
     #b11111111
     #b11111111])
  (set-fringe-bitmap-face 'right-curly-arrow 'warning)
  (set-fringe-bitmap-face 'left-curly-arrow 'warning)
  (set-fringe-bitmap-face 'right-triangle 'error))

 ;; Disable bidirectional text for tiny performance boost
(setq-default bidi-display-reordering nil)
(setq-default blink-matching-paren nil)
(setq-default cursor-in-non-selected-windows t)
(setq-default display-line-numbers-width 3)
(setq-default frame-inhibit-implied-resize t)
 ;; Remove continuation arrow on right fringe
(setq-default highlight-nonselected-windows nil)
(setq-default image-animate-loop t)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)
(setq-default max-mini-window-height 0.3)
;; Disable mode-line mouseovers
(setq-default mode-line-default-help-echo nil)
 ;; Minibuffer resizing
(setq-default resize-mini-windows 'grow-only)
 ;; hide :help-echo text
(setq-default show-help-function nil)
(setq-default split-width-threshold 120)
(setq-default visible-cursor nil)
(setq-default x-stretch-cursor nil)
(setq-default widget-image-enable nil)
(setq-default use-default-font-for-symbols nil)
;; Defer jit font locking slightly to [try to] improve Emacs performance
;; (setq-default jit-lock-defer-time 0.3)
(setq-default jit-lock-defer-time nil)
(setq-default jit-lock-stealth-nice 0.1)
(setq-default jit-lock-stealth-time 0.2)
(setq-default jit-lock-stealth-verbose nil)
 ;; No beeping or blinking please
(setq-default ring-bell-function #'ignore)
 ;; Visible-bell has some issue
(setq-default visible-bell nil)

(provide 'core-ui)
