(require 'core-theme)
(require 'core-modeline)

(when (or window-system (daemonp))
  (run-with-idle-timer 1 nil #'unicode-fonts-setup))

(blink-cursor-mode -1)
(tooltip-mode -1)

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
     #b11111111]))

(setq-default
 ;; disable bidirectional text for tiny performance boost
 bidi-display-reordering nil
 blink-matching-paren nil
 cursor-in-non-selected-windows nil
 display-line-numbers-width 3
 frame-inhibit-implied-resize t
 ;; Remove continuation arrow on right fringe
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 max-mini-window-height 0.3
 ;; disable mode-line mouseovers
 mode-line-default-help-echo nil
 ;; Minibuffer resizing
 resize-mini-windows 'grow-only
 ;; hide :help-echo text
 show-help-function nil
 split-width-threshold 120
 visible-cursor nil
 x-stretch-cursor nil
 ;; Defer jit font locking slightly to [try to] improve Emacs performance
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 ;; No beeping or blinking please
 ring-bell-function #'ignore
 ;; Visible-bell has some issue
 visible-bell nil)

(provide 'core-ui)
