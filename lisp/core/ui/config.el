;;; -*- lexical-binding: t; -*-

(setq-default mode-line-buffer-identification '("%b"))

(blink-cursor-mode -1)
(tooltip-mode -1)
(window-divider-mode -1)

;; `line-spacing' make inline-image flickering a lot
(setq-default line-spacing 0.25)

(setq echo-keystrokes 0.25)

;; paren
(setq blink-matching-paren nil)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))

(ignore-errors
  (set-fringe-bitmap-face 'right-curly-arrow 'warning)
  (set-fringe-bitmap-face 'left-curly-arrow 'warning)
  (set-fringe-bitmap-face 'right-triangle 'error)
  (dolist (bitmap '(right-arrow left-arrow up-arrow down-arrow))
    (set-fringe-bitmap-face bitmap 'compilation-info)))

(setq widget-image-enable nil)

;; Don't display line number in mode line when buffer is too large
(setq line-number-display-limit ymacs-large-buffer-limit)

;; Update ui less often
(setq idle-update-delay 2)
;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; Disable bidirectional text for tiny performance boost
(setq bidi-display-reordering nil)
(setq frame-resize-pixelwise t)

(setq cursor-in-non-selected-windows t)
(setq highlight-nonselected-windows t)

(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Minibuffer resizing
(setq-default max-mini-window-height 0.25)
(setq-default resize-mini-windows 'grow-only)

(setq split-width-threshold 120)

;; Defer jit font locking slightly to [try to] improve Emacs performance
;; (setq-default jit-lock-defer-time 0.3)
(setq jit-lock-defer-time nil)
(setq jit-lock-stealth-nice 0.5)
(setq jit-lock-stealth-time 5)
(setq jit-lock-stealth-verbose nil)

;; flash the frame to represent a bell.
(setq ring-bell-function #'ignore)
(setq visible-bell t)

(after! tab-line
  (setq tab-line-left-button "<")
  (setq tab-line-right-button ">")
  (setq tab-line-close-button-show nil)
  (setq tab-line-new-button-show nil)
  (setq tab-line-tab-name-function
        (lambda (buffer &optional _buffers)
          (format "[%s]" (buffer-name buffer))))
  (setq tab-line-separator nil)
  (setq tab-line-tabs-function #'ymacs-popup//get-active-term-buffer-list))

(after! winum
  (define-key! :map winum-keymap
    ("C-x w")
    ("M-0" . ymacs-editor/goto-next-char-or-minibuffer)
    ("M-1" . winum-select-window-1)
    ("M-2" . winum-select-window-2)
    ("M-3" . winum-select-window-3)
    ("M-4" . winum-select-window-4)
    ("M-5" . winum-select-window-5)
    ("M-6" . winum-select-window-6))

  (add-to-list 'winum-ignored-buffers " *LV*")

  (setcdr (assoc 'winum-mode minor-mode-map-alist) winum-keymap)

  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local
        winum-reverse-frame-list nil
        winum-auto-assign-0-to-minibuffer t))

(setq which-key-dont-use-unicode t)
(after! which-key
  (setq which-key-allow-imprecise-window-fit nil)
  (setq which-key-show-remaining-keys t))

(after! which-func
  (setq which-func-format
        '("[" (:propertize which-func-current face which-func) "]")))

(after! display-line-numbers
  (setq display-line-numbers-type t)
  (setq-default display-line-numbers-width 2))

;; `whitespace-space' setup
(after! whitespace
  (setq whitespace-global-modes '(text-mode))
  (setq whitespace-style '(face tabs tab-mark spaces space-mark empty)))

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top))
