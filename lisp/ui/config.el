;;; -*- lexical-binding: t; -*-

(blink-cursor-mode -1)
(tooltip-mode -1)

(setq-default window-divider-default-places t)
(setq-default window-divider-default-bottom-width 1)
(setq-default window-divider-default-right-width 1)
(window-divider-mode 1)

(setq show-paren-when-point-inside-paren t)

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
(setq-default frame-resize-pixelwise t)
;; Remove continuation arrow on right fringe
(setq-default highlight-nonselected-windows t)
(setq-default image-animate-loop t)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)
(setq-default max-mini-window-height 0.25)
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
(setq-default use-default-font-for-symbols t)
;; Defer jit font locking slightly to [try to] improve Emacs performance
;; (setq-default jit-lock-defer-time 0.3)
(setq-default jit-lock-defer-time nil)
(setq-default jit-lock-stealth-nice 0.5)
(setq-default jit-lock-stealth-time 5)
(setq-default jit-lock-stealth-verbose nil)
 ;; No beeping or blinking please
(setq-default ring-bell-function #'ignore)
 ;; Visible-bell has some issue
(setq-default visible-bell nil)

(custom-theme-set-faces
 'user

 '(font-lock-builtin-face ((t (:foreground "#749e20"))))
 '(font-lock-constant-face ((t (:foreground "#AE81FF"))))
 '(font-lock-warning-face ((t (:background "black" :weight bold :underline t))))
 '(font-lock-doc-face ((t (:foreground "#777777" :weight bold))))

 '(eldoc-highlight-function-argument ((t (:foreground "#86DC2F" :weight bold))))
 '(hl-line ((t (:background "gray5"))))

 '(org-meta-line ((t :inherit font-lock-doc-face)))
 '(org-document-info-keyword ((t :inherit font-lock-builtin-face)))
 '(org-latex-and-related ((t (:foreground "orange red" :weight bold))))

 '(flycheck-error ((t (:underline (:style wave :color "#e74c3c")))))
 '(flycheck-info ((t (:underline (:style wave :color "#b6e63e")))))
 '(flycheck-warning ((t (:underline (:style wave :color "#e2c770")))))

 '(diredp-dir-name ((t (:inherit font-lock-type-face :weight bold))))

 '(show-paren-match ((t (:background "#000000"))))

 '(ivy-current-match ((t (:background "gray0")))))

(after! doom-modeline
  (doom-modeline-def-segment
    buffer-info
    "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
    (let ((name (concat
                 (doom-modeline-spc)
                 (doom-modeline--buffer-mode-icon)
                 (doom-modeline--buffer-state-icon)
                 (doom-modeline--buffer-name))))
      (if (and (listp mode-line-buffer-identification)
               (equal (car mode-line-buffer-identification) "%b"))
          (cons name (cdr mode-line-buffer-identification))
        name)))

  (setq all-the-icons-scale-factor 1.0)

  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'auto)

  (setq doom-modeline-checker-simple-format nil)

  (setq doom-modeline-indent-info nil)
  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (setq doom-modeline-workspace-name nil)
  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-gnus nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-minor-modes nil)

  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)

  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host checker lsp buffer-position misc-info word-count selection-info)
    '(debug repl input-method indent-info buffer-encoding major-mode process vcs))

  (doom-modeline-def-modeline 'org-src
    '(bar window-number matches buffer-info-simple checker lsp buffer-position misc-info word-count selection-info)
    '(debug input-method indent-info buffer-encoding major-mode process)))

(after! winum
  (define-key! :map winum-keymap
    ("C-x w")
    ("M-0" . ymacs-edit/goto-next-char-or-minibuffer)
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
