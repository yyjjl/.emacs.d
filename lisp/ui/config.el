;;; -*- lexical-binding: t; -*-

(blink-cursor-mode -1)
(tooltip-mode -1)
(window-divider-mode -1)

(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(ignore-errors
  (set-fringe-bitmap-face 'right-curly-arrow 'warning)
  (set-fringe-bitmap-face 'left-curly-arrow 'warning)
  (set-fringe-bitmap-face 'right-triangle 'error)
  (dolist (bitmap '(right-arrow left-arrow up-arrow down-arrow))
    (set-fringe-bitmap-face bitmap 'compilation-info)))

;; Disable bidirectional text for tiny performance boost
(setq-default bidi-display-reordering nil)
(setq-default blink-matching-paren nil)
(setq-default cursor-in-non-selected-windows t)
(setq-default frame-resize-pixelwise t)
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
 '(font-lock-warning-face ((t (:background "black" :weight bold
                               :underline t))))
 '(font-lock-doc-face ((t (:foreground "#777777" :weight bold))))

 '(eldoc-highlight-function-argument ((t (:foreground "#86DC2F"
                                          :weight bold))))
 '(hl-line ((t (:background "gray5"))))

 '(org-meta-line ((t :inherit font-lock-doc-face)))
 '(org-document-info-keyword ((t :inherit font-lock-builtin-face)))
 '(org-latex-and-related ((t (:foreground "orange red" :weight bold))))

 '(flycheck-error ((t (:underline (:style wave :color "#e74c3c")))))
 '(flycheck-info ((t (:underline (:style wave :color "#b6e63e")))))
 '(flycheck-warning ((t (:underline (:style wave :color "#e2c770")))))

 '(diredp-dir-name ((t (:inherit font-lock-type-face :weight bold))))

 `(diff-hl-change ((t (:foreground ,(face-foreground 'warning)
                       :background nil))))
 '(diff-hl-insert ((t (:background nil))))
 '(diff-hl-delete ((t (:background nil))))

 '(tab-line ((t (:background "#1b1d1e" :foreground "white"))))
 '(tab-line-tab ((t (:inherit warning))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :inverse-video t))))
 '(tab-line-tab-inactive ((t (:inherit font-lock-comment-face :inverse-video t))))

 '(ivy-current-match ((t (:background "gray0")))))

(with-eval-after-load 'tab-line
  (setq tab-line-left-button "<")
  (setq tab-line-right-button ">")
  (setq tab-line-close-button-show nil)
  (setq tab-line-new-button-show nil)
  (setq tab-line-tab-name-function
        (lambda (buffer &optional _buffers)
          (format "[%s]" (buffer-name buffer))))
  (setq tab-line-separator nil)
  (setq tab-line-tabs-function #'ymacs-popup//get-active-term-buffer-list)))

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
