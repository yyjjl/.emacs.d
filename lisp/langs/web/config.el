;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (after! lsp-mode
    (setf (alist-get 'web-mode lsp--formatting-indent-alist)
          'web-mode-code-indent-offset)))

(after! web-mode
  (define-key! :map web-mode-map
    ("C-k" . ymacs-web/smart-kill)
    ("C-c C-b" . web-mode-buffer-indent))

  (setq web-mode-enable-auto-closing t) ;; Enable auto close tag in `web-mode'
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  ;; Auto expand s/ => <span>|</span>
  (setq web-mode-enable-auto-expanding t)
  ;; (setq web-mode-enable-comment-interpolation nil)
  ;; (setq web-mode-enable-element-tag-fontification t)

  ;; (setq web-mode-enable-auto-indentation nil)

  ;; Auto-quotes with single quote
  (setq web-mode-auto-quote-style 1)
  (setq web-mode-auto-close-style 2)

  (remap! "C-c C-a" "C-c a" web-mode-map)
  (remap! "C-c C-d" "C-c d" web-mode-map)
  (remap! "C-c C-e" "C-c e" web-mode-map)
  (remap! "C-c C-t" "C-c t" web-mode-map))

(after! js
  (define-key! :map js-mode-map
    ("M-.")))
