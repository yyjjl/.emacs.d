;;; -*- lexical-binding: t; -*-

(after! web-mode
  (define-key! :map web-mode-map
    ("M-e" . web-mode-element-next)
    ("M-a" . web-mode-element-previous)
    (("C-c C-b" "C-c b") . web-mode-buffer-indent))

  (setq web-mode-enable-auto-closing t) ;; Enable auto close tag in `web-mode'
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  ;; Auto expand s/ => <span>|</span>
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-element-tag-fontification t)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1
           " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1
           " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))

  (setq web-mode-content-types-alist nil)
  (setq web-mode-enable-auto-indentation nil)

  (remap! "C-c C-a" "C-c a" web-mode-map)
  (remap! "C-c C-d" "C-c d" web-mode-map)
  (remap! "C-c C-e" "C-c e" web-mode-map)
  (remap! "C-c C-t" "C-c t" web-mode-map))
