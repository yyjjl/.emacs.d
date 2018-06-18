(require-packages!
 emmet-mode
 web-mode
 company-web)
;; Optional package add support for angluar 1.x
;; (require! 'ac-html-angular)
;; (require! 'ac-html-bootstrap)



(define-hook! web|setup (web-mode-hook)
  (highlight-indentation-mode -1)
  (add-to-list 'company-backends 'company-web-html)
  ;;     (add-to-list 'company-backends 'company-web-jade)
  ;;     (add-to-list 'company-backends 'company-web-slim)

  (remove-hook 'yas-after-exit-snippet-hook
               'web-mode-yasnippet-exit-hook t)
  (remove-hook 'yas/after-exit-snippet-hook
               'web-mode-yasnippet-exit-hook t))

(with-eval-after-load 'web-mode
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
  (remap! "C-c C-a" "C-c a" web-mode-map)
  (remap! "C-c C-d" "C-c d" web-mode-map)
  (remap! "C-c C-e" "C-c e" web-mode-map)
  (remap! "C-c C-t" "C-c t" web-mode-map)
  (define-key! :map web-mode-map
    ("M-n" . web-mode-element-next)
    ("M-p" . web-mode-element-previous)))

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'nxml-mode 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(provide 'init-web-mode)
