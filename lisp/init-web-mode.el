;; flyspell set up for web-mode
(defun web|flyspell-verify ()
  (let ((f (get-text-property (- (point) 1) 'face)))
    (not (memq f '(web-mode-html-attr-value-face
                   web-mode-html-tag-face
                   web-mode-html-attr-name-face
                   web-mode-constant-face
                   web-mode-doctype-face
                   web-mode-keyword-face
                   web-mode-comment-face ;; focus on get html label right
                   web-mode-function-name-face
                   web-mode-variable-name-face
                   web-mode-css-property-name-face
                   web-mode-css-selector-face
                   web-mode-css-color-face
                   web-mode-type-face
                   web-mode-block-control-face)))))
(put 'web-mode 'flyspell-mode-predicate 'web|flyspell-verify)

(defhook web|setup (web-mode-hook)
  (ignore-errors (spelling|enable))

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
  (define-key web-mode-map (kbd "C-c b") #'web-beautify-html)
  (remap-keybindings "C-c C-a" "C-c a" web-mode-map)
  (remap-keybindings "C-c C-d" "C-c d" web-mode-map)
  (remap-keybindings "C-c C-e" "C-c e" web-mode-map)
  (remap-keybindings "C-c C-t" "C-c t" web-mode-map))

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'nxml-mode 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(provide 'init-web-mode)
