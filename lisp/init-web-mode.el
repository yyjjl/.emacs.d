;; don't use (unlesss (is-buffer-file-temp)) ,there is a bug
(defun web-mode-hook-setup ()
  (enable-flyspell-mode-conditionally)

  (add-to-list 'company-backends 'company-web-html)
  ;;     (add-to-list 'company-backends 'company-web-jade)
  ;;     (add-to-list 'company-backends 'company-web-slim)

  (remove-hook 'yas-after-exit-snippet-hook
               'web-mode-yasnippet-exit-hook t)
  (remove-hook 'yas/after-exit-snippet-hook
               'web-mode-yasnippet-exit-hook t))

(add-hook 'web-mode-hook 'web-mode-hook-setup)

(with-eval-after-load 'web-mode
  (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1
           " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1
           " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))
  (define-key web-mode-map (kbd "C-c b") #'web-beautify-html)
  (remap-kbd "C-c C-a" "C-c a" web-mode-map)
  (remap-kbd "C-c C-d" "C-c d" web-mode-map)
  (remap-kbd "C-c C-e" "C-c e" web-mode-map)
  (remap-kbd "C-c C-t" "C-c t" web-mode-map))

(provide 'init-web-mode)
