;;; -*- lexical-binding: t; -*-

(after! css-mode
  (define-hook! ymacs-web|css-setup (css-mode-hook)
    (rainbow-mode 1)
    (setq-local eldoc-documentation-function #'css-eldoc-function)
    (setq imenu-create-index-function 'ymacs-web//imenu-make-index)))

(after! tide
  (define-key! :map tide-mode-map
    ("C-c C-l" . ymacs-web/load-in-repl)
    ("C-c C-d" . tide-documentation-at-point)
    ("M-?" . tide-references)
    ("C-c r r" . tide-refactor)
    ("C-c r n" . tide-rename-symbol)
    ("C-c r f" . tide-rename-file)
    ("C-c b" . tide-format)
    ("C-c B" . tide-organize-imports)
    ("C-c C-b" . tide-format)))

(after! web-mode
  (define-key! :map web-mode-map
    ("M-e" . web-mode-element-next)
    ("M-a" . web-mode-element-previous))

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

(after! projectile
  (projectile-register-project-type 'npm '("package.json")
                                    :configure "npm install"
                                    :compile "npm run build"
                                    :test "npm test"
                                    :run "npm start"
                                    :test-suffix ".test"))
