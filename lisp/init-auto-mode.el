;; Handier way to add modes to `auto-mode-alist'
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given
file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


(add-auto-mode 'conf-mode
               "\\.[^b][^a][a-zA-Z]*rc$"
               "\\.aspell\\.en\\.pws\\'"
               "\\.meta\\'"
               "\\.?muttrc\\'"
               "\\.ctags\\'"
               "\\.mailcap\\'")

(add-auto-mode 'groovy-mode
               "\\.groovy\\'"
               "\\.gradle\\'")

(add-auto-mode 'crontab-mode
               "crontab.*\\'"
               "\\.?cron\\(tab\\)?\\'")

;; cmake
(add-auto-mode 'cmake-mode
               "CMakeLists\\.txt\\'"
               "\\.cmake\\'")
;; markdown
(add-auto-mode 'markdown-mode
               "\\.\\(md\\|markdown\\)\\'")

(add-auto-mode 'emacs-lisp-mode
               "\\.emacs-project\\'"
               "archive-contents\\'")

(add-auto-mode 'js-mode "\\.json\\'" "\\.jason\\'" "\\.jshintrc\\'")
(add-auto-mode 'js2-mode "\\.js\\(\\.erb\\)?\\'")
(add-auto-mode 'js2-jsx-mode "\\.jsx?\\'")

(add-auto-mode 'sh-mode
               "\\.basj_profile\\'" "\\.bash_history\\'"
               "\\.sh\\'" "\\.bash\\'" "\\.bashrc.local\\'"
               "\\.zsh\\'" "\\.bashrc\\'")

(add-auto-mode 'web-mode
               "\\.phtml\\'" "\\.cmp\\'" "\\.app\\'"
               "\\.page\\'" "\\.component\\'"
               "\\.wp\\'" "\\.tmpl\\'" "\\.php\\'"
               "\\.module\\'" "\\.inc\\'" "\\.hbs\\'"
               "\\.tpl\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
               "\\.erb\\'" "\\.mustache\\'"
               "\\.djhtml\\'" "\\.ftl\\'"
               "\\.html?\\'" "\\.xul?\\'" "\\.eex?\\'"
               "\\.xml?\\'")

(add-auto-mode 'glsl-mode
               "\\.glsl\\'" "\\.vert\\'"
               "\\.frag\\'" "\\.geom\\'")

(add-auto-mode 'latex-mode "\\.tikz\\'")

(setcdr (assoc-string "\\.m\\'" auto-mode-alist) 'octave-mode)
(setcdr (assoc-string "\\.[tT]e[xX]\\'" auto-mode-alist) 'latex-mode)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("python" .   python-mode))

(provide 'init-auto-mode)