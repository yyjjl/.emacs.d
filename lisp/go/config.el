;;; -*- lexical-binding: t; -*-

(after! lsp-go
  (ymacs-lsp//set-simple-install-fn
   'gopls
   "GO111MODULE=on go get golang.org/x/tools/gopls@latest && go get -u github.com/fatih/gomodifytags"))

(after! go-mode
  (define-key! :map go-mode-map
    ("C-c t t" . go-tag-add)
    ("C-c t r" . go-tag-refresh)
    ("C-c t R" . go-tag-remove)))
