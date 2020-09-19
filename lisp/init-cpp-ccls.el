;; -*- lexical-binding:t -*-

(require-packages! lsp-mode ccls)

(add-to-list 'cpp-buffer-command-functions #'cpp-ccls//buffer-compile-command)

(setq cpp-lsp-checker (lambda () (file-exists-p (cpp-ccls//dot-ccls-path))))
(setq cpp-expand-macro-function (lambda () (cpp-ccls//buffer-compile-command t)))

(defface cpp-variable-face
  `((t :foreground ,(face-attribute 'default :foreground)))
  "Face for variables"
  :group 'ccls-sem)

(defconst cpp-ccls--default-template
  "clang \n%c -std=gnu11\n%cpp -std=c++14\n-Wall\n")

(defmacro cpp-ccls//define-find (symbol command &optional extra)
  `(defun ,(intern (format "cpp/xref-find-%s" symbol)) ()
     (interactive)
     (lsp-find-custom ,command ,extra)))

(cpp-ccls//define-find bases "$ccls/inheritance" '(:level 3))
(cpp-ccls//define-find derived "$ccls/inheritance" '(:level 3 :derived t))
(cpp-ccls//define-find callers "$ccls/call")
(cpp-ccls//define-find callee "$ccls/call" '(:callee t))
(cpp-ccls//define-find members "$ccls/member")

(cpp-ccls//define-find references-write "textDocument/references"
                       (plist-put (lsp--text-document-position-params) :role 16))
(cpp-ccls//define-find references-read "textDocument/references"
                       (plist-put (lsp--text-document-position-params) :role 8))
(cpp-ccls//define-find references-macro "textDocument/references"
                       (plist-put (lsp--text-document-position-params) :role 64))
(cpp-ccls//define-find references-address "textDocument/references"
                       (plist-put (lsp--text-document-position-params) :role 128))

(defsubst cpp-ccls//dot-ccls-path (&optional -directory)
  (->> (or -directory default-directory)
       (locate-dominating-file ".ccls")
       (expand-file-name ".ccls")))

(defvar cpp-ccls-jump-map
  (define-key! :map (make-sparse-keymap)
    ("b" . cpp/xref-find-bases)
    ("d" . cpp/xref-find-derived)
    ("c" . cpp/xref-find-callers)
    ("e" . cpp/xref-find-callee)
    ("m" . cpp/xref-find-members)
    ("M" . cpp/xref-find-references-macro)
    ("w" . cpp/xref-find-references-write)
    ("r" . cpp/xref-find-references-read)
    ("a" . cpp/xref-find-references-address)
    ("R" . ccls-reload)))

(config! ccls
  :bind
  (:map c++-mode-map
   ("M-s c" . ccls-call-hierarchy)
   ("M-s m" . ccls-member-hierarchy)
   ("M-s i" . ccls-inheritance-hierarchy)
   ("C-c j" :map cpp-ccls-jump-map))

  :toggles
  ((and lsp-mode (derived-mode-p 'c-mode 'c++-mode))
   "CCLS"
   (("h n" (progn
             (ccls--clear-sem-highlights)
             (setq ccls-sem-highlight-method nil))
     "no sem-highlight"
     :toggle (not ccls-sem-highlight-method))
    ("h o" (progn
             (ccls--clear-sem-highlights)
             (setq ccls-sem-highlight-method 'overlay))
     "sem-highlight with overlays"
     :toggle (eq ccls-sem-highlight-method 'overlay))
    ("h f" (progn
             (ccls--clear-sem-highlights)
             (setq ccls-sem-highlight-method 'font-lock))
     "sem-highlight with font-lock"
     :toggle (eq ccls-sem-highlight-method 'font-lock))))

  :config
  (aset ccls-sem-macro-faces 0 'font-lock-builtin-face)
  (aset ccls-sem-variable-faces 0 'cpp-variable-face)

  (setq ccls-executable cpp-ccls-path)
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (setq ccls-sem-highlight-method 'font-lock))

(config! ccls-tree
  :bind
  (:map ccls-tree-mode-map
   ("o" . ccls-tree-press)
   ("." . ccls-tree-expand-or-set-root)
   ("^" . ccls-tree-collapse-or-select-parent)
   ("j" . next-line)
   ("k" . previous-line))

  :hook
  (:anonymous
   :define (ccls-tree-mode-hook)
   (toggle-truncate-lines 1)))


(provide 'init-cpp-ccls)
