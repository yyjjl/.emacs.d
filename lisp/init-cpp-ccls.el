;; -*- lexical-binding:t -*-

(require-packages! lsp-mode ccls)

(defface cpp-variable-face
  `((t :foreground ,(face-attribute 'default :foreground)))
  "Face for variables"
  :group 'ccls-sem)

(defconst cpp-ccls--default-template
  "clang \n%c -std=gnu11\n%cpp -std=c++14\n\n")

(defmacro cpp-ccls//define-find (symbol command &optional extra)
  `(defun ,(intern (format "cpp/xref-find-%s" symbol)) ()
     (interactive)
     (lsp-find-custom ,command ,extra)))

(cpp-ccls//define-find base "$ccls/base")
;; (cpp-ccls//define-find bases "$ccls/inheritance" '(:level 3))
;; (cpp-ccls//define-find derived "$ccls/inheritance" '(:level 3 :derived t))
(cpp-ccls//define-find callers "$ccls/callers")
(cpp-ccls//define-find callee "$ccls/call" '(:callee t))
;; (cpp-ccls//define-find vars "$ccls/vars")
(cpp-ccls//define-find members "$ccls/member")
(cpp-ccls//define-find references-write "textDocument/references"
                       '(:context (:role 16)))
(cpp-ccls//define-find references-read "textDocument/references"
                       '(:context (:role 8)))
(cpp-ccls//define-find references-not-call "textDocument/references"
                       '(:context (:excludeRole 32)))
(cpp-ccls//define-find references-macro "textDocument/references"
                       '(:context (:role 64)))
(cpp-ccls//define-find references-address "textDocument/references"
                       '(:context (:role 128)))

(defsubst cpp-ccls//dot-ccls-path (&optional -directory)
  (->> (or -directory default-directory)
       (locate-dominating-file ".ccls")
       (expand-file-name ".ccls")))

(defvar cpp-ccls-jump-map
  (define-key! :map (make-sparse-keymap)
    ("b" . cpp/xref-find-base)
    ;; ("B" . cpp/xref-find-bases)
    ;; ("d" . cpp/xref-find-derived)
    ("c" . cpp/xref-find-callers)
    ("e" . cpp/xref-find-callee)
    ;; ("v" . cpp/xref-find-vars)
    ("m" . cpp/xref-find-members)
    ("M" . cpp/xref-find-references-macro)
    ("w" . cpp/xref-find-references-write)
    ("n" . cpp/xref-find-references-not-call)
    ("r" . cpp/xref-find-references-read)
    ("a" . cpp/xref-find-references-address)
    ("R" . ccls-reload)))

(config! ccls
  :config
  (aset ccls-sem-macro-faces 0 'font-lock-builtin-face)
  (aset ccls-sem-variable-faces 0 'cpp-variable-face)

  (setq ccls-executable cpp-ccls-path)
  (setq ccls-initialization-options '(:completion (:detailedLabel t)))
  (setq ccls-sem-highlight-method nil))

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
