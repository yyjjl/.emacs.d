;; -*- lexical-binding:t -*-

(defsubst ymacs-cpp-ccls//dot-ccls-path (&optional -directory)
  (->> (or -directory default-directory)
       (locate-dominating-file ".ccls")
       (expand-file-name ".ccls")))

(defmacro ymacs-cpp-ccls//define-find (symbol command &optional extra)
  `(defun ,(intern (format "ymacs-cpp-ccls/xref-find-%s" symbol)) ()
     (interactive)
     (lsp-find-custom ,command ,extra)))

(ymacs-cpp-ccls//define-find bases "$ccls/inheritance" '(:level 3))
(ymacs-cpp-ccls//define-find derived "$ccls/inheritance" '(:level 3 :derived t))
(ymacs-cpp-ccls//define-find callers "$ccls/call")
(ymacs-cpp-ccls//define-find callee "$ccls/call" '(:callee t))
(ymacs-cpp-ccls//define-find members "$ccls/member")

(ymacs-cpp-ccls//define-find references-write "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 16))
(ymacs-cpp-ccls//define-find references-read "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 8))
(ymacs-cpp-ccls//define-find references-macro "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 64))
(ymacs-cpp-ccls//define-find references-address "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 128))
