;; -*- lexical-binding:t -*-

(defun ymacs-cpp//common-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)"
  (google-set-c-style)
  (setq c-basic-offset 4)
  ;; make DEL take all previous whitespace with it
  ;; (c-toggle-auto-newline 1)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1))

(defun ymacs-cpp//font-lock-setup ()
  (when (or (eq font-lock-maximum-decoration 1)
            (and (listp font-lock-maximum-decoration)
                 (eq 1 (alist-get major-mode font-lock-maximum-decoration))))
    (font-lock-add-keywords nil ymacs-cpp-font-lock-keywords)))

(defun ymacs-cpp//cpp-setup ()
  (when (buffer-enable-rich-feature-p)
    (try-enable-lsp! cpp
      :condition
      ;; NOTE: all checkers should be evaled
      (--some it (--map (funcall it) ymacs-cpp-lsp-checkers))
      :init
      (progn
        (setq-local lsp-eldoc-render-all nil)
        (setq ymacs-lsp-format-buffer-function #'clang-format-buffer))
      :fallback
      (progn
        (run-hooks 'ymacs-cpp-lsp-fallback-functions)
        (flycheck-mode -1)))))

(defun ymacs-cpp//env ()
  (when (functionp ymacs-cpp-environment-function)
    (funcall ymacs-cpp-environment-function)))

(defun ymacs-cpp//build-dir ()
  (when (functionp ymacs-cpp-build-dir-function)
    (funcall ymacs-cpp-build-dir-function)))
