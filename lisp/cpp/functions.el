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
                 (eq (cdr (assoc major-mode
                                 font-lock-maximum-decoration))
                     1)))
    (font-lock-add-keywords nil ymacs-cpp-font-lock-keywords)))

(defun ymacs-cpp//cpp-setup ()
  (ymacs-lsp//try-enable cpp
    :enable
    ;; NOTE: all checkers should be evaled
    (--some it (--map (funcall it) ymacs-cpp-lsp-checkers))
    :init
    (progn
      (setq-local lsp-eldoc-render-all nil)
      ;; (electric-indent-local-mode -1)
      )
    :fallback
    (progn
      (run-hooks 'ymacs-cpp-lsp-fallback-functions)
      ;; (setq completion-at-point-functions nil)
      (flycheck-mode -1))))

(defun ymacs-cpp//env ()
  (when (functionp ymacs-cpp-environment-function)
    (funcall ymacs-cpp-environment-function)))

(defun ymacs-cpp//build-dir ()
  (when (functionp ymacs-cpp-build-dir-function)
    (funcall ymacs-cpp-build-dir-function)))
