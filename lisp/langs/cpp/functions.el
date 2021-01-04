;; -*- lexical-binding:t -*-

(defmacro ymacs-cpp//run-function (-slot &optional -build-system &rest -args)
  (let ((access-fn (intern (format "ymacs-cpp-build-system-%s" -slot))))
    `(when-let ((build-system (or ,-build-system ymacs-cpp-current-build-system))
                (fn (,access-fn build-system)))
       (funcall fn ,@-args))))

(defmacro ymacs-cpp//get-function (-slot &optional -build-system)
  (let ((access-fn (intern (format "ymacs-cpp-build-system-%s" -slot))))
    `(when-let (build-system (or ,-build-system ymacs-cpp-current-build-system))
       (,access-fn build-system))))

(defsubst ymacs-cpp//get-dot-clangd-path (&optional -directory)
  (unless -directory
    (setq -directory default-directory))
  (cl-loop for name in '("compile_flags.txt" ".clangd")
           for dir = (locate-dominating-file -directory name)
           when dir
           return (expand-file-name name dir)))

(defun ymacs-cpp//get-compile-command-from-dot-clangd ()
  (when (memq major-mode '(c-mode c++-mode))
    (when-let ((dot-clangd (ymacs-cpp//get-dot-clangd-path))
               (flags (replace-regexp-in-string "\n" " " (read-file-content! dot-clangd) t t)))
      (list
       (list (format "%s %s %s"
                     (cl-case major-mode
                       (c-mode "cc")
                       (c++-mode "c++"))
                     flags
                     (buffer-file-name))
             default-directory)))))

(defun ymacs-cpp//build-dir ()
  (ymacs-cpp//run-function directory-fn))

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
      :-condition
      (catch 'done
        (dolist (build-system ymacs-cpp-build-systems)
          (when (ymacs-cpp//run-function lsp-enable-fn build-system)
            (setq-local ymacs-cpp-current-build-system build-system)
            (throw 'done t))))
      :-init
      (progn
        (setq ymacs-lsp-format-buffer-function #'clang-format-buffer)
        (when-let (command-fn (ymacs-cpp//get-function command-fn))
          (cl-pushnew command-fn ymacs-editor-compile-command-functions))
        (ymacs-cpp//run-function lsp-enable-handler))
      :-fallback
      (dolist (build-system ymacs-cpp-build-systems)
        (ymacs-cpp//run-function lsp-disable-handler build-system)))))
