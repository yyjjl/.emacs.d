;;; -*- lexical-binding: t; -*-

(add-to-list 'ymacs-editor-environment-functions #'ymacs-cpp-cmake//config-env)

(defvar ymacs-cpp-cmake-keymap
  (define-key! :map (make-sparse-keymap)
    ("C-c T" . ymacs-cpp-cmake/toggle-option)
    ("C-c C" . ymacs-cpp-cmake/change-config)
    ("C-c D" . ymacs-cpp-cmake/config)
    ("C-c C-c" . ymacs-cpp-cmake/config-project)
    ([f9] . ymacs-cpp-cmake/config-project)
    ([f10] . ymacs-cpp-cmake/run-build)))

(define-minor-mode ymacs-cpp-cmake-mode
  "Use cmake build-system"
  :group 'cmake
  :global nil
  :keymap ymacs-cpp-cmake-keymap)

(add-to-list
 'ymacs-cpp-build-systems
 (make-ymacs-cpp-build-system
  :system-id 'cmake
  :lsp-enable-fn
  (lambda ()
    (when ymacs-cmake-path
      (setq ymacs-cpp-cmake-project-root (locate-topmost! "CMakeLists.txt"))
      (file-exists-p (ymacs-cpp-cmake//cdb-path))))
  :lsp-enable-handler
  #'ymacs-cpp-cmake-mode
  :lsp-disable-handler
  (lambda ()
    (when ymacs-cpp-cmake-project-root
      (message (substitute-command-keys "Run \\[ymacs-cpp-cmake/config-project] to setup lsp"))))
  :directory-fn
  (lambda ()
    (and ymacs-cpp-cmake-project-root
         (ymacs-cpp-cmake//config-build)))))

(after! cmake-mode
  (define-key! :map cmake-mode-map
    ([f10] . compile)))

(after! cmake-ts-mode
  (define-key! :map cmake-ts-mode-map
    ([f10] . compile)))
