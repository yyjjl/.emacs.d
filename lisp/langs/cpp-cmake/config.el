;;; -*- lexical-binding: t; -*-

(add-to-list 'ymacs-editor-environment-functions #'ymacs-cpp-cmake//config-env)

(add-to-list
 'ymacs-cpp-build-systems
 (make-ymacs-cpp-build-system
  :system-id 'cmake
  :lsp-enable-fn
  (lambda ()
    (when ymacs-cmake-path
      (setq ymacs-cpp-cmake-project-root (locate-topmost! "CMakeLists.txt"))
      (file-exists-p (ymacs-cpp-cmake//cdb-path))))
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

(after! cc-mode
  (dolist (map (list c-mode-map c++-mode-map))
    (define-key! :map map
      (("C-c T" "C-c t") . ymacs-cpp-cmake/toggle-option)
      ("C-c C" . ymacs-cpp-cmake/change-config)
      (("C-c D" "C-c d") . ymacs-cpp-cmake/config)
      ("C-c C-c" . ymacs-cpp-cmake/config-project)
      ([f9] . ymacs-cpp-cmake/config-project))))

(eval-when-has-feature! lsp
  (after! lsp-cmake
    (ymacs-lsp//set-simple-install-fn
     'cmakels
     "pip3 install --user cmake-language-server"
     "pip3 install --user -U cmake-language-server")))
