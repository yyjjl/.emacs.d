;;; -*- lexical-binding: t; -*-

(after! cmake-mode
  (define-key! :map cmake-mode-map
    ([f10] . compile)))

(after! cc-mode
  (dolist (map (list c-mode-map c++-mode-map))
    (define-key! :map map
      ("C-c T" . ymacs-cpp-cmake/toggle-option)
      ("C-c C" . ymacs-cpp-cmake/change-config)
      ("C-c D" . ymacs-cpp-cmake/config)
      ("C-c C-c" . ymacs-cpp-cmake/config-project)
      ([f9] . ymacs-cpp-cmake/config-project))))

(after! lsp-cmake
  (ymacs-lsp//set-simple-install-fn
   'cmakels
   "pip3 install --user cmake-language-server"
   "pip3 install --user -U cmake-language-server"))
