;; -*- lexical-binding:t -*-

(require-packages! cmake-mode cmake-font-lock lsp-mode)

(executable! cmake)

(defcustom ymacs-cpp-cmake-config-list
  '(("Release"
     (build . "build/release")
     (env . ())
     (options . (("CMAKE_BUILD_TYPE" . "Release"))))
    ("Debug"
     (build . "build/debug")
     (env . ())
     (options . (("CMAKE_BUILD_TYPE" . "Debug")))))
  "Project settings"
  :group 'cpp-cmake
  :type 'directory
  :safe #'listp)

(defcustom ymacs-cpp-cmake-current-config-name "Release"
  "Project settings"
  :group 'cpp-cmake
  :type 'directory
  :safe #'stringp)

(defcustom ymacs-cpp-cmake-available-options nil
  "All available cmake options of this project"
  :group 'cpp-cmake
  :type 'directory
  :safe #'listp)

(defvar-local ymacs-cpp-cmake-project-root nil)

(add-auto-mode! 'cmake-mode "CMakeLists\\.txt\\'" "\\.cmake\\'")

(ymacs-lsp//register-client 'cmakels :package 'lsp-cmake)

(after-feature! cpp
  (add-to-list 'ymacs-cpp-lsp-checkers #'ymacs-cpp-cmake//check)
  (add-to-list 'ymacs-cpp-lsp-fallback-functions #'ymacs-cpp-cmake//lsp-fallback)

  (setq ymacs-cpp-environment-function #'ymacs-cpp-cmake//config-env)
  (setq ymacs-cpp-build-dir-function
        (lambda ()
          (and ymacs-cpp-cmake-project-root
               (ymacs-cpp-cmake//config-build)))))
