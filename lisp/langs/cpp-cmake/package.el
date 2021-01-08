;; -*- lexical-binding:t -*-

(eval-when-compile
  (unless (has-feature! 'cpp)
    (user-error "cpp-cmake should be loaded after feature cpp")))

(require-packages! cmake-mode cmake-font-lock)

(executable! cmake)

(defvar ymacs-cpp-cmake-config-buffer-name "*ymacs-cpp-cmake-config*")

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
  :group 'ymacs
  :type '(alist)
  :safe #'listp)

(defcustom ymacs-cpp-cmake-current-config-name "Release"
  "Project settings"
  :group 'ymacs
  :type 'string
  :safe #'stringp)

(defcustom ymacs-cpp-cmake-available-options nil
  "All available cmake options of this project"
  :group 'ymacs
  :type '(repeat string)
  :safe #'listp)

(defvar-local ymacs-cpp-cmake-project-root nil)

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'cmakels :package 'lsp-cmake))
