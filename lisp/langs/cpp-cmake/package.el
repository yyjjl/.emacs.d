;; -*- lexical-binding:t -*-

(option! cpp-cmake-config-list
    '(("Release"
       (build . "build/release")
       (env . ())
       (options . (("CMAKE_BUILD_TYPE" . "Release"))))
      ("Debug"
       (build . "build/debug")
       (env . ())
       (options . (("CMAKE_BUILD_TYPE" . "Debug")))))
  "Project settings"
  :type '(alist)
  :safe #'listp)

(option! cpp-cmake-current-config-name "Release"
  "Project settings"
  :type 'string
  :safe #'stringp)

(option! cpp-cmake-available-options nil
  "All available cmake options of this project"
  :type '(repeat string)
  :safe #'listp)

(eval-when-compile
  (unless (has-feature! 'cpp)
    (user-error "cpp-cmake should be loaded after feature cpp")))

(require-packages! cmake-mode cmake-font-lock)

(executable! cmake)

(defvar ymacs-cpp-cmake-config-buffer-name "*ymacs-cpp-cmake-config*")
(defvar-local ymacs-cpp-cmake-project-root nil)

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'cmakels :package 'lsp-cmake))
