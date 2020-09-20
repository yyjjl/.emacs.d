;; -*- lexical-binding:t -*-

(require-packages! cmake-mode cmake-font-lock lsp-mode)

(defcustom cpp-cmake-config-list
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

(defcustom cpp-cmake-current-config-name "Release"
  "Project settings"
  :group 'cpp-cmake
  :type 'directory
  :safe #'stringp)

(defcustom cpp-cmake-available-options nil
  "All available cmake options of this project"
  :group 'cpp-cmake
  :type 'directory
  :safe #'listp)

(defvar-local cpp-cmake-project-root nil)

(add-auto-mode! 'cmake-mode "CMakeLists\\.txt\\'" "\\.cmake\\'")


(defsubst cpp-cmake//current-config ()
  (assoc-string cpp-cmake-current-config-name cpp-cmake-config-list))

(defsubst cpp-cmake//config-name (&optional  config)
  (car (or config (cpp-cmake//current-config))))

(defsubst cpp-cmake//config-slot (slot &optional config)
  (cdr (assoc slot (cdr (or config (cpp-cmake//current-config))))))

(defsubst cpp-cmake//config-env (&optional config)
  (cpp-cmake//config-slot 'env config))

(defsubst cpp-cmake//config-options (&optional config)
  (cpp-cmake//config-slot 'options config))

(defsubst cpp-cmake//config-build (&optional config)
  (let ((item (assoc 'build (cdr (or config (cpp-cmake//current-config))))))
    (when item
      (setcdr item (expand-file-name (cdr item) cpp-cmake-project-root)))))

(defsubst cpp-cmake//set-config-slot (slot value &optional config)
  (setcdr (assoc slot (cdr (or config (cpp-cmake//current-config))))
          value))

(defsubst cpp-cmake//option-to-string (option)
  (concat "-D" (car option) "=" (cdr option)))

(defsubst cpp-cmake//new-config (name)
  (unless (assoc-string name cpp-cmake-config-list)
    (setq cpp-cmake-current-config-name name)
    (push `(,cpp-cmake-current-config-name
            (build . "build/Debug")
            (env . ())
            (options . (("CMAKE_BUILD_TYPE" . "Debug"))))
          cpp-cmake-config-list)))

(defsubst cpp-cmake//cmakelists-path ()
  (expand-file-name "CMakeLists.txt" cpp-cmake-project-root))

(defsubst cpp-cmake//cdb-path ()
  (expand-file-name "compile_commands.json" (cpp-cmake//config-build)))


(defun cpp-cmake//locate-cmakelists (&optional -dir -last-found -filename)
  "Find the topmost CMakeLists.txt."
  (if-let (new-dir (locate-dominating-file (or -dir default-directory)
                                           (or -filename "CMakeLists.txt")))
      (cpp-cmake//locate-cmakelists (expand-file-name ".." new-dir) new-dir -filename)
    -last-found))

(defun cpp-cmake//check ()
  (and
   cpp-use-cmake-p
   (setq cpp-cmake-project-root (cpp-cmake//locate-cmakelists))
   (file-exists-p (cpp-cmake//cdb-path))))

(config! cmake-mode
  :bind (:map cmake-mode-map ([f10] . compile))

  :hook
  (setup
   :define (cmake-mode-hook)
   (font-lock-mode 1)
   (cmake-font-lock-activate)

   (lsp//try-enable cmake|setup
     :fallback
     (company//add-backend 'company-cmake))))

(provide 'init-cpp-cmake)
