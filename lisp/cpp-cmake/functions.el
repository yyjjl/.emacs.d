;;; -*- lexical-binding: t; -*-

(defsubst ymacs-cpp-cmake//current-config ()
  (assoc-string ymacs-cpp-cmake-current-config-name ymacs-cpp-cmake-config-list))

(defsubst ymacs-cpp-cmake//config-name (&optional -config)
  (car (or -config (ymacs-cpp-cmake//current-config))))

(defsubst ymacs-cpp-cmake//config-slot (slot &optional -config)
  (cdr (assoc slot (cdr (or -config (ymacs-cpp-cmake//current-config))))))

(defsubst ymacs-cpp-cmake//config-env (&optional -config)
  (ymacs-cpp-cmake//config-slot 'env -config))

(defsubst ymacs-cpp-cmake//config-options (&optional -config)
  (ymacs-cpp-cmake//config-slot 'options -config))

(defsubst ymacs-cpp-cmake//config-build (&optional -config)
  (let ((item (assoc 'build (cdr (or -config (ymacs-cpp-cmake//current-config))))))
    (when item
      (setcdr item (expand-file-name (cdr item) ymacs-cpp-cmake-project-root)))))

(defsubst ymacs-cpp-cmake//set-config-slot (slot value &optional -config)
  (setcdr (assoc slot (cdr (or -config (ymacs-cpp-cmake//current-config))))
          value))

(defsubst ymacs-cpp-cmake//option-to-string (option)
  (concat "-D" (car option) "=" (cdr option)))

(defsubst ymacs-cpp-cmake//new-config (name)
  (unless (assoc-string name ymacs-cpp-cmake-config-list)
    (setq ymacs-cpp-cmake-current-config-name name)
    (push `(,ymacs-cpp-cmake-current-config-name
            (build . "build/Debug")
            (env . ())
            (options . (("CMAKE_BUILD_TYPE" . "Debug"))))
          ymacs-cpp-cmake-config-list)))

(defsubst ymacs-cpp-cmake//cmakelists-path ()
  (expand-file-name "CMakeLists.txt" ymacs-cpp-cmake-project-root))

(defsubst ymacs-cpp-cmake//cdb-path ()
  (expand-file-name "compile_commands.json" (ymacs-cpp-cmake//config-build)))

(defun ymacs-cpp-cmake//check ()
  (and
   ymacs-cmake-path
   (setq ymacs-cpp-cmake-project-root (locate-topmost! "CMakeLists.txt"))
   (file-exists-p (ymacs-cpp-cmake//cdb-path))))

(defun ymacs-cpp-cmake//lsp-fallback ()
  (when ymacs-cpp-cmake-project-root
    (message
     (substitute-command-keys
      "Run `ymacscpp/config-project'(\\[ymacscpp/config-project] to setup lsp"))))
