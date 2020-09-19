;;; -*- lexical-binding: t; -*-

(defun cpp-ccls//filter-arguments (-args &optional -no-filename)
  (--filter
   (not (or (string-prefix-p "-working-directory" it)
            (string-prefix-p "-resource-dir" it)
            (string-prefix-p "-fparse-all-comments" it)
            (member it '("-o" "-c"))
            (and -no-filename
                 (not (string-prefix-p "-" it)))))
   -args))

(defsubst cpp-ccls//get-file-args ()
  (or (when-let (args (ignore-errors (gethash "args" (ccls-file-info))))
        (when (vectorp args)
          (setq args (cl-map 'list #'identity args)))
        args)
      '("cpp" "-v" "-E")))

(defun cpp-ccls//filter-include-lines (-lines)
  (--map (expand-file-name (s-trim it))
         (--filter (string-prefix-p " " it)
                   (split-string -lines "\n" :omit-nulls))))

(defun cpp-ccls//include-directories ()
  (let ((args (cpp-ccls//get-file-args)))
    (with-temp-buffer
      (erase-buffer)
      (apply #'call-process (car args) nil t nil
             "-v" "-E" "-"
             (cpp-ccls//filter-arguments (cdr args) :no-filename))
      (goto-char (point-min))
      (let (pos1 pos2 pos3)
        (when (re-search-forward "^#include \"\\.\\.\\.\"" nil :noerror)
          (forward-line 1)
          (setq pos1 (point)))
        (when (re-search-forward "^#include <\\.\\.\\.>" nil :noerror)
          (forward-line 1)
          (setq pos2 (point)))
        (when (re-search-forward "^End of search list\\." nil :noerror)
          (forward-line 0)
          (setq pos3 (point)))
        (when (and pos1 pos2 pos3)
          (cons
           (cpp-ccls//filter-include-lines (buffer-substring pos1 pos2))
           (cpp-ccls//filter-include-lines (buffer-substring pos2 pos3))))))))

;;;###autoload
(defun cpp-ccls//buffer-compile-command (&optional -preprocess-only-p)
  (let* ((args (cpp-ccls//get-file-args))
         (options (cpp-ccls//filter-arguments (cdr args) -preprocess-only-p)))
    (when -preprocess-only-p
      (dolist (option '("-E" "-xc++" "-C"))
        (unless (member option options)
          (push option options))))
    (concat (car args) " "
            (string-join options " ")
            (when -preprocess-only-p " -"))))

;;;###autoload
(defun cpp-ccls/install-or-upgrade-ccls (&optional -upgrade)
  (interactive "P")
  (let ((build-command "mkdir -p build && cd build && cmake --build .. --config Release && make")
        (download-command "git clone --depth=1 --recursive https://github.com/MaskRay/ccls"))
    (if (file-exists-p cpp-ccls-base-path)
        (if -upgrade
            (let ((default-directory cpp-ccls-base-path))
              (run-command!
               :name "update ccls"
               :command (concat "git pull && git submodule update && " build-command)))
          (message "ccls directory exists"))
      (let ((default-directory emacs-var-direcotry))
        (make-directory cpp-ccls-base-path t)
        (run-command!
         :name "install ccls"
         :command (concat download-command " && " build-command))))))

;;;###autoload
(defun cpp-ccls/create-dot-ccls (-directory)
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Directory: ")
                       default-directory)))
  (when -directory
    (let ((ccls-file (expand-file-name ".ccls" -directory)))
      (if (file-exists-p ccls-file)
          (message ".ccls already exists")
        (with-temp-buffer
          (erase-buffer)
          (insert cpp-ccls--default-template)
          (write-file ccls-file))
        (message ".ccls created")))))
