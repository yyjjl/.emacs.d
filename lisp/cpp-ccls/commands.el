;;; -*- lexical-binding: t; -*-

(defun ymacs-cpp-ccls//filter-arguments (-args &optional -no-filename)
  (--filter
   (not (or (string-prefix-p "-working-directory" it)
            (string-prefix-p "-resource-dir" it)
            (string-prefix-p "-fparse-all-comments" it)
            (member it '("-o" "-c"))
            (and -no-filename
                 (not (string-prefix-p "-" it)))))
   -args))

(defsubst ymacs-cpp-ccls//get-file-args ()
  (or (when-let (args (ignore-errors (gethash "args" (ccls-file-info))))
        (when (vectorp args)
          (setq args (cl-map 'list #'identity args)))
        args)
      '("cpp" "-v" "-E")))

(defun ymacs-cpp-ccls//filter-include-lines (-lines)
  (--map (expand-file-name (s-trim it))
         (--filter (string-prefix-p " " it)
                   (split-string -lines "\n" :omit-nulls))))

(defun ymacs-cpp-ccls//include-directories ()
  (let ((args (ymacs-cpp-ccls//get-file-args)))
    (with-temp-buffer
      (erase-buffer)
      (apply #'call-process (car args) nil t nil
             "-v" "-E" "-"
             (ymacs-cpp-ccls//filter-arguments (cdr args) :no-filename))
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
           (ymacs-cpp-ccls//filter-include-lines (buffer-substring pos1 pos2))
           (ymacs-cpp-ccls//filter-include-lines (buffer-substring pos2 pos3))))))))

;;;###autoload
(defun ymacs-cpp-ccls//buffer-compile-command (&optional -preprocess-only-p)
  (let* ((args (ymacs-cpp-ccls//get-file-args))
         (options (ymacs-cpp-ccls//filter-arguments (cdr args) -preprocess-only-p)))
    (when -preprocess-only-p
      (dolist (option '("-E" "-xc++" "-C"))
        (unless (member option options)
          (push option options))))
    (concat (car args) " "
            (string-join options " ")
            (when -preprocess-only-p " -"))))

;;;###autoload
(defun ymacs-cpp-ccls/create-dot-ccls (-directory)
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Directory: ")
                       default-directory)))
  (when -directory
    (let ((ccls-file (expand-file-name ".ccls" -directory)))
      (if (file-exists-p ccls-file)
          (message ".ccls already exists")
        (with-temp-buffer
          (erase-buffer)
          (insert ymacs-cpp-ccls--default-template)
          (write-file ccls-file))
        (message ".ccls created")))))
