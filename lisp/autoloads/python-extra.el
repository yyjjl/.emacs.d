;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun python/autopep8 ()
  (interactive)
  (save-restriction
    (widen)
    (py-autopep8-buffer)))

;;;###autoload
(defun python/debug-current-file ()
  (interactive)
  (unless (featurep 'gud)
    (require 'gud nil :noerror))
  (pdb (read-from-minibuffer "Run pdb: "
                             (format "pdb3 %s" (buffer-file-name))
                             gud-minibuffer-local-map
                             nil
                             (gud-symbol 'history nil 'pdb))))

;;;###autoload
(defun python/toggle-pdbtrack ()
  (interactive)
  (if (memq 'python-pdbtrack-comint-output-filter-function
            comint-output-filter-functions)
      (progn
        (remove-hook 'comint-output-filter-functions
                     #'python-pdbtrack-comint-output-filter-function)
        (message "pdbtrack disabled"))
    (add-hook 'comint-output-filter-functions
              #'python-pdbtrack-comint-output-filter-function)
    (message "pdbtrack enabled")))

;;;###autoload
(defun python/create-venv-in-workon-home ($name $python-exe &optional $args)
  (interactive
   (list (read-from-minibuffer "Name: ")
         (read-shell-command "Python executable: ")
         (when current-prefix-arg
           (read-from-minibuffer "Arguments: "))))
  (when (and $name $python-exe (> (length $name) 0))
    (let* ((dir (expand-file-name $name (pyvenv-workon-home)))
           (command (format "virtualenv --python=%s %s %s\n"
                            $python-exe dir (or $args ""))))
      (compile command))))

;;;###autoload
(defun python/create-virtualenv ($dir $python-exe &optional $args)
  (interactive
   (list (read-directory-name "Directory: "
                              (ignore-errors (projectile-project-root))
                              nil :mustmatch)
         (read-shell-command "Python executable: ")
         (when current-prefix-arg
           (read-from-minibuffer "Arguments: "))))
  (when (and $dir $python-exe)
    (let* ((dir (expand-file-name ".venv" $dir))
           (command (format "virtualenv --python=%s %s %s\n"
                            $python-exe dir (or $args ""))))
      (compile command))))

;;;###autoload
(defun python//generate-doc ($params $indent)
  (setq $indent (concat "\n" $indent))
  (string-join (mapcar (lambda (token)
                         (let ((param (split-string token "=" t " +"))
                               default)
                           (setq default (cadr param))
                           (setq param (car param))
                           (concat "@param " param
                                   (if default
                                       (format " (default: %s): " default)
                                     ": ")
                                   $indent "@type " param ": ")))
                       (split-string $params "," t " +"))
               $indent))

;;;###autoload
(defun python/generate-doc-at-point ()
  (interactive)
  (let (params indent)
    (save-excursion
      (if (re-search-backward
           "^\\( *\\)def[^(]+(\\([^\n]*\\)): *$" nil t)
          (progn
            (setq params (match-string-no-properties 2))
            (setq indent (concat (match-string-no-properties 1)
                                 (make-string python-indent-offset
                                              (string-to-char " ")))))
        (message "Can not find `def'")))
    (when params
      (insert "\"\"\""
              "\n" indent
              (python//generate-doc params indent)
              "\n" indent
              "\"\"\""))))
