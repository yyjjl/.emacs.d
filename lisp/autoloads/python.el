;;; -*- lexical-binding: t; -*-

(defvar python--elpy-multiedit-buffers nil)

;;;###autoload
(defun python/autopep8 ()
  (interactive)
  (save-restriction
    (widen)
    (py-autopep8-buffer)))

;;;###autoload
(defun python/debug-current-file (&optional directory)
  (interactive
   (list
    (or (and current-prefix-arg default-directory)
        (expand-file-name
         (read-directory-name "Directory: " nil nil :must-match)))))
  (let ((default-directory directory))
    (cond
     ((require 'realgud nil :noerror)
      (realgud:pdb
       (read-shell-command "Run pdb like this: "
                           (ignore-errors (car realgud:pdb-minibuffer-history))
                           'realgud:pdb-minibuffer-history)))
     ((require 'gud nil :noerror)
      (call-interactively #'pdb)))))

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
(defun python/create-venv-in-workon-home (-name -python-exe &optional -args)
  (interactive
   (list (read-from-minibuffer "Name: ")
         (read-shell-command "Python executable: ")
         (when current-prefix-arg
           (read-from-minibuffer "Arguments: "))))
  (when (and -name -python-exe (> (length -name) 0))
    (let* ((dir (expand-file-name -name (pyvenv-workon-home)))
           (command (format "virtualenv --python=%s %s %s\n"
                            -python-exe dir (or -args ""))))
      (compile command))))

;;;###autoload
(defun python/create-virtualenv (-dir -python-exe &optional -args)
  (interactive
   (list (read-directory-name "Directory: "
                              (ignore-errors (projectile-project-root))
                              nil :mustmatch)
         (read-shell-command "Python executable: ")
         (when current-prefix-arg
           (read-from-minibuffer "Arguments: "))))
  (when (and -dir -python-exe)
    (let* ((dir (expand-file-name ".venv" -dir))
           (command (format "virtualenv --python=%s %s %s\n"
                            -python-exe dir (or -args ""))))
      (compile command))))

;;;###autoload
(defun python//generate-doc (-params -indent)
  (setq -indent (concat "\n" -indent))
  (string-join (mapcar (lambda (token)
                         (let ((param (split-string token "=" t " +"))
                               default)
                           (setq default (cadr param))
                           (setq param (car param))
                           (concat "@param " param
                                   (if default
                                       (format " (default: %s): " default)
                                     ": ")
                                   -indent "@type " param ": ")))
                       (split-string -params "," t " +"))
               -indent))

;;;###autoload
(defun python/generate-doc-at-point ()
  (interactive)
  (let (params indent insert-point)
    (save-excursion
      (if (re-search-backward
           "^\\( *\\)def[^(]+(\\([^\n]*\\))[^:]*: *$" nil t)
          (progn
            (setq params (match-string-no-properties 2))
            (setq indent (concat (match-string-no-properties 1)
                                 (make-string python-indent-offset
                                              (string-to-char " "))))
            (setq insert-point (point)))
        (message "Can not find `def'")))
    (when params
      (goto-char insert-point)
      (forward-line)
      (insert indent "\"\"\""
              "\n" indent
              (python//generate-doc params indent)
              "\n" indent
              "\"\"\""))))

(defun python//elpy-multiedit-jump-overlay (-buffer &optional -pos -backward-p)
  (switch-to-buffer -buffer)
  (unless -pos
    (setq -pos (if -backward-p (point-max) (point-min))))
  (let* ((property-fn (if -backward-p
                          'previous-single-char-property-change
                        'next-single-char-property-change))
         (pos (funcall property-fn
                       (if (get-char-property -pos 'elpy-multiedit-overlay)
                           (funcall property-fn -pos 'elpy-multiedit-overlay)
                         -pos)
                       'elpy-multiedit-overlay))
         (buffers (if -backward-p
                      (reverse python--elpy-multiedit-buffers)
                    python--elpy-multiedit-buffers)))

    (if (or (and -backward-p (/= pos (point-min)))
            (and (not -backward-p) (/= pos (point-max))))
        (goto-char pos)
      (-when-let (next-buffer (or (cadr (member -buffer buffers))
                                  (car buffers)))
        (python//elpy-multiedit-jump-overlay next-buffer nil -backward-p)))))

;;;###autoload
(defun python/elpy-multiedit-next-overlay ()
  (interactive)
  (python//elpy-multiedit-jump-overlay (current-buffer) (point)))

;;;###autoload
(defun python/elpy-multiedit-previous-overlay ()
  (interactive)
  (python//elpy-multiedit-jump-overlay (current-buffer) (point) t))

;;;###autoload
(defun python/pop-to-shell (&optional -arg)
  (interactive "P")
  (let ((elpy-shell-use-project-root (not -arg)))
    (elpy-shell-switch-to-shell)))
