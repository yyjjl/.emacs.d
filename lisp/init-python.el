(setvar! python-has-pytest-p (executable-find "pytest")
         python-has-ipython-p (executable-find "ipython3")
         python-has-pylint-path (executable-find "pylint"))

(require-packages!
 (elpy :archive "melpa-stable")
 py-isort)



(define-hook! python|setup (python-mode-hook)
  (local-set-key (kbd "C-c b") 'elpy-autopep8-fix-code)
  (local-set-key (kbd "C-c B") 'py-isort-buffer)
  (local-set-key (kbd "C-c M-d") 'python/generate-doc-at-point)
  ;; emacs 24.4 only
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  (unless (buffer-temporary?)
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (semantic-idle-summary-mode -1)
    (elpy-mode 1)))

(with-eval-after-load 'py-isort
  (setq py-isort-options '("--lines=100")))

(with-eval-after-load 'elpy
  (remap! "C-c C-r" "C-c r" elpy-mode-map)
  (setcar elpy-test-discover-runner-command "python3")
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python3"
        elpy-modules (delete 'elpy-module-django
                             (delete 'elpy-module-flymake elpy-modules))
        elpy-test-runner 'elpy-test-pytest-runner)
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")

  (define-key! :map elpy-mode-map
    ("C-c C-n" . nil)
    ("C-c C-p" . nil)
    ("M-i" . python/multiedit-symbol-at-point))

  (defun python*elpy-multiedit-stop-hack ()
    (when-let ((buffer (get-buffer "*Elpy Edit Usages*"))
               (window (get-buffer-window buffer)))
      (kill-buffer buffer)))

  (advice-add 'elpy-multiedit-stop :after #'python*elpy-multiedit-stop-hack))



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

(defun python/multiedit-symbol-at-point ()
  "Edit all usages of the the Python symbol at point."
  (interactive)
  (cond
   ((or current-prefix-arg
        (use-region-p)
        (bound-and-true-p iedit-mode))
    (call-interactively 'iedit-mode))
   (elpy-multiedit-overlays
    (elpy-multiedit-stop))
   (t
    (save-some-buffers)
    (let ((usages (condition-case err
                      (elpy-rpc-get-usages)
                    ;; This is quite the stunt, but elisp parses JSON
                    ;; null as nil, which is indistinguishable from
                    ;; the empty list, we stick to the error.
                    (error
                     (if (and (eq (car err) 'error)
                              (stringp (cadr err))
                              (string-match "not implemented" (cadr err)))
                         'not-supported
                       (error (cadr err)))))))
      (cond
       ((eq usages 'not-supported)
        (call-interactively 'iedit-mode)
        (message "`get_usages' is not supported"))
       ((null usages)
        (call-interactively 'iedit-mode)
        (message "No usages of the symbol at point found"))
       (t
        (save-restriction
          (widen)
          (elpy-multiedit--usages usages))))))))

(defun python/generate-doc ($params $indent)
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
              (python/generate-doc params indent)
              "\n" indent
              "\"\"\""))))

(with-eval-after-load 'pyvenv
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name
          ("Py:" pyvenv-virtual-env-name " "))))

(with-eval-after-load 'python
  (when (boundp 'python-shell-completion-native-disabled-interpreters)
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "jupyter")
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "python3"))

  (setq python-shell-prompt-detect-failure-warning nil)

  (when python-has-pylint-path
    (setq python-check-command python-has-pylint-path))

  (elpy-enable)
  (remove-hook 'python-mode-hook 'elpy-mode)

  (define-key! :map inferior-python-mode-map
    ("C-c C-t" . python/toggle-pdbtrack)
    ([f5] . python/toggle-pdbtrack))

  (define-key! :map python-mode-map
    ([f5] . python/debug-current-file)
    ("M-p" . flycheck-previous-error)
    ("M-n" . flycheck-next-error))

  (defun python/debug-current-file ()
    (interactive)
    (unless (featurep 'gud)
      (require 'gud nil :noerror))
    (pdb (read-from-minibuffer "Run pdb: "
                               (format "pdb3 %s" (buffer-file-name))
                               gud-minibuffer-local-map
                               nil
                               (gud-symbol 'history nil 'pdb))))

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

  (define-hook! python|python-inferior-setup (inferior-python-mode-hook)
    (remove-hook 'comint-output-filter-functions
                 #'python-pdbtrack-comint-output-filter-function)))

(provide 'init-python)
