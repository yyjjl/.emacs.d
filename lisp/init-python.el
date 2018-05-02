(setvar! python-has-pytest-p (executable-find "pytest")
         python-has-ipython-p (executable-find "ipython3")
         python-has-pylint-path (executable-find "pylint"))

(require-packages!
 (elpy :archive "melpa-stable")
 gud
 py-isort)



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

(define-hook! python|setup (python-mode-hook)
  ;; emacs 24.4 only
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  (unless (and (buffer-temporary?)
               (not (eq major-mode 'python-mode)))
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (semantic-idle-summary-mode -1)
    (elpy-mode 1)))



(with-eval-after-load 'pyvenv
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name
          ("[" pyvenv-virtual-env-name "] "))))

(with-eval-after-load 'python
  (when (boundp 'python-shell-completion-native-disabled-interpreters)
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "jupyter")
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "python3"))

  (setq python-shell-prompt-detect-failure-warning nil)

  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")

  (with-eval-after-load 'flycheck
    ;; (setq flycheck-python-pylint-executable "python3")
    (setq flycheck-python-flake8-executable "python3")
    (setq flycheck-python-pycompile-executable "python3"))

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

  (define-hook! python|python-inferior-setup (inferior-python-mode-hook)
    (remove-hook 'comint-output-filter-functions
                 #'python-pdbtrack-comint-output-filter-function)))

(with-eval-after-load 'py-isort
  (setq py-isort-options '("--lines=75" "--multi-line=1")))

(with-eval-after-load 'elpy
  (put 'elpy-shell-use-project-root 'safe-local-variable #'booleanp)
  (remap! "C-c C-r" "C-c r" elpy-mode-map)
  (setcar elpy-test-discover-runner-command "python3")
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python3"
        elpy-modules (delete 'elpy-module-django
                             (delete 'elpy-module-flymake elpy-modules))
        elpy-test-runner 'elpy-test-pytest-runner)

  (define-key! :map elpy-mode-map
    ("C-c C-n" . nil)
    ("C-c C-p" . nil)
    ("C-c b" . elpy-autopep8-fix-code)
    ("C-c B" . py-isort-buffer)
    ("C-c M-d" . python/generate-doc-at-point)
    ;; ("M-i" . python/multiedit-symbol-at-point)
    ("M-i" . elpy-multiedit-python-symbol-at-point))

  (defvar python--elpy-mutiedit-overlay-map
    (define-key! :map (make-sparse-keymap)
      ("TAB" . python/elpy-multiedit-next-overlay)
      ("<tab>" . python/elpy-multiedit-next-overlay)
      ("<backtab>" . python/elpy-multiedit-previous-overlay)
      ([remap keyboard-escape-quit] . elpy-multiedit-stop)
      ([remap keyboard-quit] . elpy-multiedit-stop)))

  (defvar python--elpy-multiedit-buffers nil)

  (defun python//elpy-multiedit-jump-overlay ($buffer &optional $pos $backward-p)
    (switch-to-buffer $buffer)
    (unless $pos
      (setq $pos (if $backward-p (point-max) (point-min))))
    (let* ((property-fn (if $backward-p
                            'previous-single-char-property-change
                          'next-single-char-property-change))
           (pos (funcall property-fn
                         (if (get-char-property $pos 'elpy-multiedit-overlay)
                             (funcall property-fn $pos 'elpy-multiedit-overlay)
                           $pos)
                         'elpy-multiedit-overlay))
           (buffers (if $backward-p
                        (reverse python--elpy-multiedit-buffers)
                      python--elpy-multiedit-buffers)))

      (if (or (and $backward-p (/= pos (point-min)))
              (and (not $backward-p) (/= pos (point-max))))
          (goto-char pos)
        (-when-let (next-buffer (or (cadr (member $buffer buffers))
                                    (car buffers)))
          (python//elpy-multiedit-jump-overlay next-buffer nil $backward-p)))))

  (defun python/elpy-multiedit-next-overlay ()
    (interactive)
    (python//elpy-multiedit-jump-overlay (current-buffer) (point)))

  (defun python/elpy-multiedit-previous-overlay ()
    (interactive)
    (python//elpy-multiedit-jump-overlay (current-buffer) (point) t))

  (defun python*elpy-multiedit-hack ($fn &optional $arg)
    (setq python--elpy-multiedit-buffers nil)
    (if (and (not elpy-multiedit-overlays)
             (or $arg iedit-mode))
        (call-interactively 'iedit-mode)
      (funcall $fn $arg)
      (dolist (ov elpy-multiedit-overlays)
        (-when-let (buffer (overlay-buffer ov))
          (unless (eq buffer (get-buffer "*Elpy Edit Usages*"))
            (overlay-put ov 'elpy-multiedit-overlay t)
            (overlay-put ov 'keymap python--elpy-mutiedit-overlay-map)
            (add-to-list 'python--elpy-multiedit-buffers (overlay-buffer ov)))))))

  (advice-add 'elpy-multiedit-python-symbol-at-point
              :around #'python*elpy-multiedit-hack)

  (defun python*elpy-multiedit-stop-hack ()
    (when-let ((buffer (get-buffer "*Elpy Edit Usages*"))
               (window (get-buffer-window buffer)))
      (kill-buffer buffer)))

  (advice-add 'elpy-multiedit-stop :after #'python*elpy-multiedit-stop-hack))

(provide 'init-python)
