(setvar! python-has-pytest-p (executable-find "pytest")
         python-has-ipython-p (executable-find "ipython3")
         python-has-pylint-path (executable-find "pylint"))

(require-packages!
 elpy
 gud
 py-autopep8
 py-isort)



(define-hook! python|setup (python-mode-hook)
  ;; emacs 24.4 only
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  (electric-operator-mode 1)

  (when (file-remote-p default-directory)
    (setq-local python-shell-interpreter "python3")
    (setq-local python-shell-interpreter-args "-i"))

  (unless (or (buffer-temporary?)
              (not (eq major-mode 'python-mode)))
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (semantic-idle-summary-mode -1)
    (elpy-mode 1)))

(with-eval-after-load 'pyvenv
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name (" [" pyvenv-virtual-env-name "]"))))

(with-eval-after-load 'python
  (when (boundp 'python-shell-completion-native-disabled-interpreters)
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "jupyter")
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "python3")
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "python"))

  (setq python-shell-prompt-detect-failure-warning nil)

  (if python-has-ipython-p
      (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "--simple-prompt")
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))

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
  (remap! "C-c C-r" "C-c r" elpy-mode-map)
  (setcar elpy-test-discover-runner-command "python3")
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python3"
        elpy-modules (delete 'elpy-module-django
                             (delete 'elpy-module-flymake elpy-modules))
        elpy-test-runner 'elpy-test-pytest-runner
        elpy-shell-echo-input nil)

  (define-key! :map elpy-mode-map
    ("C-c C-n" . nil)
    ("C-c C-p" . nil)
    ("C-c b" . python/autopep8)
    ("C-c B" . py-isort-buffer)
    ("C-c M-d" . python/generate-doc-at-point)
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
             (or $arg (bound-and-true-p iedit-mode)))
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
