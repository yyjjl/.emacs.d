;;; -*- lexical-binding: t; -*-

(setvar! python-has-pytest-p (executable-find "pytest")
         python-has-ipython-p (executable-find "ipython3")
         python-has-pylint-path (executable-find "pylint"))

(require-packages!
 elpy
 gud
 py-autopep8
 py-isort)



(defvar python--elpy-mutiedit-overlay-map
  (define-key! :map (make-sparse-keymap)
    ("TAB" . python/elpy-multiedit-next-overlay)
    ("<tab>" . python/elpy-multiedit-next-overlay)
    ("<backtab>" . python/elpy-multiedit-previous-overlay)
    ([remap keyboard-escape-quit] . elpy-multiedit-stop)
    ([remap keyboard-quit] . elpy-multiedit-stop)))

(defun python*around-elpy-multiedit (-fn &optional -arg)
  (setq python--elpy-multiedit-buffers nil)
  (if (and (not elpy-multiedit-overlays)
           (or -arg (bound-and-true-p iedit-mode)))
      (call-interactively 'iedit-mode)
    (funcall -fn -arg)
    (dolist (ov elpy-multiedit-overlays)
      (-when-let (buffer (overlay-buffer ov))
        (unless (eq buffer (get-buffer "*Elpy Edit Usages*"))
          (overlay-put ov 'elpy-multiedit-overlay t)
          (overlay-put ov 'keymap python--elpy-mutiedit-overlay-map)
          (add-to-list 'python--elpy-multiedit-buffers (overlay-buffer ov)))))))

(defun python*after-elpy-multiedit-stop ()
  (when-let ((buffer (get-buffer "*Elpy Edit Usages*"))
             (window (get-buffer-window buffer)))
    (kill-buffer buffer)))

(define-hook! python|setup (python-mode-hook)
  ;; emacs 24.4 only
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  (electric-operator-mode 1)

  (when (file-remote-p default-directory)
    (setq-local python-shell-interpreter "python3")
    (setq-local python-shell-interpreter-args "-i"))

  (unless (or (buffer-temporary-p)
              (not (eq major-mode 'python-mode)))
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (semantic-idle-summary-mode -1)
    (elpy-mode 1)))

(with-eval-after-load 'pyvenv
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name (" [" pyvenv-virtual-env-name "]"))))

(with-eval-after-load 'python
  (require 'electric-operator)

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
    ("M-p" . previous-error)
    ("M-n" . next-error))

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
        elpy-modules (delq 'elpy-module-django
                           (delq 'elpy-module-flymake
                                 (delq 'elpy-module-highlight-indentation
                                       elpy-modules)))
        elpy-test-runner 'elpy-test-pytest-runner
        elpy-shell-echo-input nil)

  (define-key! :map elpy-mode-map
    ("C-c C-n" . nil)
    ("C-c C-p" . nil)
    ("C-c b" . python/autopep8)
    ("C-c B" . py-isort-buffer)
    ("C-c M-d" . python/generate-doc-at-point)
    ("M-i" . elpy-multiedit-python-symbol-at-point))

  (advice-add 'elpy-multiedit-python-symbol-at-point
              :around #'python*around-elpy-multiedit)

  (advice-add 'elpy-multiedit-stop :after #'python*after-elpy-multiedit-stop))

(provide 'init-python)
