;;; -*- lexical-binding: t; -*-

(after! lsp-pyls
  (setq lsp-pyls-configuration-sources ["flake8"])
  (setq lsp-pyls-plugins-rope-completion-enabled nil)

  (ymacs-lsp//set-simple-install-fn
   'pyls
   "pip3 install --user 'python-language-server[all]"))

(after! python
  (define-key! :map inferior-python-mode-map
    ("C-c C-t" . ymacs-python/toggle-pdbtrack)
    ("C-c C-z" . elpy-shell-switch-to-buffer)
    ([f5] . ymacs-python/toggle-pdbtrack))

  (define-key! :map python-mode-map
    ([f5] . ymacs-python/debug-current-file)
    ("C-c v" . ymacs-python/create-venv)
    ("C-c V" . ymacs-python/create-venv-in-workon-home)
    ("C-c P" . ymacs-python/profile-buffer)
    ("C-c C-z" . ymacs-python/pop-to-shell)
    ("C-c C-b" . py-isort-buffer)
    ("C-c b" . ymacs-python/autopep8)
    ("C-c C-c" . ymacs-python/send-buffer)
    ("C-c M-d" . ymacs-python/generate-doc-at-point)
    ("C-c T" . ymacs-python/toggle-breakpoint)
    ("M-p" . previous-error)
    ("M-n" . next-error))

  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python")

  (setq python-shell-prompt-detect-failure-warning nil)
  (setq python-pdbtrack-activate t)
  (setq flycheck-python-flake8-executable "python3")
  (setq flycheck-python-pycompile-executable "python3")

  (if ymacs-ipython3-path
      (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "--simple-prompt")
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))

  (when ymacs-pylint-path
    (setq python-check-command ymacs-pylint-path))

  (ymacs-python//elpy-enable))

(after! elpy
  (define-key! :map elpy-pdb-map
    (";" . elpy-pdb-toggle-breakpoint-at-point))
  (define-key! :map elpy-mode-map
    ("C-c C-z" . ymacs-python/pop-to-shell)
    ("C-c C-n")
    ("C-c C-p")
    ("C-c C-b")
    ("C-c ;" :map elpy-pdb-map)
    ("C-c C-c" . ymacs-python/send-buffer)
    ("C-c I" . elpy-nav-expand-to-indentation)
    ("C-c M-d" . ymacs-python/generate-doc-at-point)
    ("M-i" . elpy-multiedit-python-symbol-at-point))

  (remap! "C-c C-r" "C-c r" elpy-mode-map)
  (setcar elpy-test-discover-runner-command "python3")
  (setq elpy-rpc-python-command "python3"
        elpy-rpc-virtualenv-path 'system
        elpy-modules (delq 'elpy-module-django
                           (delq 'elpy-module-highlight-indentation
                                 elpy-modules))
        elpy-test-runner 'elpy-test-pytest-runner
        elpy-shell-echo-input nil))
