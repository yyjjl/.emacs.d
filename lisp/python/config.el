;;; -*- lexical-binding: t; -*-

(after! lsp-pyls
  (setq lsp-pyls-configuration-sources ["flake8"])
  (setq lsp-pyls-plugins-rope-completion-enabled nil)

  (ymacs-lsp//set-simple-install-fn
   'pyls
   "pip3 install --user 'python-language-server[all]'"))

(after! lsp-pyright
  (setq lsp-pyright-python-executable-cmd "python3")
  (setq lsp-pyright-typechecking-mode "off"))

(after! python
  (define-key! :map comint-mode-map
    ([f5] . ymacs-python/toggle-pdbtrack))

  (define-key! :map inferior-python-mode-map
    ("C-c C-t" . ymacs-python/toggle-pdbtrack)
    ("C-c C-z" . ymacs-python/pop-to-source-buffer))

  (define-key! :map python-mode-map
    ([f5] . ymacs-python/debug-current-file)
    ("C-c v" . ymacs-python/create-venv)
    ("C-c V" . ymacs-python/create-venv-in-workon-home)
    ("C-c C-z" . ymacs-python/pop-to-shell)
    ("C-c C-b" . py-isort-buffer)
    ("C-c b" . ymacs-python/autopep8)
    ("C-c C-c" . ymacs-python/send-buffer)
    ("C-c M-d" . ymacs-python/generate-doc-at-point)
    ("C-c T" . ymacs-python/toggle-breakpoint)
    ("M-p" . previous-error)
    ("M-n" . next-error))

  (after! ffap
    (setq ffap-alist
          (delq (assoc 'python-mode ffap-alist) ffap-alist)))

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
    (setq python-check-command ymacs-pylint-path)))
