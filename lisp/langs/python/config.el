;;; -*- lexical-binding: t; -*-

(ymacs-editor//set-forward-sexp-handler
 :modes (python-mode)
 :forward #'python-nav-forward-statement)

(eval-when-has-feature! lsp
  (ymacs-editor//add-toggles
   "Python"
   '(or (derived-mode-p 'python-mode)
        (derived-mode-p 'python-ts-mode))
   '("v" ymacs-python/create-venv "Create Venv" :exit t)
   '("w" ymacs-python/create-venv-in-workon-home "Create Venv $WORKON-HOME" :exit t)
   '("c" ymacs-python/change-lsp-server "Change LSP Server" :exit t)
   '("t" ymacs-python/toggle-breakpoint "Toggle Breakpoint" :exit t))

  (after! lsp-pyls
    (setq lsp-pyls-configuration-sources ["flake8"])
    (setq lsp-pyls-plugins-rope-completion-enabled nil)

    (ymacs-lsp//set-simple-install-fn
     'pyls
     '("pip3" "install" "--user" "'python-language-server[all]'")
     '("pip3" "install" "--user" "-U" "'python-language-server[all]'")))

  (after! lsp-pyright
    (setq lsp-pyright-python-executable-cmd "python3")
    (setq lsp-pyright-typechecking-mode "off")))

(after! python
  (pyvenv-mode 1)

  (define-key! :map comint-mode-map
    ([f5] . ymacs-python/toggle-pdbtrack))

  (define-key! :map inferior-python-mode-map
    ([remap python-shell-completion-complete-or-indent] . ymacs-python/shell-completion-complete-or-indent)
    ("C-c C-t" . ymacs-python/toggle-pdbtrack)
    (("C-c C-z" "C-c z") . ymacs-python/pop-to-source-buffer))

  (dolist (keymap (list python-mode-map python-ts-mode-map))
    (define-key! :map keymap
      ("C-c e" . ymacs-python/extract-expression)
      ("C-c v" . ymacs-python/create-venv)
      ("C-c V" . ymacs-python/create-venv-in-workon-home)
      (("C-c C-z" "C-c z") . ymacs-python/pop-to-shell)
      ("C-c b" . py-isort-buffer)
      ("C-c C-b" . ymacs-python/autopep8)
      (("C-c C-c" "C-c c") . ymacs-python/send-buffer)
      (("C-c T" "C-c t") . ymacs-python/toggle-breakpoint)
      ("M-p" . previous-error)
      ("M-n" . next-error)))

  (after! ffap
    (setq ffap-alist (delq (assoc 'python-mode ffap-alist) ffap-alist)))

  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python")

  (setq python-shell-prompt-detect-failure-warning t)
  (setq python-pdbtrack-activate t)

  (if ymacs-ipython3-path
      (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "--simple-prompt")
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))

  (when ymacs-pylint-path
    (setq python-check-command ymacs-pylint-path)))
