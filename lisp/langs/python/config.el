;;; -*- lexical-binding: t; -*-

(ymacs-editor//set-forward-sexp-handler
 :modes (python-mode)
 :forward #'python-nav-forward-statement)

(after! python
  (define-key! :map comint-mode-map
    ([f5] . ymacs-python/toggle-pdbtrack))

  (define-key! :map inferior-python-mode-map
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
      ([f9] . ymacs-python/run-current-file)
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
