;;; -*- lexical-binding: t; -*-

(define-variable! :pkg python pytest ipython pylint pyls)

(require-packages!
 lsp-mode
 elpy
 py-autopep8
 py-isort)

(eval-when-compile
  (require 'lsp-pyls))


(defvar python--elpy-multiedit-buffers nil)
(defvar python--elpy-mutiedit-overlay-map
  (define-key! :map (make-sparse-keymap)
    ("TAB" . python/elpy-multiedit-next-overlay)
    ("<tab>" . python/elpy-multiedit-next-overlay)
    ("<backtab>" . python/elpy-multiedit-previous-overlay)
    ([remap keyboard-escape-quit] . elpy-multiedit-stop)
    ([remap keyboard-quit] . elpy-multiedit-stop)))

(defun python//shell-running-p ()
  (when-let* ((process (or (and (derived-mode-p 'inferior-python-mode)
                                (get-buffer-process (current-buffer)))
                           (python-shell-get-process))))
    (with-current-buffer (process-buffer process)
      (not comint-last-prompt))))

(config! lsp-pyls
  :config
  (setq lsp-pyls-configuration-sources ["flake8"])
  (setq lsp-pyls-plugins-rope-completion-enabled nil))

(config! pyvenv
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name (" [" pyvenv-virtual-env-name "]"))))

(config! python
  :bind
  (:map inferior-python-mode-map
   ("C-c C-t" . python/toggle-pdbtrack)
   ([f5] . python/toggle-pdbtrack))
  (:map python-mode-map
   ([f5] . python/debug-current-file)
   ("C-c C-z" . python/pop-to-shell)
   ("C-c C-b" . py-isort-buffer)
   ("C-c b" . python/autopep8)
   ("C-c C-c" . python/send-buffer)
   ("C-c M-d" . python/generate-doc-at-point)
   ("C-c T" . python/toggle-breakpoint)
   ("M-p" . previous-error)
   ("M-n" . next-error))

  :advice
  (:around python-ffap-module-path
   :define (-fn &rest -args)
   (unless (python//shell-running-p)
     (apply -fn -args)))

  :hook
  (setup
   :define (python-mode-hook)
   (setq electric-indent-chars (delq ?: electric-indent-chars))

   (when (file-remote-p default-directory)
     (setq-local python-shell-interpreter "python3")
     (setq-local python-shell-interpreter-args "-i"))

   (when (and (buffer-enable-rich-feature-p)
              (eq major-mode 'python-mode))
     (semantic-idle-summary-mode -1)
     (lsp//try-enable python|setup
       :fallback
       (progn
         (elpy-mode 1)
         (company//add-backend 'elpy-company-backend)))))

  (inferior-setup
   :define (inferior-python-mode-hook)
   (remove-hook 'comint-output-filter-functions
                #'python-pdbtrack-comint-output-filter-function))

  :config
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python")

  (setq python-shell-prompt-detect-failure-warning nil)
  (setq python-pdbtrack-activate t)
  (setq flycheck-python-flake8-executable "python3")
  (setq flycheck-python-pycompile-executable "python3")

  (if python-use-ipython-p
      (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "--simple-prompt")
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))

  (when python-use-pylint-p
    (setq python-check-command python-use-pylint-p))

  ;; Setup elpy or pyls
  (if python-use-pyls-p
      (progn
        ;; init some parts of elpy
        (require 'elpy)

        (add-hook 'inferior-python-mode-hook 'elpy-shell--enable-output-filter)
        (let ((elpy-modules '(elpy-module-sane-defaults
                              elpy-module-yasnippet
                              elpy-module-pyvenv)))
          (elpy-modules-global-init)))
    (elpy-enable)
    (remove-hook 'python-mode-hook 'elpy-mode)))

(config! elpy
  :bind
  (:map elpy-pdb-map
   (";" . elpy-pdb-toggle-breakpoint-at-point))
  (:map elpy-mode-map
   ("C-c C-z" . python/pop-to-shell)
   ("C-c C-n")
   ("C-c C-p")
   ("C-c C-b")
   ("C-c ;" :map elpy-pdb-map)
   ("C-c C-c" . python/send-buffer)
   ("C-c I" . elpy-nav-expand-to-indentation)
   ("C-c M-d" . python/generate-doc-at-point)
   ("M-i" . elpy-multiedit-python-symbol-at-point))

  :advice
  (:around elpy-multiedit-python-symbol-at-point
   :define (-fn &optional -arg)
   (setq python--elpy-multiedit-buffers nil)
   (cond
    ((bound-and-true-p multiple-cursors-mode)
     (multiple-cursors-mode -1))
    ((and (not elpy-multiedit-overlays)
          (or -arg
              (buffer-narrowed-p)
              (bound-and-true-p iedit-mode)))
     (call-interactively 'iedit-mode))
    (t
     (funcall -fn -arg)
     (dolist (ov elpy-multiedit-overlays)
       (-when-let (buffer (overlay-buffer ov))
         (unless (eq buffer (get-buffer "*Elpy Edit Usages*"))
           (overlay-put ov 'elpy-multiedit-overlay t)
           (overlay-put ov 'keymap python--elpy-mutiedit-overlay-map)
           (add-to-list 'python--elpy-multiedit-buffers (overlay-buffer ov))))))))

  (:after elpy-multiedit-stop
   :define ()
   (when-let ((buffer (get-buffer "*Elpy Edit Usages*"))
              (window (get-buffer-window buffer)))
     (kill-buffer buffer)))

  :config
  (remap! "C-c C-r" "C-c r" elpy-mode-map)
  (setcar elpy-test-discover-runner-command "python3")
  (setq elpy-rpc-python-command "python3"
        elpy-rpc-virtualenv-path 'system
        elpy-modules (delq 'elpy-module-django
                           (delq 'elpy-module-highlight-indentation
                                 elpy-modules))
        elpy-test-runner 'elpy-test-pytest-runner
        elpy-shell-echo-input nil))

(put 'elpy-shell-use-project-root 'safe-local-variable #'booleanp)

(provide 'init-python)
