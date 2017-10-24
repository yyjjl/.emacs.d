(setvar! python-has-pytest-p (executable-find "pytest")
         python-has-ipython-p (executable-find "ipython3"))

(require-packages!
 elpy 
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
  (elpy-use-cpython "python3")
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python3"
        elpy-modules (delete 'elpy-module-django
                             (delete 'elpy-module-flymake elpy-modules))
        elpy-test-runner 'elpy-test-pytest-runner)
  (define-key! :map elpy-mode-map
    ("C-c C-n" . nil)
    ("C-c C-p" . nil)))



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

(defun python/get-class-defs ()
  (interactive)
  (let* ((indent (make-string python-indent-offset
                              (string-to-char " "))))
    (save-excursion
      (forward-line 1)
      (if (re-search-backward "^\\( *\\)class.+\n")
          (progn
            (goto-char (match-end 0))
            (let* ((extra-indent (match-string 1))
                   (re (format "^%s%sdef +\\([^(]+\\)" extra-indent indent))
                   bound defs)
              (when (re-search-forward
                     (format "\\(^%s%s\\(.+\\)\\|\n\\)*" extra-indent indent)
                     nil nil)
                (setq bound (match-end 0))
                (goto-char (match-beginning 0))
                (while (re-search-forward re bound t)
                  (add-to-list 'defs (match-string 1))))
              (kill-new (concat (string-join defs " ")))
              (message "copy => all defs")))
        (message "Can not find where is the class")))))

(with-eval-after-load 'python
  (when (boundp 'python-shell-completion-native-disabled-interpreters)
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "jupyter")
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "python3"))
  (setq python-shell-prompt-detect-failure-warning nil)
  (elpy-enable)
  (remove-hook 'python-mode-hook 'elpy-mode)

  (define-hook! python|python-inferior-setup (inferior-python-mode-hook)
    (remove-hook 'comint-output-filter-functions
                 #'python-pdbtrack-comint-output-filter-function)))

(provide 'init-python)
