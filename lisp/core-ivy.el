(defun core//ivy-switch-buffer-transformer (-string)
  "Transform STR to more readable format."
  (let ((buffer (get-buffer -string)))
    (cond
     (buffer
      (format-line! -string (buffer-local-value 'default-directory buffer)))
     ((and (eq ivy-virtual-abbreviate 'full)
           (file-name-directory -string))
      (format-line! (propertize (file-name-nondirectory -string)
                                'face 'ivy-virtual)
                    (file-name-directory -string)))
     (t -string))))

(defun core//counsel-bookmark-transformer (-string)
  "Transform STR to more readable format."
  (format-line! (propertize -string 'face 'font-lock-string-face)
                (when-let (bm (bookmark-get-bookmark-record -string))
                  (concat (file-name-nondirectory (cdr (assoc 'filename bm)))
                          (propertize (format " %10d"
                                              (cdr (assoc 'position bm)))
                                      'face 'warning)))))

(defun counsel-projectile*before (&optional -arg)
  (when (eq -arg '(16))
    (setq projectile-cached-project-root nil)))

(define-hook! ivy|occur-mode-setup (ivy-occur-mode-hook
                                    ivy-occur-grep-mode-hook)
  (local-set-key "/" #'ivy-occur-filter-lines)
  (local-set-key (kbd "C-/") #'ivy-occur-undo)
  (toggle-truncate-lines 1))

(defvar ivy-views-persistent-file "ivy-views.el")
(with-eval-after-load 'ivy
  (core//load-variable 'ivy-views ivy-views-persistent-file)

  (add-hook 'kill-emacs-hook
            (lambda ()
              (core//save-variable 'ivy-views ivy-views-persistent-file)))

  (dolist (caller '(ivy-switch-buffer
                    counsel/kill-buffer
                    internal-complete-buffer
                    ivy-switch-buffer-other-window
                    counsel-projectile-switch-to-buffer
                    counsel-projectile))
    (ivy-set-display-transformer caller
                                 #'core//ivy-switch-buffer-transformer))

  (ivy-set-display-transformer 'counsel-bookmark
                               #'core//counsel-bookmark-transformer)

  (advice-add 'ivy--preselect-index :around #'ignore-errors!)

  (require 'ivy-hydra)

  ;; (add-to-list 'ivy-display-functions-alist
  ;;              '(counsel-company . ivy-display-function-overlay))

  (setq ivy-action-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories '("./"))
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-dispatching-done-columns 3)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-re-builders-alist
        '(;; Use regex as default
          (t . ivy--regex-plus)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-use-selectable-prompt t)

  (define-key!
    :map ivy-minibuffer-map
    ("C-j" . ivy-immediate-done)
    ("C-M-j" . ivy-done)
    ("M-." . ignore)))

(with-eval-after-load 'counsel
  (define-key!
    ("C-x j j" . counsel-bookmark)
    ("C-s" . swiper/dispatch)
    ("C-x C-f" . counsel-find-file)
    ("C-x k" . counsel/kill-buffer)
    ("C-x b" . ivy-switch-buffer)
    ("C-x w -" . ivy-pop-view)
    ("C-x w =" . ivy-push-view))

  (define-key! :prefix "C-c i"
    ("r" . ivy-resume)
    ("l l" . counsel-load-library)
    ("l t" . counsel-load-theme)
    ("l p" . counsel-list-processes)
    ("l f" . counsel-find-library)
    ("u" . counsel-unicode-char)
    ("d" . counsel-dired-jump)
    ("i" . counsel/semantic-or-imenu*)
    ("x" . counsel-linux-app)
    ("v" . counsel-set-variable)
    ("j" . counsel-file-jump)
    ("p" . counsel-git)
    ("g" . counsel-git-grep)
    ("s" . counsel-git-stash)
    ("h" . counsel-minibuffer-history)
    ("m" . counsel-mark-ring)
    ("/" . counsel-grep)
    ("L" . counsel-locate)
    ("f" . counsel-describe-face)
    ("S" . counsel-find-symbol)
    ("F" . counsel-faces)
    ("W" . counsel-colors-web)
    ("E" . counsel-colors-emacs)
    ("e" . counsel/sudo-edit)
    ("O" . counsel-outline)
    ("o" . counsel-org-goto-all)
    ("t" . counsel-tmm))

  (setq counsel-linux-app-format-function #'counsel-linux-app-format-function-name-first)

  (setq counsel-yank-pop-separator
        "\n------------------------------------------------------------\n")
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))

  (ivy-set-actions
   'counsel-find-file
   `(("x"
      (lambda (x) (delete-file (expand-file-name x ivy--directory)))
      ,(propertize "delete" 'face 'font-lock-warning-face))))
  (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history))

(with-eval-after-load 'counsel-projectile
  (advice-add 'counsel-projectile :before #'counsel-projectile*before)

  (if env-has-ripgrep-p
      (progn
        (setq counsel-grep-base-command
              "rg -M 1000 -S --no-heading --line-number --color never %s %s")
        (define-key projectile-command-map "ss" 'counsel-projectile-rg)
        (global-set-key (kbd "C-c i a") 'counsel-rg))
    (define-key projectile-command-map "ss" 'counsel-projectile-grep)
    (global-set-key (kbd "C-c i a") 'counsel-grep))

  (define-key! :map projectile-command-map
    ("s a" . counsel-projectile-ag)
    ("p" . counsel-projectile)
    ("K" . projectile-kill-buffers)
    ("w" . projectile-switch-project)))

(provide 'core-ivy)
