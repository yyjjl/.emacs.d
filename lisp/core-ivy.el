;; -*- lexical-binding: t -*-

(defun core//package-install-transformer (-string)
  (let ((package (cadr (assoc-string -string package-archive-contents))))
    (format "%-30s %-16s %-7s %s"
            -string
            (if package
                (propertize (package-version-join (package-desc-version package))
                            'face font-lock-comment-face)
              "")
            (if package
                (propertize (package-desc-archive package)
                            'face font-lock-builtin-face)
              "")
            (if package
                (propertize (package-desc-summary package)
                            'face font-lock-doc-face)
              ""))))

(defun core//counsel-find-file-transformer (-string)
  (concat
   (ivy-read-file-transformer -string)
   (propertize
    (let* ((default-directory ivy--directory)
           (type (-> -string
                     expand-file-name
                     directory-file-name
                     file-attributes
                     car)))
      (if (stringp type)
          (concat "-> " (expand-file-name type))
        ""))
    'face 'font-lock-doc-face)))

(defun core//ivy-switch-buffer-transformer (-string)
  "Transform STR to more readable format."
  (let ((buffer (get-buffer -string)))
    (cond
     (buffer
      (format "%-60s %s" -string (buffer-local-value 'default-directory buffer)))
     ((and (eq ivy-virtual-abbreviate 'full)
           (file-name-directory -string))
      (format "%-60s %s" (propertize (file-name-nondirectory -string)
                                     'face 'ivy-virtual)
              (file-name-directory -string)))
     (t -string))))

(defun core//counsel-bookmark-transformer (-string)
  "Transform STR to more readable format."
  (let ((bm (bookmark-get-bookmark-record -string)))
    (format "%-40s %s %s"
            (propertize -string 'face 'font-lock-string-face)
            (if bm
                (propertize (format "%-10d" (cdr (assoc 'position bm)))
                            'face 'warning)
              "")
            (if bm (file-name-nondirectory (cdr (assoc 'filename bm))) ""))))

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
    (ivy-set-display-transformer caller #'core//ivy-switch-buffer-transformer))

  (ivy-set-display-transformer 'package-install #'core//package-install-transformer)
  (ivy-set-display-transformer 'counsel-find-file
                               #'core//counsel-find-file-transformer)
  (ivy-set-display-transformer 'counsel-bookmark
                               #'core//counsel-bookmark-transformer)

  (advice-add 'ivy--preselect-index :around #'ignore-errors!)

  (setq ivy-read-action-function #'ivy-hydra-read-action)

  (setq ivy-action-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories '("./"))
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
    ("C-r" . ivy-reverse-i-search)
    ("C-j" . ivy-immediate-done)
    ("C-M-j" . ivy-done)
    ("M-." . ignore)))

(with-eval-after-load 'swiper
  (setq swiper-stay-on-quit t))

(with-eval-after-load 'counsel
  (define-key!
    ("C-x j j" . counsel-bookmark)
    ("C-s" . swiper/dispatch)
    ("C-r" . swiper-isearch-backward)
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
    ("j" . counsel/rg-file-jump)
    ("p" . counsel/file-jump)
    ("g" . counsel-git)
    ("s" . counsel-git-grep)
    ("S" . counsel-git-stash)
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

  (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history))

(with-eval-after-load 'counsel-projectile
  (if emacs-use-ripgrep-p
      (progn
        (setq counsel-rg-base-command
              "rg -M 1000 -S --no-heading --line-number --color never %s .")
        (define-key projectile-command-map "ss" 'counsel-projectile-rg)
        (global-set-key (kbd "C-c i a") 'counsel/rg))
    (define-key projectile-command-map "ss" 'counsel-projectile-grep)
    (global-set-key (kbd "C-c i a") 'counsel-grep))

  (define-key! :map projectile-command-map
    ("s a" . counsel-projectile-ag)
    ("p" . counsel-projectile)
    ("K" . projectile-kill-buffers)
    ("w" . projectile-switch-project)))

(provide 'core-ivy)
