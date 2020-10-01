;; -*- lexical-binding: t -*-

(after! ivy
  (ymacs//load-variable 'ivy-views ymacs-ivy-views-persistent-file)

  (define-key! :map ivy-minibuffer-map
    ("C-r" . ivy-reverse-i-search)
    ("C-j" . ivy-immediate-done)
    ("C-M-j" . ivy-done)
    ("M-." . ignore)
    ("<C-return>" . ymacs-ivy/switch))

  (ymacs-ivy//define-switch swiper
    (swiper "Search" "S")
    (swiper-isearch "Isearch" "I")
    (counsel-rg "ripgrep" "rg"))

  (ymacs-ivy//define-switch file-jump
    (counsel-fzf "fzf" "f")
    (counsel-git "git" "g")
    (counsel-projectile "projectile" "proj"))

  (dolist (caller
           '(ivy-switch-buffer
             internal-complete-buffer
             ivy-switch-buffer-other-window
             counsel-projectile-switch-to-buffer
             counsel-projectile
             ymacs-counsel/kill-buffer))
    (ivy-configure
        caller
      :display-transformer-fn #'ymacs-ivy//switch-buffer-transformer))

  (ivy-configure 'package-install :display-transformer-fn
                 #'ymacs-ivy//package-install-transformer)
  (ivy-configure 'counsel-find-file :display-transformer-fn
                 #'ymacs-ivy//find-file-transformer)
  (ivy-configure 'counsel-bookmark :display-transformer-fn
                 #'ymacs-ivy//bookmark-transformer)

  (setq ivy-read-action-function #'ivy-hydra-read-action)
  (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-arrow)
  (setq ivy-action-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories '("./"))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-dispatching-done-columns 3)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-on-del-error-function #'ignore))

(after! swiper
  (setq swiper-stay-on-quit t))

(after! counsel
  (define-key! :map counsel-ag-map
    ("M-." . ymacs-ivy/select-rg-type-aliases)
    ("C-." . ymacs-ivy/select-rg-type-aliases))

  (ivy-configure 'counsel-fzf
    :unwind-fn
    (lambda ()
      (counsel-delete-process)
      (lv-delete-window)))

  (setq counsel-linux-app-format-function
        #'counsel-linux-app-format-function-name-first)
  (setq counsel-yank-pop-separator
        "\n------------------------------------------------------------\n")
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)")))

(after! counsel-projectile
  (define-key! :map projectile-command-map
    ("s a" . counsel-projectile-ag)
    ("p" . counsel-projectile)
    ("K" . projectile-kill-buffers)
    ("w" . projectile-switch-project))

  (setq counsel-fzf-cmd (concat (expand-var! "fzf") " -f \"%s\""))
  (setq counsel-rg-base-command
        "rg -M 1000 -S --no-heading --line-number --color never %s .")

  (if ymacs-ripgrep-path
      (progn
        (define-key projectile-command-map "ss" 'counsel-projectile-rg)
        (global-set-key (kbd "C-c i a") 'ymacs-counsel/rg))
    (define-key projectile-command-map "ss" 'counsel-projectile-grep)
    (global-set-key (kbd "C-c i a") 'counsel-grep)))
