;; -*- lexical-binding: t -*-

(after! ivy
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
    (projectile-find-file "projectile" "proj"))

  (dolist (caller '(ivy-switch-buffer
                    internal-complete-buffer
                    ivy-switch-buffer-other-window
                    ymacs-counsel/kill-buffer))
    (ivy-configure
        caller
      :display-transformer-fn #'ymacs-ivy//switch-buffer-transformer))

  (ivy-configure 'package-install :display-transformer-fn
                 #'ymacs-ivy//package-install-transformer)
  (ivy-configure 'counsel-bookmark :display-transformer-fn
                 #'ymacs-ivy//bookmark-transformer)

  (setq ivy-read-action-function #'ivy-hydra-read-action)
  (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-arrow)
  (setq ivy-height 13)
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

  (setq counsel-yank-pop-separator
        "\n------------------------------------------------------------\n")
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))

  (setq counsel-compile-make-args "-k -j4")
  (setq counsel-rg-base-command
        "rg -M 1000 -S --no-heading --line-number --color never %s .")

  (global-set-key (kbd "C-c i a")
                  (if ymacs-ripgrep-path #'ymacs-counsel/rg #'counsel-grep)))
