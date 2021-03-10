;; -*- lexical-binding: t -*-

(load (expand! "config-builtin") nil t)

(after! fcitx
  ;; Init fcitx prefix keys
  (setq fcitx-use-dbus nil)
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s" "M-o" "C-x" "C-c" "C-z"))

(after! ivy
  (define-key! :map ivy-minibuffer-map
    ("C-r" . ivy-reverse-i-search)
    ("C-j" . ivy-immediate-done)
    ("C-M-j" . ivy-done)
    ("M-." . ymacs-editor/ivy-meta-dot)
    ("C-." . ymacs-editor/ivy-meta-dot))

  (ymacs-editor//define-switch
    (swiper :doc "Swiper" :key "s")
    (swiper-isearch :doc "SwiperI" :key "x")
    (swiper-all :doc "SwiperA" :key "a")
    (ymacs-editor//rg :doc "ripgrep" :key "r" :save-text t :caller counsel-rg)
    (counsel-git-grep :doc "gitgrep" :key "g"))

  (ymacs-editor//define-switch
    (ymacs-editor//fzf :doc "fzf" :key "z" :caller counsel-fzf)
    (counsel-git :doc "git" :key "g")
    (counsel-find-file :doc "find file" :key "f"))

  (dolist (caller '(ivy-switch-buffer
                    internal-complete-buffer
                    ivy-switch-buffer-other-window))
    (ivy-configure caller
      :display-transformer-fn
      #'ymacs-editor//ivy-switch-buffer-transformer))

  (ivy-configure 'package-install
    :display-transformer-fn
    #'ymacs-editor//ivy-package-install-transformer)
  (ivy-configure 'counsel-bookmark
    :display-transformer-fn
    #'ymacs-editor//ivy-bookmark-transformer)

  (add-to-list 'ivy-hooks-alist '(t . ymacs-editor//display-help))

  (setq ivy-read-action-function #'ivy-hydra-read-action)
  (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-arrow)
  (setq ivy-display-functions-alist '((t . nil)))
  (setq ivy-height 13)
  (setq ivy-action-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories '("./"))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-dispatching-done-columns 3)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-re-builders-alist '((t . ymacs-editor//ivy-re-builder)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-on-del-error-function #'ignore))

(after! swiper
  (setq swiper-stay-on-quit t))

(after! counsel
  (add-to-list 'counsel-compile-local-builds #'ymacs-editor//default-compile-command t)

  (setq counsel-yank-pop-separator "\n------------------------------------------------------------\n")
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))

  (setq counsel-compile-make-args "-k -j4"))

(make-variable-buffer-local 'company-backends)
(after! company
  (define-key! :map company-active-map
    ("C-d")
    ([tab] . ymacs-editor/complete-common)
    ("TAB" . ymacs-editor/complete-common)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-n" . company-next-page)
    ("M-p" . company-previous-page))

  (setq-default company-backends
                `((company-capf
                   company-dabbrev-code company-keywords
                   ;; company-files
                   :with company-yasnippet
                   :separate)
                  ;; (company-gtags company-etags)
                  company-dabbrev))

  ;; Company should be case sensitive
  ;; (setq company-dabbrev-downcase nil)
  ;; (setq company-dabbrev-ignore-case t)
  ;; (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-char-regexp "[0-9a-zA-Z-_]")
  (setq company-dabbrev-code-other-buffers t)
  (setq company-show-numbers t)
  ;; Don't auto auto complete
  (setq company-idle-delay 0.3)
  (setq company-require-match nil)
  ;; (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-auto-commit nil)
  ;; Not to load company-mode for certain major modes.
  (setq company-global-modes
        '(not eshell-mode
              ;; comint-mode
              ;; gud-mode
              erc-mode
              rcirc-mode shell-mode
              minibuffer-inactive-mode))

  (setf (alist-get 'company-search-mode mode-line-misc-info)
        '(("" company-search-lighter " "))))

(when ymacs-editor-use-childframe-p
  (after! company-posframe
    (setq company-posframe-show-indicator nil)
    (setq company-posframe-show-metadata nil)
    (setq company-posframe-quickhelp-delay nil)

    (define-key! :map company-posframe-active-map
      ("C-c C-d" . company-posframe-quickhelp-toggle)
      ("C-h" . company-posframe-quickhelp-toggle)
      ("C-v" . company-posframe-quickhelp-scroll-down)
      ("M-v" . company-posframe-quickhelp-scroll-up))))

(after! iedit
  (setq iedit-auto-narrow t))

(after! yasnippet
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq-default yas-indent-line 'fixed)
  (setq yas-triggers-in-field nil))

(after! avy
  (setq avy-keys
        '(
          ?a ?s ?d ?f ?g ?h ?j ?k ?l
          ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
          ?z ?x ?c ?v ?b ?n ?m))

  (setq avy-background t)
  (setq avy-all-windows nil)
  (setq avy-all-windows-alt t)
  (setq avy-style 'at-full))
