;; -*- lexical-binding: t -*-

(defvar ivy-views-persistent-file "ivy-views.el")

;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
(defun core//toggle-between-swiper-rg ()
  "Toggle `counsel-rg', `swiper', `swiper-isearch' with the current input."
  (interactive)
  (ivy-quit-and-run
    (cl-case (ivy-state-caller ivy-last)
      (swiper
       (swiper-isearch ivy-text))
      (swiper-isearch
       (if emacs-use-ripgrep-p
           (counsel-rg ivy-text)
         (counsel-grep ivy-text)))
      (t
       (swiper ivy-text)))))

(defun core//swiper-prompt ()
  (ivy-add-prompt-count
   (cl-case (ivy-state-caller ivy-last)
     (swiper "Search/I/rg: ")
     (swiper-isearch "S/Isearch/rg: ")
     (t "S/I/rg: "))))

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

(config! ivy
  :prefix ivy

  :bind
  (:map ivy-minibuffer-map
   ("C-r" . ivy-reverse-i-search)
   ("C-j" . ivy-immediate-done)
   ("C-M-j" . ivy-done)
   ("M-." . ignore))

  :advice
  (:around ivy--preselect-index :name ignore-errors!)

  :hook
  (occur-mode-setup
   :define (ivy-occur-mode-hook ivy-occur-grep-mode-hook)
   (local-set-key "/" #'ivy-occur/filter-lines)
   (local-set-key (kbd "C-/") #'ivy-occur/undo)
   (toggle-truncate-lines 1))

  (:anonymous
   :define (kill-emacs-hook)
   (core//save-variable 'ivy-views ivy-views-persistent-file))

  :config
  (core//load-variable 'ivy-views ivy-views-persistent-file)

  (dolist (caller '(counsel-rg counsel-ag counsel-grep swiper-isearch swiper counsel-rg))
    (ivy-set-prompt caller #'core//swiper-prompt))

  (dolist (caller '(ivy-switch-buffer
                    counsel/kill-buffer
                    internal-complete-buffer
                    ivy-switch-buffer-other-window
                    counsel-projectile-switch-to-buffer
                    counsel-projectile))
    (ivy-configure caller :display-transformer-fn #'core//ivy-switch-buffer-transformer))

  (ivy-configure 'package-install :display-transformer-fn #'core//package-install-transformer)
  (ivy-configure 'counsel-find-file :display-transformer-fn #'core//counsel-find-file-transformer)
  (ivy-configure 'counsel-bookmark :display-transformer-fn #'core//counsel-bookmark-transformer)

  (setq ivy-read-action-function #'ivy-hydra-read-action)

  (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-arrow)

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
  (setq ivy-on-del-error-function #'ignore))

(config! swiper
  :bind
  (:map swiper-map
   ("<C-return>" . core//toggle-between-swiper-rg))

  :config
  (setq swiper-stay-on-quit t))

(config! counsel
  :bind
  (:map counsel-ag-map
   ("<C-return>" . core//toggle-between-swiper-rg))
  (:map counsel-grep-map
   ("<C-return>" . core//toggle-between-swiper-rg))

  :advice
  (:around counsel-cd
   :define (-fn)
   (let ((enable-recursive-minibuffers t))
     (funcall -fn)))

  (:before counsel--async-command
   :define (-cmd &rest _)
   (unless (stringp -cmd)
     (setq -cmd (string-join -cmd " ")))
   (lv-message "Command: (@%s) %s"
               (propertize default-directory 'face font-lock-constant-face)
               (propertize -cmd 'face font-lock-doc-face)))

  (:after counsel--grep-unwind
   :define (&optional _) (lv-delete-window))

  :config

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

(config! counsel-projectile
  :bind
  (:map projectile-command-map
   ("s a" . counsel-projectile-ag)
   ("p" . counsel-projectile)
   ("K" . projectile-kill-buffers)
   ("w" . projectile-switch-project))

  :config
  (if emacs-use-ripgrep-p
      (progn
        (setq counsel-rg-base-command
              "rg -M 1000 -S --no-heading --line-number --color never %s .")
        (define-key projectile-command-map "ss" 'counsel-projectile-rg)
        (global-set-key (kbd "C-c i a") 'counsel/rg))
    (define-key projectile-command-map "ss" 'counsel-projectile-grep)
    (global-set-key (kbd "C-c i a") 'counsel-grep)))

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
  ("j" . counsel/file-jump)
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

(define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)

(provide 'core-ivy)
