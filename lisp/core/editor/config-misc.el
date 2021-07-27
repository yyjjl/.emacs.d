;;; -*- lexical-binding: t; -*-

(defvar bookmark-alist)
(defvar bookmark-current-bookmark)
(declare-function bookmark-get-filename 'bookmark)

(defvar ymacs-editor-external-file-regexp
  (eval-when-compile
    (let ((extentions '("pdf" "djvu" "dvi"
                        "odf" "odg" "odp" "ods" "odt"
                        "docx?" "xlsx?" "pptx?"
                        "mkv" "avi" "mp4" "rmvb")))
      (rx-to-string
       `(and "."
             (or ,@extentions ,@(mapcar #'upcase extentions))
             string-end)))))

(defun ymacs-editor//external-file-handler (_op &rest -args)
  (let ((file (car -args))
        (process-connection-type nil))
    (recentf-add-file file)
    (kill-buffer)
    (if (fboundp #'counsel-find-file-extern)
        (progn
          (counsel-find-file-extern file)
          (message "Opened %s externally" file))
      (message "Don't know how to open %s" file))))

(defun ymacs-editor//bookmark-setup ()
  (unless (ignore-errors (file-remote-p default-directory))
    ;; Setup default bookmark
    (setq bookmark-current-bookmark
          (ignore-errors
            (cl-loop for (name . record) in bookmark-alist
                     when (equal (file-truename (buffer-file-name))
                                 (file-truename (bookmark-get-filename name)))
                     return name)))))

(put 'ymacs-editor//external-file-handler 'safe-magic t)
(put 'ymacs-editor//external-file-handler 'operations '(insert-file-contents))

(setq he-dabbrev-chars "0-9a-zA-Z\\?!_")
(setq-default hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-all-abbrevs
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-file-name-partially
                try-complete-file-name))

(after! autorevert
  (setq auto-revert-verbose nil)
  (setq global-auto-revert-non-file-buffers t))

(after! calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

(after! isearch
  (define-key! :map isearch-mode-map
    ("C-o" . isearch-occur)))

(after! speedbar
  (setq speedbar-use-images nil))

(after! ediff
  (add-hook 'ediff-before-setup-hook
            (lambda () (window-configuration-to-register :ediff-windows)))
  (add-hook 'ediff-quit-hook
            (lambda () (jump-to-register :ediff-windows)))

  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(after! bookmark
  (bookmark-maybe-load-default-file)

  ;; Setup for existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (ymacs-editor//bookmark-setup))))

(after! ffap
  (advice-add #'ffap-guesser :around #'ignore-remote!)

  ;; do not use ping, it's very slow
  (setq ffap-machine-p-known 'reject))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (setq vc-ignore-dir-regexp
        (format "\\(?:%s\\)\\|\\(?:%s\\)"
                locate-dominating-stop-dir-regexp
                tramp-file-name-regexp))

  (setq tramp-terminal-type "tramp")
  (setq tramp-default-method "ssh")
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (file-remote-p name 'method)))))
  (setq tramp-chunksize 8192)
  (setq tramp-verbose 1)
  ;; @see https://github.com/syl20bnr/spacemacs/issues/1921
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

(after! view
  (define-key! :map view-mode-map
    ("s" . ymacs-editor/swiper)
    ("q" . View-exit)
    ("Q" . View-quit)))

(after! xref
  (define-key! :map xref--xref-buffer-mode-map
    ("M-n" . next-error)
    ("M-p" . previous-error)
    ("j" (defun ymacs-editor/xref-next ()
           (interactive)
           (xref--search-property 'xref-item)))
    ("k" (defun ymacs-editor/xref-prev ()
           (interactive)
           (xref--search-property 'xref-item t))))

  (add-to-list 'xref-prompt-for-identifier 'xref-find-references :append))

(after! grep
  (setq grep-highlight-matches t)
  (setq grep-scroll-output t))

(after! info
  (setq info-lookup-other-window-flag nil))

(after! eldoc
  (when (fboundp 'eldoc-documentation-compose-eagerly)
    (setq-default eldoc-documentation-function #'eldoc-documentation-compose-eagerly)))

(after! so-long
  ;; reduce false positives w/ larger threshold
  (setq so-long-threshold 1000)
  ;; make sure that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; Text files could possibly be too long too
  (add-to-list 'so-long-target-modes 'text-mode)
  ;; But disable everything else that may be unnecessary/expensive for large
  ;; or wide buffers.
  (setq so-long-target-modes
        (append so-long-target-modes
                '(flyspell-mode
                  eldoc-mode
                  auto-composition-mode))))

(after! prolog
  (setq prolog-system 'swi))

(after! erc-track
  (setq erc-track-enable-keybindings nil)) ;; erc

(after! pulse
  (setq pulse-delay 0.01))

(after! inf-lisp
  (setq inferior-lisp-program "sbcl"))

(after! image-mode
  (setq image-animate-loop t))

(setq winner-dont-bind-my-keys t)
(after! winner
  (setq winner-boring-buffers-regexp "^ \\*"))

(after! re-builder
  (define-key! :map reb-mode-map
    ("M-q" . reb-quit)
    ("M-n" . reb-next-match)
    ("M-p" . reb-prev-match))
  (define-key! :map reb-lisp-mode-map
    ("M-q" . reb-quit)
    ("M-n" . reb-next-match)
    ("M-p" . reb-prev-match))

  (setq reb-re-syntax 'string)

  (define-hook! ymacs-editor//setup-reb-mode (reb-mode-hook reb-lisp-mode-hook)
    (ymacs-editor//display-help (ymacs-editor//display-keys--collect reb-mode-map)))

  (advice-add 'reb-quit :before #'ymacs-editor//display-help--hide))

(after! fcitx
  ;; Init fcitx prefix keys
  (setq fcitx-use-dbus nil)
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s" "M-o" "C-x" "C-c" "C-z"))

(after! iedit
  (define-advice iedit-mode (:before (&rest _) disable-mc)
    (when (bound-and-true-p multiple-cursors-mode)
      (multiple-cursors-mode -1)))

  (setq iedit-auto-narrow t))

(after! multiple-cursors
  (define-advice multiple-cursors-mode (:before (&rest _) disable-iedit)
    (when (bound-and-true-p iedit-mode)
      (iedit-mode -1))))

(after! yasnippet
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq-default yas-indent-line 'fixed)
  (setq yas-triggers-in-field nil))
