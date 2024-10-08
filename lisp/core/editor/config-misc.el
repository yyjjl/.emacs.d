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

(defun ymacs-editor//find-file-extern (x)
  (interactive "FFile: ")
  (call-process-shell-command
   (format "%s %s"
           (cl-case system-type
             (darwin "open")
             (cygwin "cygstart")
             (t "xdg-open"))
           (shell-quote-argument x))
   nil 0))

(defun ymacs-editor//external-file-handler (_op &rest -args)
  (let ((file (car -args))
        (process-connection-type nil))
    (recentf-add-file file)
    (kill-buffer)
    (ymacs-editor//find-file-extern file)
    (message "Opened %s externally" file)))

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

;; Make `apropos' more useful
(after! apropos
  (setq apropos-do-all t))

(after! autorevert
  (setq auto-revert-verbose nil)
  (setq global-auto-revert-non-file-buffers t))

(after! calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

(after! isearch
  (define-key! :map isearch-mode-map
    ("C-o" . isearch-occur)
    ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
    ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
    ("M-s l" . consult-line)    ;; needed by consult-line to detect isearch
    ("M-s L" . consult-line-multi)))

(after! speedbar
  (setq speedbar-use-images nil))

(after! diff-mode
  (define-key! :map diff-mode-map
    ("ESC")
    ("a" . diff-apply-hunk)
    ("n" . diff-hunk-next)
    ("p" . diff-hunk-prev)
    ("g" . diff-refresh-hunk)
    ("o" . diff-goto-source)
    ("t" . diff-test-hunk)
    ("/" . diff-split-hunk)
    ("C-/" . diff-undo)
    ("C-_" . diff-undo)
    ("TAB" . diff-hunk-next)
    ("<tab>" . diff-hunk-next)
    ("<backtab>" . diff-hunk-prev)))

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
  (setq tramp-verbose 2))

(after! view
  (define-key! :map view-mode-map
    ("s" . consult-line)
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
  (when (boundp 'eldoc-documentation-strategy)
    (setq-default eldoc-documentation-strategy #'eldoc-documentation-enthusiast)))

(after! treesit
  (defsubst ymacs-editor//init-treesit-source (langs url-suffix-fmt &optional src)
    (let ((url-fmt (concat "https://github.com/" url-suffix-fmt)))
      (dolist (lang langs)
        (add-to-list 'treesit-language-source-alist
                     (if src
                         (list lang (format url-fmt lang)  nil src nil nil)
                       (list lang (format url-fmt lang)))))))

  (ymacs-editor//init-treesit-source '(bash c cpp css java go html javascript json python rust) "tree-sitter/tree-sitter-%s")
  (ymacs-editor//init-treesit-source '(typescript) "tree-sitter/tree-sitter-typescript" "typescript/src")
  (ymacs-editor//init-treesit-source '(tsx) "tree-sitter/tree-sitter-typescript" "tsx/src")
  (ymacs-editor//init-treesit-source '(cmake) "uyha/tree-sitter-cmake")
  (ymacs-editor//init-treesit-source '(dockerfile) "camdencheek/tree-sitter-dockerfile")

  (dolist (mode
           '((c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (css-mode        . css-ts-mode)
             (cmake-mode      . cmake-ts-mode)
             (dockerfile-mode . dockerfile-ts-mode)
             (go-mode         . go-ts-mode)
             (java-mode       . java-ts-mode)
             (js-mode         . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (python-mode     . python-ts-mode)
             (sh-mode         . bash-ts-mode)
             (typescript-mode . typescript-ts-mode)))
    (let ((original-mode (car mode))
          (ts-mode (cdr mode)))
      (unless (memq original-mode ymacs-native-treesit-modes)
        (setq mode (cons ts-mode original-mode)))

      (setq major-mode-remap-alist (assq-delete-all (cdr mode) major-mode-remap-alist))
      (add-to-list 'major-mode-remap-alist mode))))

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

(after! rect
  (define-key! :map rectangle-mark-mode-map
    ("M-i" . iedit-rectangle-mode)
    ("C-x c" . clear-rectangle))

  (define-hook! ymacs-editor//setup-rectangle-mark-mode (rectangle-mark-mode-hook)
    (if rectangle-mark-mode
        (ymacs-editor//display-help (ymacs-editor//display-keys--collect rectangle-mark-mode-map))
      (ymacs-editor//display-help--hide))))

(after! fcitx
  ;; Init fcitx prefix keys
  (setq fcitx-use-dbus nil)
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s" "M-o" "C-x" "C-c" "C-z"))

(setq iedit-toggle-key-default nil)
(after! iedit
  (define-advice iedit-mode (:before (&rest _) disable-mc)
    (when (bound-and-true-p multiple-cursors-mode)
      (multiple-cursors-mode -1)))

  (setq iedit-auto-buffering nil)
  (setq iedit-auto-narrow t))

(after! multiple-cursors
  (define-advice multiple-cursors-mode (:before (&rest _) disable-iedit)
    (when (bound-and-true-p iedit-mode)
      (iedit-mode -1))))

(after! yasnippet
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq-default yas-indent-line 'fixed)
  (setq yas-triggers-in-field nil))
