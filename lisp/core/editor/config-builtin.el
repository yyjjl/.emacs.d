;;; -*- lexical-binding: t; -*-

(after! semantic
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)

  (define-key! :map semantic-mode-map :prefix "C-c ,"
    ("." . semantic-ia-fast-jump)
    ("v" . semantic-ia-show-variants)
    (("TAB" "<tab>") . semantic-ia-complete-symbol))

  ;; It's too slow, when file is large
  ;; (require 'stickyfunc-enhance)
  (setq semantic-default-submodes
        '(;; global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          ;; global-semantic-idle-summary-mode
          ;; global-semantic-idle-local-symbol-highlight-mode
          ;; global-semantic-stickyfunc-mode
          ;; Error occurs a lot
          ;; global-semantic-decoration-mode
          ;; global-semantic-highlight-func-mode
          ;; global-semantic-mru-bookmark-mode
          ))
  (setq semantic-idle-scheduler-idle-time 1)
  (setq semanticdb-project-root-functions '(ymacs-default//project-root))

  (dolist (mode '(c++-mode c-mode java-mode))
    (semanticdb-enable-gnu-global-databases mode)))

(after! dired
  (define-key! :map dired-mode-map
    ("\\" . dired-compare-directories)
    ("]" . dired-omit-mode)
    ("[" . dired-hide-details-mode)
    ("E" . ymacs-editor/find-file-externally)
    ("M-p" . dired-prev-subdir)
    ("M-n" . dired-next-subdir)
    (";" . dired-kill-subdir))

  (require 'dired-x)

  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when ymacs-gls-path
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (if (and sys/macp (not ymacs-gls-path))
      (setq dired-listing-switches "-alh")

    (setq ls-lisp-use-insert-directory-program t)
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))

  (setq wdired-allow-to-change-permissions t))

(after! ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1048576) (format "%5.1fM" (/ (buffer-size) 1048576.0)))
     ((> (buffer-size) 1024) (format "%5.1fK" (/ (buffer-size) 1024.0)))
     (t (format "%6d" (buffer-size)))))

  (setq ibuffer-expert t
        ibuffer-use-other-window t
        ibuffer-show-empty-filter-groups nil
        ibuffer-movement-cycle nil
        ibuffer-display-summary nil)

  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (or (mode . dired-mode)
                        (mode . sr-mode)))
           ("Planner" (or (name . "^\\*Calendar\\*$")
                          (name . "^diary$")
                          (mode . muse-mode)
                          (mode . org-mode)
                          (mode . org-agenda-mode)))
           ("Text" (predicate . (derived-mode-p 'text-mode)))
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Help\\*$")
                        (name . "^\\*info\\*$")
                        (name . "^\\*Backtrace\\*$")
                        (name . "^\\*Completions\\*$")
                        (name . "^\\*Compile-Log\\*$")
                        (name . "^\\*Man .*\\*$")))
           ("Emacs-Var" (filename . ".emacs.d/var"))
           ("Emacs-Config" (filename . ".emacs.d"))
           ("Code" (predicate . (derived-mode-p 'prog-mode)))
           ("Process" (or (predicate . (get-buffer-process (current-buffer)))
                          (mode . eshell-mode)))
           ("Gnus" (or (mode . message-mode)
                       (mode . bbdb-mode)
                       (mode . mail-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)
                       (name . "^\\.bbdb$")
                       (name . "^\\.newsrc-dribble"))))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only
                " " (name 18 18 :left :elide)
                " " (size-h 9 -1 :right)
                " " (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 32 -1) " " filename)))

  (setq ibuffer-filter-group-name-face 'font-lock-doc-face))

(after! hideshow
  (define-key! :map hs-minor-mode-map
    ("C-x t h" (defun ymacs-editor/hs-hide-block ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-hide-block))))
    ("C-x t s" (defun ymacs-editor/hs-show-block ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-show-block))))
    ("C-x t H" (defun ymacs-editor/hs-hide-all ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-hide-all))))
    ("C-x t S" (defun ymacs-editor/hs-show-all ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-show-all))))
    ("C-x t l" (defun ymacs-editor/hs-hide-level ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-hide-level))))
    ("C-x t t" (defun ymacs-editor/hs-toggle-hiding ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-toggle-hiding)))))

  (define-key! :map ymacs-editor-hs-overlay-map
    ("RET" . hs-show-block))

  (setq hs-isearch-open t)
  (setq hs-allow-nesting t)
  (setq hs-set-up-overlay #'ymacs-editor//hs-setup-overlay))
