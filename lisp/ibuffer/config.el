;;; -*- lexical-binding: t; -*-

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
