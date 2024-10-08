;; -*- lexical-binding:t -*-

(load (expand-file-name "early-init.el" user-emacs-directory))
(load (expand-file-name "init.el" user-emacs-directory))

;; store load-path
(setq ymacs-dump-load-path load-path)

(defvar ymacs-ignored-packages '(vterm auctex tsc))
(defvar ymacs-preload-packages
  `(so-long
    edmacro
    calc-menu
    face-remap
    autorevert
    bookmark
    midnight
    elec-pair
    hippie-exp
    gdbmi
    view
    ffap

    monokai-theme

    recentf
    saveplace
    savehist

    winner

    ;; ui
    tab-line
    subword
    paren
    whitespace
    hl-line
    display-fill-column-indicator
    display-line-numbers
    hideshow

    checkdoc

    org
    org-num
    org-element
    ol-bbdb
    ol-bibtex
    ol-docview
    ol-eww
    ol-gnus
    ol-info
    ol-irc
    ol-mhe
    ol-rmail
    ol-w3m

    diff-hl-margin
    vc-bzr
    vc-cvs
    vc-git
    vc-rcs
    vc-sccs
    vc-src
    vc-svn

    latex
    sh-script
    cperl-mode

    tramp
    term

    ,@ymacs-required-packages))

(dolist (package ymacs-preload-packages)
  (when (not (memq package ymacs-ignored-packages))
    (require package nil t)))

(message "Preload packages:\n")
(message "  %s"
         (string-join
          (sort (mapcar #'symbol-name features) #'string<)
          "\n  "))
(message "PreLoaded %d packages" (length features))

(when (ignore-errors (native-comp-available-p))
  (let ((time (current-time))
        (total-count (length comp-files-queue))
        new-count)
    (while comp-files-queue
      (sleep-for 2)
      (let ((rest-count (length comp-files-queue)))
        (message "estimated %.2fs (%d remaining)"
                 (* rest-count (/ (float-time (time-since time))
                                  (max (- total-count rest-count) 1)))
                 rest-count)))))

;; dump image
(when noninteractive
  (dump-emacs-portable (expand-cache! "emacs.pdmp")))
