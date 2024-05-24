;;; -*- lexical-binding: t; -*-

(after! savehist
  (define-advice savehist-autosave (:after () save-additional-content)
    (recentf-save-list)
    (save-place-kill-emacs-hook))

  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          global-mark-ring
          extended-command-history)))

(after! recentf
  (setq recentf-filename-handlers nil)
  (setq recentf-max-saved-items 2048)
  (setq recentf-exclude
        (list (eval-when-compile
                (rx (or (and "." (or "gz" "gif" "svg" "png" "jpg" "jpeg" "xpm" "tags") string-end)
                        (and (or "tags" "TAGS" "GTAGS") string-end)
                        (and "COMMIT_EDITMSG" string-end)
                        (and string-start (or "/tmp/" "/var/"))
                        ".cache/"))))))
