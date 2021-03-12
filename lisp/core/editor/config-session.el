;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-savehist-exclude-variables
  '(load-history
    register-alist
    vc-comment-ring
    flyspell-auto-correct-ring
    org-mark-ring
    mark-ring
    global-mark-ring
    planner-browser-file-display-rule-ring))

(after! savehist
  (define-advice savehist-autosave (:after () save-additional-content)
    (recentf-save-list)
    (save-place-kill-emacs-hook))

  (define-advice savehist-save
      (:around (-fn &optional -auto-save) set-additional-variables)
    (let ((variables savehist-additional-variables)
          (kill-ring (cl-remove-if-not
                      (lambda (x) (< (length x) 1024))
                      kill-ring)))
      (dolist (symbol (apropos-internal "-\\(ring\\|history\\)\\'" 'boundp))
        (unless (or (null (symbol-value symbol))
                    (memq symbol ymacs-editor-savehist-exclude-variables)
                    (memq symbol savehist-minibuffer-history-variables)
                    (ring-p (symbol-value symbol))
                    (keywordp symbol))
          (cl-pushnew symbol variables)))
      (let ((savehist-additional-variables variables))
        (funcall -fn -auto-save)))))

(after! recentf
  (setq recentf-max-saved-items 2048)
  (setq recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$"
          (lambda (file)
            (file-in-directory-p file package-user-dir)))))
