;;; -*- lexical-binding: t; -*-

(after! company-posframe
  (setq company-posframe-show-indicator nil)
  (setq company-posframe-show-metadata nil))

(setq which-key-dont-use-unicode t)
(after! which-key
  (setq which-key-allow-imprecise-window-fit nil)
  (setq which-key-show-remaining-keys t)

  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c ," "semantic")
  (which-key-add-key-based-replacements "C-c @" "hide-show")
  (which-key-add-key-based-replacements "C-c f" "flycheck")
  (which-key-add-key-based-replacements "C-c i" "counsel")
  (which-key-add-key-based-replacements "C-c m" "mc")

  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x @" "modifior")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")
  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x X" "edebug")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x j" "jump")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x r" "register & rectangle")
  (which-key-add-key-based-replacements "C-x t" "tab & hide-show")
  (which-key-add-key-based-replacements "C-x w" "winner & buf-move & ivy-view")
  (which-key-add-key-based-replacements "C-x g" "git")
  (which-key-add-key-based-replacements "C-x ," "hydra & misc")

  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode "C-c ?" "checkdoc")
  (which-key-add-major-mode-key-based-replacements 'python-mode "C-c C-t" "python-skeleton")

  (which-key-add-major-mode-key-based-replacements 'org-mode "C-c C-v" "babel")
  (which-key-add-major-mode-key-based-replacements 'org-mode "C-c t" "table")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-x" "markdown-toggle"))

