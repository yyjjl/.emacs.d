(setvar!
 ;; Use `xsel' to copy/paste in terminal thoungh system clipboard
 env-has-xsel-p (executable-find "xsel")
 ;; Whether to use X input method `fcitx'
 env-has-fcitx-p (executable-find "fcitx")
 ;; In order to use `emms' package, you need a music player
 env-has-mpv-p (executable-find "mplayer")
 env-has-ripgrep-p (executable-find "rg")
 ;; Whether has `git'
 env-has-git-p (executable-find "git")
 ;; Only one of `aspell' and `hunspell' is needed Use for spellcheck
 env-has-aspell-p (executable-find "aspell")
 ;; Use for spellcheck
 env-has-hunspell-p (executable-find "hunspell")
 ;; Tag multiple languages
 env-has-gtags-p (executable-find "global")
 ;; Whether to use latex packages
 env-has-latex-p (executable-find "xelatex"))

(provide 'core-env-vars)
