;; Add current directory to `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'env-defs)

(var emacs|locale-is-utf8-p
     :value (or (utf8-locale-p (and (executable-find "locale")
                                    (shell-command-to-string "locale")))
                (utf8-locale-p (getenv "LC_ALL"))
                (utf8-locale-p (getenv "LC_CTYPE"))
                (utf8-locale-p (getenv "LANG"))))

;; ----------------------------------------
;;* Misc
;; ----------------------------------------
(var emacs|has-xsel-p
     :value (executable-find "xsel")
     :doc "Use `xsel' to copy/paste in terminal thoungh system clipboard")
(var emacs|use-fcitx-p
     :value (executable-find "fcitx")
     :doc "Whether to use X input method `fcitx'")
(var emacs|has-mpv-p
     :value (executable-find "mplayer")
     :doc "In order to use `emms' package, need a music player")

;; ----------------------------------------
;;* Git
;; ----------------------------------------
(var git|has-git-p
     :value (executable-find "git")
     :doc "Whether has `git'")

;; ----------------------------------------
;;* Spellcheck
;; ----------------------------------------
;; Only one of `aspell' and `hunspell' is needed
(var spelling|has-aspell-p
     :value (executable-find "aspell")
     :doc "Use for spellcheck")
(var spelling|has-hunspell-p
     :value (executable-find "hunspell")
     :doc "Use for spellcheck")

;; ----------------------------------------
;;* C/C++
;; ----------------------------------------
(var cpp|rtags-path
     :value (env|expand-var "rtags/bin/")
     :doc "Rtags directory")
(var cpp|irony-path
     :value (env|expand-var "irony/bin")
     :doc "irony directory")
(var cpp|has-rtags-p
     :value (file-exists-p
             (expand-file-name "rdm" cpp|rtags-path))
     :doc "Rtags support, indexing c++ projects")
(var cpp|has-irony-p
     :value (file-exists-p
             (expand-file-name "irony-server" cpp|irony-path))
     :doc "Irony support, context sensitive c++ completions")

;; ----------------------------------------
;;* Haskell
;; ----------------------------------------
(var haskell|hare-path
     :value (ignore-errors
              (find-library-in-directory "hare.el" "~/.cabal/share"))
     :doc "Hare is a haskell refacting tool")
(var haskell|has-stylish-haskell-p
     :value (executable-find "stylish-haskell")
     :doc "Like clang-format, format haskell file")
(var haskell|has-hastags-p
     :value (executable-find "hastags")
     :doc "Tag haskell file, jump to defination")
(var haskell|has-ghc-mod-p
     :value (executable-find "ghc-mod")
     :doc "Haskell context sensitive completions")
(var haskell|has-shm-p
     :value (executable-find "structured-haskell-mode")
     :doc "Great tool to edit haskell file")
(var haskell|has-hindent-p
     :value (executable-find "hindent")
     :doc "Indent haskell expression")
(var haskell|has-cabal-p
     :value (executable-find "cabal")
     :doc "Haskell cabal support")
(var haskell|has-idris-p
     :value (executable-find "idris")
     :doc "Idris language support")

;; ----------------------------------------
;;* Python
;; ----------------------------------------
(var python|has-pytest-p
     :value (executable-find "pytest")
     :doc "Use for python unit test ")

;; ----------------------------------------
;;* Lisp
;; ----------------------------------------
(var lisp|has-racket-p
     :value (executable-find "racket")
     :doc "Racket support")

;; ----------------------------------------
;;* Tags
;; ----------------------------------------
(var tags|has-ggtags-p
     :value (executable-find "global")
     :doc "Tag multiple languages")

;; ----------------------------------------
;;* Terminal
;; ----------------------------------------
(var term|zsh-path
     :value (executable-find "zsh")
     :doc "Zsh path")
(var term|bash-path
     :value (executable-find "bash")
     :doc "Bash path")
(var term|use-eshell-p
     :value nil
     :doc "Non-nil means to use eshell as default terminal")

;; ----------------------------------------
;;* Latex
;; ----------------------------------------
(var latex|use-latex-p
     :value t
     :doc "Whether to use latex packages")

;; ----------------------------------------
;;* Javascript
;; ----------------------------------------
(var js2|has-tern-p
     :value (executable-find "tern")
     :doc "Js context sensitive completion")
(var js2|has-web-beautify-p
     :value (executable-find "js-beautify")
     :doc "Like clang-format")

(env|write-file "../var/init-env-vars.el")
