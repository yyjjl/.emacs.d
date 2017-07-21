;; Add current directory to `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'env-defs)

(env|open)

(defun utf8-locale-p (v)
  "Return whether locale string V relates to a utf-8 locale."
  (and v (string-match "utf-8" v)))

(env|layer (emacs "Core variables")
           (var use-fcitx-p (executable-find "fcitx")
                "Whether to use X input method `fcitx'")
           (var locale-is-utf8-p
                (or (utf8-locale-p (and (executable-find "locale")
                                             (shell-command-to-string "locale")))
                    (utf8-locale-p (getenv "LC_ALL"))
                    (utf8-locale-p (getenv "LC_CTYPE"))
                    (utf8-locale-p (getenv "LANG")))))

(env|layer (git "Git support")
           (var has-git-p (executable-find "git")))

(env|layer (spelling "Variables for spelling check")
           (var has-aspell-p (executable-find "aspell"))
           (var has-hunspell-p (executable-find "hunspell")))

(env|layer (cpp "Variables for c/c++ languages support")
           )
(env|layer (python "Variable for python")
           (var has-pytest-p (executable-find "pytest")))

(env|layer (lisp "Variable for lisp")
           (var has-racket-p (executable-find "racket")))

(env|close "../var/init-env-vars.el")
