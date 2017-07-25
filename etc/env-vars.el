;; Add current directory to `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'env-defs)

(defun find-library-in-directory (name dir)
  (setq dir (expand-file-name dir))
  (let ((files (directory-files dir))
        file
        lib-path)
    (while (and (not lib-path) files)
      (setq file (pop files))
      (unless (member file '("." ".."))
        (setq lib-path
              (if (file-directory-p file)
                  (ignore-errors (find-library-in-directory name file))
                (and (string= file name)
                     (expand-file-name file dir))))))))

(defun utf8-locale-p (v)
  "Return whether locale string V relates to a utf-8 locale."
  (and v (string-match "utf-8" v)))

(defun python-has-module (modules &optional script)
  "Check python whether has MODULES"
  (shell-command-to-string
   (format "python3 %s %s"
           (or script (env|expand-etc "has-module.py"))
           (string-join (mapcar (lambda (x)
                                  (if (symbolp x) (symbol-name x) x)) modules)
                        " "))))

(env|open)

(env|layer (emacs "Core variables")
           (var has-xsel-p (executable-find "xsel"))
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
           (var rtags-path (env|expand-var "rtags/bin/"))
           (var irony-path (env|expand-var "irony/bin"))
           (var has-rtags-p (file-exists-p
                             (expand-file-name "rdm" (ref rtags-path))))
           (var has-irony-p (file-exists-p
                             (expand-file-name "irony-server" (ref irony-path)))))

(env|layer (haskell "Variable for haskell ")
           (var hare-path (find-library-in-directory "hare.el" "~/.cabal/share"))
           (var has-stylish-haskell-p (executable-find "stylish-haskell"))
           (var has-hastags-p (executable-find "hastags"))
           (var has-ghc-mod-p (executable-find "ghc-mod"))
           (var has-shm-p  (executable-find "structured-haskell-mode"))
           (var has-hindent-p (executable-find "hindent"))
           (var has-cabal-p (executable-find "cabal"))
           (var has-idris-p (executable-find "idris")))

(env|layer (python "Variable for python")
           (var has-pytest-p (executable-find "pytest")))

(env|layer (lisp "Variable for lisp")
           (var has-racket-p (executable-find "racket")))

(env|layer (tags "Tags")
           (var has-ggtags-p (executable-find "global")))

(env|close "../var/init-env-vars.el")
