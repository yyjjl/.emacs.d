(defmacro setvar! (&rest body)
  (let (forms
        (setter (if (bound-and-true-p byte-compile-current-file)
                    'defvar 'setq)))
    (while (not (null body))
      (push `(,setter ,(pop body)
                      (eval-when-compile ,(pop body)))
            forms))
    `(progn ,@(nreverse forms))))

(defun find-library-in-directory ($name $dir)
  (setq $dir (expand-file-name $dir))
  (let ((files (directory-files $dir))
        (default-directory $dir)
        file
        lib-path)
    (while (and (not lib-path) files)
      (setq file (pop files))
      (unless (member file '("." ".."))
        (setq lib-path
              (if (file-directory-p file)
                  (ignore-errors (find-library-in-directory $name file))
                (and (string= file $name)
                     (expand-file-name file $dir))))))
    lib-path))

(defun python-has-module ($modules &optional $script)
  "Check python whether has MODULES"
  (shell-command-to-string
   (format "python3 %s %s"
           (or $script (expand-etc! "has-module.py"))
           (string-join (mapcar (lambda (x)
                                  (if (symbolp x) (symbol-name x) x))
                                $modules)
                        " "))))
(setvar!
 ;; Use `xsel' to copy/paste in terminal thoungh system clipboard
 emacs-has-xsel-p (executable-find "xsel")
 ;; Whether to use X input method `fcitx'
 emacs-use-fcitx-p (executable-find "fcitx")
 ;; In order to use `emms' package, you need a music player
 emacs-has-mpv-p (executable-find "mplayer")

 ;; Whether has `git'
 git-has-git-p (executable-find "git")

 ;; Only one of `aspell' and `hunspell' is needed Use for spellcheck
 spelling-has-aspell-p (executable-find "aspell")
 ;; Use for spellcheck
 spelling-has-hunspell-p (executable-find "hunspell")

 ;; Tag multiple languages
 tags-has-gtags-p (executable-find "global")

 ;; Whether to use latex packages
 latex-use-latex-p t)

(provide 'core-env-vars)
