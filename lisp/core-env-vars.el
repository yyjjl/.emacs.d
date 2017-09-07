(defmacro defvar! (name value &optional doc)
  (declare (doc-string 3))
  `(defvar ,name (eval-when-compile ,value) ,doc))

(defun find-library-in-directory ($name $dir)
  (setq $dir (expand-file-name $dir))
  (let ((files (directory-files $dir))
        file
        lib-path)
    (while (and (not lib-path) files)
      (setq file (pop files))
      (unless (member file '("." ".."))
        (setq lib-path
              (if (file-directory-p file)
                  (ignore-errors (find-library-in-directory $name file))
                (and (string= file $name)
                     (expand-file-name file $dir))))))))

(defun python-has-module ($modules &optional $script)
  "Check python whether has MODULES"
  (shell-command-to-string
   (format "python3 %s %s"
           (or $script (expand-etc! "has-module.py"))
           (string-join (mapcar (lambda (x)
                                  (if (symbolp x) (symbol-name x) x))
                                $modules)
                        " "))))

(defvar! emacs-has-xsel-p (executable-find "xsel")
  "Use `xsel' to copy/paste in terminal thoungh system clipboard")
(defvar! emacs-use-fcitx-p (executable-find "fcitx")
  "Whether to use X input method `fcitx'")
(defvar! emacs-has-mpv-p (executable-find "mplayer")
  "In order to use `emms' package, you need a music player")

(defvar! git-has-git-p (executable-find "git")
  "Whether has `git'")

;; Only one of `aspell' and `hunspell' is needed
(defvar! spelling-has-aspell-p (executable-find "aspell")
  "Use for spellcheck")
(defvar! spelling-has-hunspell-p (executable-find "hunspell")
  "Use for spellcheck")

(defvar! tags-has-ggtags-p (executable-find "global")
  "Tag multiple languages")

(defvar! latex-use-latex-p t
  "Whether to use latex packages")

(provide 'core-env-vars)
