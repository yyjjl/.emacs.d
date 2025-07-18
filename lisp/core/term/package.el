;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash))

(option! term-type
    (eval-when-compile
      (if (and (bound-and-true-p module-file-suffix) ; dynamic module
               (executable-find "cmake")
               (executable-find "libtool")
               (executable-find "make"))
          'vterm
        'term))
  "Terminal type"
  :group 'ymacs
  :type '(choice
          (const :tag "Vterm" vterm)
          (const :tag "Shell" shell)))

(option! term-path-alist nil
  "Terminal path"
  :group 'ymacs
  :type '(alist :key-type string :value-type string))

(require-packages!
 (vterm :when (eq ymacs-term-type 'vterm))
 bash-completion)

(defvar ymacs-term-repl-alist nil)

;; kill the buffer when terminal is exited
(defvar ymacs-term-directory-functions '(ymacs-editor//project-root))

(defvar ymacs-term-program-arguments ())

(defvar-local ymacs-term-extra-name nil)
(defvar-local ymacs-term-exit-action 'kill
  "shell => when process exits, switch to other shell
keep   => when process exits, make buffer readonly
kill     => when process exits, kill buffer")
(put 'ymacs-term-exit-action 'permanent-local t)

(defvar ymacs-term-process-exit-hook '(ymacs-term//shell-exit))
(defvar ymacs-term-shell-exec-hook nil)

(defvar-local ymacs-term--ssh-info nil)
(defvar-local ymacs-term--last-buffer nil)
(defvar-local ymacs-term-keep-buffer-alive nil)

(defvar ymacs-term-buffer-name "shell"
  "The buffer name of term buffer.")

(define-key!
  ([f8] . ymacs-term/pop-shell))

(define-key! :map prog-mode-map
  ("C-c C-l" . ymacs-term/load-file-in-repl))
