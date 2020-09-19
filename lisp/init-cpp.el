;; -*- lexical-binding:t -*-

(defvar cpp-ccls-base-path (expand-var! "ccls"))
(defvar cpp-ccls-path (expand-file-name "build/ccls" cpp-ccls-base-path))

(defvar cpp-buffer-command-functions ())
(defvar cpp-expand-macro-function nil)

(define-variable! :pkg cpp cmake)

(require-packages!
 (ggtags :when emacs-use-gtags-p)
 clang-format
 google-c-style)

(require 'init-cpp-cmake)
(require 'init-cpp-ccls)

(defconst cpp--font-lock-keywords
  (eval-when-compile
    `((,(concat "\\<"
                (regexp-opt
                 '("alignas" "alignof" "and" "and_eq" "asm"
                   "auto" "bitand" "bitor" "break" "case"
                   "catch" "class" "compl" "concept" "const"
                   "constexpr" "const_cast" "continue" "decltype"
                   "default" "delete" "do" "double" "dynamic_cast"
                   "else" "enum" "explicit" "export" "extern"
                   "for" "friend" "goto" "if" "import" "inline" "module"
                   "mutable" "namespace" "new"
                   "noexcept" "not" "not_eq" "operator" "or" "or_eq"
                   "private" "protected" "public" "register"
                   "reinterpret_cast" "requires" "return" "short"
                   "signed" "sizeof" "static" "static_assert"
                   "static_cast" "struct" "switch" "template" "this"
                   "thread_local" "throw" "try" "typedef" "typeid"
                   "typename" "union" "using" "virtual" "void"
                   "volatile" "while" "xor" "xor_eq"))
                "\\>")
       . font-lock-keyword-face)
      (,(concat "\\<"
                (regexp-opt '("bool" "float" "double" "int" "long"
                              "char" "char16_t" "char32_t" "wchar_t"
                              "unsigned"))
                "\\>")
       . font-lock-type-face)
      (,(concat "\\<" (regexp-opt '("NULL" "nullptr" "false" "true")) "\\>")
       . font-lock-constant-face))))



;; Setup
(defun cpp//common-cc-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)"
  (google-set-c-style)
  (setq c-basic-offset 4)
  ;; make DEL take all previous whitespace with it
  ;; (c-toggle-auto-newline 1)
  (highlight-indentation-set-offset c-basic-offset)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1))

(defun cpp//font-lock-setup ()
  (when (or (eq font-lock-maximum-decoration 1)
            (and (listp font-lock-maximum-decoration)
                 (eq (cdr (assoc major-mode
                                 font-lock-maximum-decoration))
                     1)))
    (font-lock-add-keywords nil cpp--font-lock-keywords)))

(defun cpp//main-setup ()
  (lsp//try-enable cpp|setup
    :enable
    (or (and
         cpp-use-cmake-p
         (setq cpp-cmake-project-root (cpp-cmake//locate-cmakelists))
         (file-exists-p (cpp-cmake//cdb-path)))
        (file-exists-p (cpp-ccls//dot-ccls-path)))
    :init
    (progn
      (when (fboundp 'ggtags-mode)
        (ggtags-mode -1))
      (electric-indent-local-mode -1))
    :fallback
    (progn
      (when cpp-cmake-project-root
        (message "Need to run `cpp/config-project' to setup ccls server"))
      (when emacs-use-gtags-p
        (ggtags-mode 1))
      ;; (setq completion-at-point-functions nil)
      (flycheck-mode -1))))

(config! projectile
  :advice
  (:around projectile--run-project-cmd
   :define (-fn &rest -args)
   (with-temp-env! (cpp-cmake//config-env) (apply -fn -args))))

(config! cc-mode
  :prefix cpp
  :bind
  (:map c-mode-base-map
   ("*" . cpp/electric-star)
   ("C-c o" . ff-find-other-file)
   ("C-c C-j" . semantic-ia-fast-jump)
   ("C-c C-v" . semantic-ia-show-variants)
   ("M-n" . next-error)
   ("M-p" . previous-error)
   ("C-M-i" . counsel-company))
  (:map c++-mode-map
   ("<")
   (">")
   ("C-c C-d")
   ("C-c C-e" . cpp/macro-expand)
   ("C-c b" . clang-format-buffer)
   ("C-c C-b" . clang-format-buffer)
   ("C-c C-l" . cpp/load-file-in-root)
   ("C-c T" . cpp-cmake/toggle-option)
   ("C-c C" . cpp-cmake/change-config)
   ("C-c D" . cpp-cmake/config)
   ("C-c C-c" . cpp/config-project)
   ("M-s c" . ccls-call-hierarchy)
   ("M-s m" . ccls-member-hierarchy)
   ("M-s i" . ccls-inheritance-hierarchy)
   ("C-c j" :map cpp-ccls-jump-map)
   ([f9] . cpp/config-project)
   ([f10] . cpp/compile)
   ([f5] . cpp/debug-current-file))

  :hook
  (common-setup
   :define (c-mode-common-hook)
   (cpp//common-cc-setup)

   (unless (or (derived-mode-p 'java-mode)
               (derived-mode-p 'groovy-mode))
     ;; (hide-ifdef-mode 1)
     ;; Make a #define be left-aligned
     (setq c-electric-pound-behavior '(alignleft))
     (cpp//font-lock-setup)

     (when (buffer-enable-rich-feature-p)
       (cpp//main-setup))))

  :advice ;; Smart tab
  (:around c-indent-line-or-region :name core*indent-for-tab)

  :config
  (require 'ccls nil t)

  (dolist (key '("#" "}" "/" ";" "," ":" "(" ")" "{"))
    (define-key c-mode-base-map key nil)))

;; Set term default directory
(when (boundp 'term-default-directory-function-list)
  (add-to-list 'term-default-directory-function-list
               (lambda ()
                 (and cpp-cmake-project-root (cpp-cmake//config-build)))))

(when (boundp 'term-default-environment-function-list)
  (add-to-list 'term-default-environment-function-list 'cpp-cmake//config-env))

(cond ((eq font-lock-maximum-decoration t)
       (setq font-lock-maximum-decoration
             '((c++-mode . 1) (c-mode . 1) (t . t))))
      ((listp font-lock-maximum-decoration)
       (dolist (mode '(c++-mode c-mode))
         (if-let (cell (assoc mode font-lock-maximum-decoration))
             (setcdr cell 1)
           (push (cons mode 1) font-lock-maximum-decoration)))))

(provide 'init-cpp)
