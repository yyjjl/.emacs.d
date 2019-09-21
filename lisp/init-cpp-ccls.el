;; -*- lexical-binding:t -*-

(require-packages!
 lsp-mode
 company-lsp
 ccls)

(require 'init-cpp-cmake)

(defface cpp-variable-face
  `((t :foreground ,(face-attribute 'default :foreground)))
  "Face for variables"
  :group 'ccls-sem)

(defconst cpp-ccls--default-template
  "clang \n%c -std=gnu11\n%cpp -std=c++14\n\n")




(defun cpp-ccls/install-or-upgrade-ccls (&optional -upgrade)
  (interactive "P")
  (let ((build-command "mkdir -p build && cd build && cmake --build .. --config Release && make")
        (download-command "git clone --depth=1 --recursive https://github.com/MaskRay/ccls"))
    (if (file-exists-p cpp-ccls-base-path)
        (if -upgrade
            (let ((default-directory cpp-ccls-base-path))
              (compilation-start
               (concat "git pull && git submodule update && " build-command)))
          (message "ccls directory exists"))
      (let ((default-directory emacs-var-direcotry))
        (make-directory cpp-ccls-base-path t)
        (compilation-start
         (concat download-command " && " build-command))))))

(defmacro cpp-ccls//define-find (symbol command &optional extra)
  `(defun ,(intern (format "cpp/xref-find-%s" symbol)) ()
     (interactive)
     (lsp-find-custom ,command ,extra)))

(cpp-ccls//define-find base "$ccls/base")
;; (cpp-ccls//define-find bases "$ccls/inheritance" '(:level 3))
;; (cpp-ccls//define-find derived "$ccls/inheritance" '(:level 3 :derived t))
(cpp-ccls//define-find callers "$ccls/callers")
(cpp-ccls//define-find callee "$ccls/call" '(:callee t))
;; (cpp-ccls//define-find vars "$ccls/vars")
(cpp-ccls//define-find members "$ccls/member")
(cpp-ccls//define-find references-write "textDocument/references"
                       '(:context (:role 16)))
(cpp-ccls//define-find references-read "textDocument/references"
                       '(:context (:role 8)))
(cpp-ccls//define-find references-not-call "textDocument/references"
                       '(:context (:excludeRole 32)))
(cpp-ccls//define-find references-macro "textDocument/references"
                       '(:context (:role 64)))
(cpp-ccls//define-find references-address "textDocument/references"
                       '(:context (:role 128)))

(defsubst cpp-ccls//dot-ccls-path (&optional -dir)
  (expand-file-name ".ccls"
                    (locate-dominating-file (or -dir default-directory)
                                            ".ccls")))

(defun cpp-ccls//setup ()
  (condition-case err
      (progn
        (lsp)
        (setq-local lsp-enable-links nil)
        (electric-indent-local-mode -1))
    (error (message "%s" err))))

(defun cpp-ccls/create-dot-ccls (-dir)
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Directory: ")
                       default-directory)))
  (when -dir
    (let ((ccls-file (expand-file-name ".ccls" -dir)))
      (if (file-exists-p ccls-file)
          (message ".ccls already exists")
        (with-temp-buffer
          (erase-buffer)
          (insert cpp-ccls--default-template)
          (write-file ccls-file))
        (message ".ccls created")))))

(defun cpp-ccls//filter-arguments (-args &optional -no-filename)
  (--filter
   (not (or (string-prefix-p "-working-directory" it)
            (string-prefix-p "-resource-dir" it)
            (string-prefix-p "-fparse-all-comments" it)
            (member it '("-o" "-c"))
            (and -no-filename
                 (not (string-prefix-p "-" it)))))
   -args))

(defsubst cpp-ccls/get-file-args ()
  (or (when-let (args (ignore-errors (gethash "args" (ccls-file-info))))
        (when (vectorp args)
          (setq args (cl-map 'list #'identity args)))
        args)
      '("cpp" "-v" "-E")))

(defun cpp-ccls//filter-include-lines (lines)
  (--map (expand-file-name (s-trim it))
         (--filter (string-prefix-p " " it)
                   (split-string lines "\n" :omit-nulls))))

(defun cpp-ccls//include-directories ()
  (let ((args (cpp-ccls/get-file-args)))
    (with-temp-buffer
      (erase-buffer)
      (apply #'call-process (car args) nil t nil
             "-v" "-E" "-"
             (cpp-ccls//filter-arguments (cdr args) :no-filename))
      (goto-char (point-min))
      (let (pos1 pos2 pos3)
        (when (re-search-forward "^#include \"\\.\\.\\.\"" nil :noerror)
          (forward-line 1)
          (setq pos1 (point)))
        (when (re-search-forward "^#include <\\.\\.\\.>" nil :noerror)
          (forward-line 1)
          (setq pos2 (point)))
        (when (re-search-forward "^End of search list\\." nil :noerror)
          (forward-line 0)
          (setq pos3 (point)))
        (when (and pos1 pos2 pos3)
          (cons
           (cpp-ccls//filter-include-lines (buffer-substring pos1 pos2))
           (cpp-ccls//filter-include-lines (buffer-substring pos2 pos3))))))))

(defun cpp-ccls//buffer-compile-command (&optional -preprocess-only-p)
  (let* ((args (cpp-ccls/get-file-args))
         (options (cpp-ccls//filter-arguments (cdr args) -preprocess-only-p)))
    (when -preprocess-only-p
      (dolist (option '("-E" "-xc++" "-C"))
        (unless (member option options)
          (push option options))))
    (concat (car args) " "
            (string-join options " ")
            (when -preprocess-only-p " -"))))




(defvar cpp-ccls-jump-map
  (define-key! :map (make-sparse-keymap)
    ("b" . cpp/xref-find-base)
    ;; ("B" . cpp/xref-find-bases)
    ;; ("d" . cpp/xref-find-derived)
    ("c" . cpp/xref-find-callers)
    ("e" . cpp/xref-find-callee)
    ;; ("v" . cpp/xref-find-vars)
    ("m" . cpp/xref-find-members)
    ("M" . cpp/xref-find-references-macro)
    ("w" . cpp/xref-find-references-write)
    ("n" . cpp/xref-find-references-not-call)
    ("r" . cpp/xref-find-references-read)
    ("a" . cpp/xref-find-references-address)
    ("R" . ccls-reload)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache$"))

(with-eval-after-load 'ccls
  (require 'ccls-semantic-highlight)

  (aset ccls-sem-macro-faces 0 'font-lock-builtin-face)
  (aset ccls-sem-variable-faces 0 'cpp-variable-face)

  (setq ccls-executable cpp-ccls-path)
  (setq ccls-initialization-options '(:completion (:detailedLabel t)))

  (setq ccls-sem-highlight-method 'nil))

(with-eval-after-load 'ccls-tree
  (add-hook 'ccls-tree-mode-hook
            (lambda ()
              (toggle-truncate-lines 1)))
  (define-key! :map ccls-tree-mode-map
    ("o" . ccls-tree-press)
    ("." . ccls-tree-expand-or-set-root)
    ("^" . ccls-tree-collapse-or-select-parent)
    ("j" . next-line)
    ("k" . previous-line)))


(provide 'init-cpp-ccls)
