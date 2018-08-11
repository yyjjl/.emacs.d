;; -*- lexical-binding:t -*-

(require-packages!
 lsp-mode
 company-lsp
 cquery)

(require 'init-cpp-cmake)

(defface cpp-variable-face
  `((t :foreground ,(face-attribute 'default :foreground)))
  "Face for variables"
  :group 'cquery-sem)

(defconst cpp-cquery--default-template
  "%clang \n%c -std=gnu11\n%cpp -std=c++14\n\n")




(defsubst cpp-cquery//dot-cquery-path (&optional $dir)
  (expand-file-name ".cquery"
                    (locate-dominating-file (or $dir default-directory)
                                            ".cquery")))

(defun cpp-cquery//setup ()
  (when cpp-has-cquery-p
    (unless lsp-mode
      (condition-case err
          (lsp-cquery-enable)
        (error (message "%s" (error-message-string err))))
      (setq-local company-transformers nil)
      (setq-local company-idle-delay nil)
      (setq-local company-lsp-cache-candidates nil)
      (add-to-list 'company-backends 'company-lsp)
      (add-to-list 'company-backends 'company-files))))

(defun cpp-cquery/create-dot-cquery ($dir)
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Directory: ")
                       default-directory)))
  (when $dir
    (let ((cquery-file (expand-file-name ".cquery" $dir)))
      (if (file-exists-p cquery-file)
          (message ".cquery already exists")
        (with-temp-buffer
          (erase-buffer)
          (insert cpp-cquery--default-template)
          (write-file cquery-file))
        (message ".cquery created")))))


(defmacro cpp-cquery//define-find (symbol command)
  `(defun ,(intern (format "cpp/xref-find-%s" symbol)) ()
     (interactive)
     (cquery-xref-find-custom ,command)))

(cpp-cquery//define-find base "$cquery/base")
(cpp-cquery//define-find callers "$cquery/callers")
(cpp-cquery//define-find derived "$cquery/derived")
(cpp-cquery//define-find vars "$cquery/vars")

(defun cpp-cquery//filter-arguments ($args &optional $no-filename)
  (--filter
   (not (or (string-prefix-p "-working-directory" it)
            (string-prefix-p "-resource-dir" it)
            (string-prefix-p "-fparse-all-comments" it)
            (and $no-filename
                 (not (string-prefix-p "-" it)))))
   $args))

(defun cpp-cquery//include-directories ()
  (let ((args (or (ignore-errors (gethash "args" (cquery-file-info)))
                  '("cpp" "-v" "-E"))))
    (with-temp-buffer
      (erase-buffer)
      (apply #'call-process (car args) nil t nil
             "-v" "-E" "-"
             (cpp-cquery//filter-arguments (cdr args) :no-filename))
      (goto-char (point-min))
      (let ((bounds
             (cons
              (when (re-search-forward "^#include \"\\.\\.\\.\"" nil :noerror)
                (forward-line 1)
                (point))
              (when (re-search-forward "^End of search list\\." nil :noerror)
                (forward-line 0)
                (point)))))
        (when (and (car bounds) (cdr bounds))
          (--map (expand-file-name (s-trim it))
                 (--filter
                  (string-prefix-p " " it)
                  (split-string (buffer-substring-no-properties (car bounds)
                                                                (cdr bounds))
                                "\n"
                                :omit-nulls))))))))

(defun cpp-cquery//buffer-compile-command (&optional $preprocess-only-p)
  (let* ((args (or (ignore-errors (gethash "args" (cquery-file-info)))
                   '("/usr/bin/c++")))
         (options (cpp-cquery//filter-arguments (cdr args) $preprocess-only-p)))
    (when $preprocess-only-p
      (dolist (option '("-E" "-xc++" "-C"))
        (unless (member option options)
          (push option options))))
    (concat (car args) " "
            (string-join options " ")
            (when $preprocess-only-p " -"))))




(defvar cpp-cquery-jump-map
  (define-key! :map (make-sparse-keymap)
    ("d" . cpp/xref-find-derived)
    ("v" . cpp/xref-find-vars)
    ("b" . cpp/xref-find-base)
    ("c" . cpp/xref-find-callers)))

(with-eval-after-load 'cquery
  ;; Need to register snippet capability
  (require 'company-lsp)
  (require 'cquery-semantic-highlighting)

  (aset cquery-sem-macro-faces 0 'font-lock-builtin-face)
  (aset cquery-sem-variable-faces 0 'cpp-variable-face)

  (push 'c++-mode (get 'lsp 'flycheck-modes))
  (push 'c-mode (get 'lsp 'flycheck-modes))
  (setq cquery-executable cpp-cquery-path)
  (setq cquery-extra-init-params
        '(:index (:comments 2)
          :diagnostics (:frequencyMs 2000)
          :cacheFormat "msgpack"
          :completion (:detailedLabel t)))

  ;; (defvar cpp-cquery--semantic-highlight-timer nil)
  ;; (defvar cpp-cquery--semantic-highlight-interval 1)
  ;; (defvar cpp-cquery--semantic-highlight-params nil)
  ;; (defun cpp-cquery*semantic-highlight ($fn _ $params)
  ;;   (setq cpp-cquery--semantic-highlight-params $params)
  ;;   (unless cpp-cquery--semantic-highlight-timer
  ;;     (setq cpp-cquery--semantic-highlight-timer
  ;;           (run-with-idle-timer
  ;;            cpp-cquery--semantic-highlight-interval nil
  ;;            (lambda ()
  ;;              (setq cpp-cquery--semantic-highlight-timer nil)
  ;;              (funcall $fn nil cpp-cquery--semantic-highlight-params))))))

  ;; (advice-add 'cquery--publish-semantic-highlighting
  ;;             :around #'cpp-cquery*semantic-highlight)

  (setq cquery-sem-highlight-method 'font-lock))

(with-eval-after-load 'cquery-tree
  (add-hook 'cquery-tree-mode-hook
            (lambda ()
              (toggle-truncate-lines 1)))
  (define-key! :map cquery-tree-mode-map
    ("o" . cquery-tree-press)
    ("." . cquery-tree-expand-or-set-root)
    ("^" . cquery-tree-collapse-or-select-parent)
    ("j" . next-line)
    ("k" . previous-line)))


(provide 'init-cpp-cquery)
