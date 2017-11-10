(require-packages!
 restclient
 company-restclient
 markdown-mode
 csv-mode
 php-mode
 gnuplot-mode
 csharp-mode
 graphviz-dot-mode
 ;; Move buffers between windows
 buffer-move
 zeal-at-point
 skeletor
 slime)



;; prolog system
(setq prolog-system 'swi)
(add-hook 'prolog-mode-hook 'core/space-punctuation-mode)

;; pulse
(setq pulse-delay 0.01)
;; csv
(setq csv-separators '(", " ";" "|" " "))

(defalias 'perl-mode 'cperl-mode)

(setq inferior-lisp-program "ccl")

(with-eval-after-load 'slime
  (setq slime-contribs '(slime-fancy))
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-complete-symbol*-fancy t))

(with-eval-after-load 'graphviz-dot-mode
  (defun extra/dot-complete ()
    (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
           (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
           (graphviz-dot-str (buffer-substring b e))
           (allcomp (all-completions graphviz-dot-str
                                     (graphviz-dot-get-keywords))))
      (list b e allcomp)))

  (setq graphviz-dot-auto-indent-on-semi nil)
  (setq graphviz-dot-indent-width 4)

  (define-hook! extra|setup-dot (graphviz-dot-mode-hook)
    (hs-minor-mode 1)
    (add-to-list 'completion-at-point-functions 'extra/dot-complete)))

(with-eval-after-load 'grep
  (setq grep-highlight-matches t
        grep-scroll-output t)
  (dolist (v core-ignored-directories)
    (add-to-list 'grep-find-ignored-directories v)))

;; Zeal at point
(global-set-key (kbd "C-h z") 'zeal-at-point)
(global-set-key (kbd "C-h Z") 'zeal-at-point-search)
(with-eval-after-load 'zeal-at-point
  (setf (cdr (assoc 'c++-mode zeal-at-point-mode-alist)) "cpp"
        (cdr (assoc 'python-mode zeal-at-point-mode-alist)) "python")
  (add-to-list 'zeal-at-point-mode-alist '(cmake-mode . "cmake")))

;; Star dictionary lookup
(autoload 'sdcv-current-word "sdcv" nil t)
(autoload 'sdcv-goto-sdcv "sdcv" nil t)

;; `restclient-mode'
(with-eval-after-load 'restclient
  (define-key! :map restclient-mode-map
    ("M-p" . restclient-jump-prev)
    ("M-n" . restclient-jump-next)))
(define-hook! extra|restclient-setup (restclient-mode-hook)
  (add-to-list 'company-backends 'company-restclient))

(defun extra/clipboard-copy ($beg $end)
  (interactive "r")
  (if (display-graphic-p)
      (kill-new (buffer-substring-no-properties $beg $end))
    (if emacs-has-xsel-p
        (if (= 0 (shell-command-on-region $beg $end "xsel -ib"))
            (message "Copy finished")
          (message "Error occured !!!"))
      (message "Executable `xsel' not found !!!"))))

(defun extra/clipboard-paste ()
  (interactive)
  (if (display-graphic-p)
      (yank 1)
    (if emacs-has-xsel-p
        (shell-command "xsel -ob" t)
      (message "Executable `xsel' not found !!!"))))

;; `whitespace-space' setup
(with-eval-after-load 'whitespace
  (setq whitespace-style '(face spaces tabs newline
                                space-mark tab-mark newline-mark))
  (setcdr (assoc 'newline-mark whitespace-display-mappings)
          '(10 [182 10]))
  (setcdr (assoc 'tab-mark whitespace-display-mappings)
          '(9 [8594 9])))

(with-eval-after-load 'skeletor
  (setq skeletor-completing-read-function 'ivy-completing-read)

  (skeletor-define-template "cpp-cmake"
    :title "C++ Project (CMake)"
    :default-license "^gpl"
    :after-creation (lambda (dir)
                      (skeletor-async-shell-command "mkdir build"))))
(global-set-key (kbd "C-c p n") 'skeletor-create-project)
(global-set-key (kbd "C-c p N") 'skeletor-create-project-at)

;; `calc' setup
(with-eval-after-load 'calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

(define-key!
  ;; buffer-mode
  ("C-x w k" . buf-move-up)
  ("C-x w j" . buf-move-down)
  ("C-x w h" . buf-move-left)
  ("C-x w l" . buf-move-right))

(defvar extra-file-groups nil)

(defun extra%display-group ($group)
  (when (cdr $group)
    (message "GROUP %s:\n%s" (car $group) (string-join (cdr $group) "\n"))))

(defun extra%clean-groups ()
  (setq extra-file-groups
        (loop for (name . fns) in extra-file-groups
              for new-fns = (--filter (file-exists-p it) fns)
              when new-fns
              collect (cons name new-fns))))

(defun extra/add-file-to-group ($group $filename)
  "Add FILENAME to GROUP"
  (interactive
   (let* ((group-name
           (completing-read "Group: " (--map (car it) extra-file-groups)))
          (group (assoc group-name extra-file-groups)))
     (list
      (or group group-name)
      (completing-read "File: "
                       (--map (abbreviate-file-name
                               (buffer-file-name it))
                              (--filter (let ((fn (buffer-file-name it)))
                                          (and fn
                                               (or (not group)
                                                   (not (member (abbreviate-file-name fn)
                                                                group)))))
                                        (buffer-list)))
                       nil :require-match))))
  (if (stringp $group)
      (progn
        (setq $group (list $group $filename))
        (push $group extra-file-groups))
    (setcdr $group (cons $filename (cdr $group))))
  (extra%clean-groups)
  (extra%display-group $group))

(defun extra/delete-file-from-group ($group $filename)
  "Delete a FILENAME from GROUP"
  (interactive
   (let* ((group-name (completing-read "Group: "
                                       (--map (car it) extra-file-groups)
                                       nil
                                       :require-match))
          (group (assoc group-name extra-file-groups)))
     (list group
           (completing-read "File: "
                            (cdr-safe group)
                            nil
                            :require-match))))
  (when (and $group
             $filename
             (or current-prefix-arg
                 (yes-or-no-p (format "Delete file `%s' from GROUP %s"
                                      $filename (car $group)))))
    (setcdr $group (delete $filename (cdr $group))))
  (extra%clean-groups)
  (extra%display-group $group))

(defun extra/load-group ($group)
  "Open all files in GROUP. If `current-prefix-arg' is non-nil,
just display filenames in GROUP"
  (interactive
   (progn
     (extra%clean-groups)
     (list
      (assoc (completing-read "Group: "
                              (--map (car it) extra-file-groups)
                              nil
                              :require-match)
             extra-file-groups))))
  (when $group
    (unless current-prefix-arg
      (dolist (filename (cdr $group))
        (find-file filename)))
    (message "%s:\n%s"
             (if current-prefix-arg "Files" "Opened FIles")
             (string-join (cdr $group) "\n"))))

(defun extra/delete-group ($group)
  (interactive
   (list
    (assoc (completing-read "Group: "
                            (--map (car it) extra-file-groups)
                            nil
                            :require-match)
           extra-file-groups)))
  (when (and $group
             (or current-prefix-arg
                 (yes-or-no-p (format "Delete GROUP %s:\n" (car $group)))))
    (setcdr $group nil))
  (extra%clean-groups))

(add-to-list 'savehist-additional-variables 'extra-file-groups)

(define-key!
  ("C-x , a" . extra/add-file-to-group)
  ("C-x , d" . extra/delete-file-from-group)
  ("C-x , x" . extra/delete-group)
  ("C-x , ," . extra/load-group))

(provide 'init-extra)
