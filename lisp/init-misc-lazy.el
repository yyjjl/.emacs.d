;;; init-misc-lazy.el --- misc setup loaded later
(transient-mark-mode t)

(recentf-mode 1)

(global-auto-revert-mode)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(setq csv-separators '("," ";" "|" " "))


;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(add-hook 'comint-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(global-page-break-lines-mode)

(column-number-mode 1)

(setq eshell-prompt-function
      (lambda()
        (concat (getenv "USER") "@" (getenv "HOST") ":"
                (eshell/pwdx)
                (if (= (user-uid) 0) " # " " $ "))))

;; Write backup files to own directory
(if (not (file-exists-p (expand-file-name "~/.emacs.d/data/backups")))
  (make-directory (expand-file-name "~/.emacs.d/data/backups")))
(setq backup-by-coping t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/data/backups"))
      delete-old-versions t
      version-control t  ;use versioned backups
      kept-new-versions 6
      kept-old-versions 2)

;; Donot make backups of files, not safe
;; @see https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq vc-make-backup-files nil)

;; I'm in Australia now, so I set the locale to "en_AU"
(defun insert-date (prefix)
    "Insert the current date.
With prefix-argument, use ISO format.  With
two PREFIX arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%d.%m.%Y")
                   ((equal prefix '(4)) "%Y-%m-%d")
                   ((equal prefix '(16)) "%d %B %Y")))
          )
      (insert (format-time-string format))))

;;compute the length of the marked region
(defun region-length ()
  "Length of a region."
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))


;; {{ imenu tweakment
(defvar rimenu-position-pair nil "Positions before and after imenu jump.")
(add-hook 'imenu-after-jump-hook
          (lambda ()
            (let ((start-point (marker-position (car mark-ring)))
                  (end-point (point)))
              (setq rimenu-position-pair (list start-point end-point)))))

(defun rimenu-jump ()
  "Jump to the closest before/after position of latest imenu jump."
  (interactive)
  (when rimenu-position-pair
    (let ((p1 (car rimenu-position-pair))
          (p2 (cadr rimenu-position-pair)))

      ;; jump to the far way point of the rimenu-position-pair
      (if (< (abs (- (point) p1))
             (abs (- (point) p2)))
          (goto-char p2)
          (goto-char p1))
      )))
;; }}

;; show ascii table
(defun ascii-table ()
  "Print the ascii table.  Based on a defun by Alex Schroeder <asc@bsiag.com>."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
           (setq i (+ i 1))
           (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;; {{ grep and kill-ring
(defun grep-pattern-into-list (regexp)
  (let ((s (buffer-string))
        (pos 0)
        item
        items)
    (while (setq pos (string-match regexp s pos))
      (setq item (match-string-no-properties 0 s))
      (setq pos (+ pos (length item)))
      (if (not (member item items))
          (add-to-list 'items item)
        ))
    items))

(defun grep-pattern-into-kill-ring (regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string and insert them into `kill-ring'"
  (interactive
   (let* ((regexp (read-regexp "grep regex:")))
     (list regexp)))
  (let (items rlt)
    (setq items (grep-pattern-into-list regexp))
    (dolist (i items)
      (setq rlt (concat rlt (format "%s\n" i)))
      )
    (kill-new rlt)
    (message "matched strings => kill-ring")
    rlt))

(defun grep-pattern-jsonize-into-kill-ring (regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string, jsonize them, and insert into kill ring"
  (interactive
   (let* ((regexp (read-regexp "grep regex:")))
     (list regexp)))
  (let (items rlt)
    (setq items (grep-pattern-into-list regexp))
    (dolist (i items)
      (setq rlt (concat rlt (format "%s : %s ,\n" i i)))
      )
    (kill-new rlt)
    (message "matched strings => json => kill-ring")
    rlt))

(defun grep-pattern-cssize-into-kill-ring (regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string, cssize them, and insert into kill ring"
  (interactive
   (let* ((regexp (read-regexp "grep regex:")))
     (list regexp)))
  (let (items rlt)
    (setq items (grep-pattern-into-list regexp))
    (dolist (i items)
      (setq i (replace-regexp-in-string "\\(class=\\|\"\\)" "" i))
      (setq rlt (concat rlt (format ".%s {\n}\n\n" i))))
    (kill-new rlt)
    (message "matched strings => json => kill-ring")
    rlt))
;; }}


(defun display-line-number ()
  "Display current line number in mini-buffer."
  (interactive)
  (let (l)
    (setq l (line-number-at-pos))
    (message "line number:%d" l)
    ))

(with-eval-after-load 'grep
  (dolist (v '("auto"
               "target"
               "node_modules"
               "bower_components"
               ".sass_cache"
               ".cache"
               ".git"
               ".cvs"
               ".svn"
               ".hg"
               "elpa"))
    (add-to-list 'grep-find-ignored-directories v)))

;; {{ unique lines
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))
;; }}

(defun insert-file-link-from-clipboard ()
  "Make sure the full path of file exist in clipboard.
This command will convert
The full path into relative path insert it as a local file link in `org-mode'"
  (interactive)
  (let (str)
    (with-temp-buffer
      (paste-from-x-clipboard)
      (setq str (buffer-string)))

    ;; convert to relative path (relative to current buffer) if possible
    (let ((m (string-match (file-name-directory (buffer-file-name)) str) ))
        (if (and m (= 0 m ))
            (setq str (substring str (length (file-name-directory (buffer-file-name))))))
      (insert (format "[[file:%s]]" str))
      )))

(defun font-file-to-base64 (file)
  (let ((str "")
        (file-base (file-name-sans-extension file))
        (file-ext (file-name-extension file)))

    (if (file-exists-p file)
        (with-temp-buffer
          (shell-command (concat "cat " file "|base64") 1)
          (setq str (replace-regexp-in-string "\n" "" (buffer-string)))))
    str))

(defun convert-binary-to-css-code ()
  "Convert binary (image, font...) into css."
  (interactive)
  (let (str
        rlt
        (file (read-file-name "The path of image:"))
        file-ext
        file-base)

    (setq file-ext (file-name-extension file))
    (setq file-base (file-name-sans-extension file))
    (cond
     ((member file-ext '("ttf" "eot" "woff"))
      (setq rlt (concat "@font-face {\n"
                        "  font-family: familarName;\n"
                        "  src: url('data:font/eot;base64," (font-file-to-base64 (concat file-base ".eot")) "') format('embedded-opentype'),\n"
                        "       url('data:application/x-font-woff;base64," (font-file-to-base64 (concat file-base ".woff")) "') format('woff'),\n"
                        "       url('data:font/ttf;base64," (font-file-to-base64 (concat file-base ".ttf")) "') format('truetype');"
                        "\n}"
                        )))
     (t
      (with-temp-buffer
        (shell-command (concat "cat " file "|base64") 1)
        (setq str (replace-regexp-in-string "\n" "" (buffer-string))))
      (setq rlt (concat "background:url(\"data:image/"
                          file-ext
                          ";base64,"
                          str
                          "\") no-repeat 0 0;"
                          ))))
    (kill-new rlt)
    (copy-yank-str rlt)
    (message "css code => clipboard & yank ring")))


(defun current-font-face ()
  "Get the font face under cursor."
  (interactive)
  (let ((rlt (format "%S" (get-text-property (point) 'face))))
    (kill-new rlt)
    (copy-yank-str rlt)
    (message "%s => clipboard & yank ring" rlt)
    ))

(defun current-thing-at-point ()
  (interactive)
  (message "thing = %s" (thing-at-point 'symbol)))

(defun open-readme-in-git-root-directory ()
  (interactive)
  (let (filename
        (root-dir (locate-dominating-file (file-name-as-directory (file-name-directory buffer-file-name)) ".git"))
        )
    ;; (message "root-dir=%s" root-dir)
    (and root-dir (file-name-as-directory root-dir))
    (setq filename (concat root-dir "README.org"))
    (if (not (file-exists-p filename))
        (setq filename (concat root-dir "README.md"))
      )
    ;; (message "filename=%s" filename)
    (if (file-exists-p filename)
        (switch-to-buffer (find-file-noselect filename nil nil))
      (message "NO README.org or README.md found!"))
    ))

;; from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun vc-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defun vc-copy-file-and-rename-buffer ()
  "Copy the current buffer and file it is visiting.
if the old file is under version control, the new file is added into
version control automatically"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (copy-file filename new-name t)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)
        (when (vc-backend filename)
          (vc-register)
          )))))

(defun toggle-env-http-proxy ()
  "Set/unset the environment variable http_proxy which w3m uses."
  (interactive)
  (let ((proxy "http://127.0.0.1:8000"))
    (if (string= (getenv "http_proxy") proxy)
        ;; clear the the proxy
        (progn
          (setenv "http_proxy" "")
          (message "env http_proxy is empty now")
          )
      ;; set the proxy
      (setenv "http_proxy" proxy)
      (message "env http_proxy is %s now" proxy))
    ))

(defun strip-convert-lines-into-one-big-string (beg end)
  "Strip and convert selected lines into one big string which is copied into kill ring.
When transient-mark-mode is enabled, if no region is active then only the
current line is acted upon.

If the region begins or ends in the middle of a line, that entire line is
copied, even if the region is narrowed to the middle of a line.

Current position is preserved."
  (interactive "r")
  (let (str (orig-pos (point-marker)))
    (save-restriction
      (widen)
      (when (and transient-mark-mode (not (use-region-p)))
        (setq beg (line-beginning-position)
              end (line-beginning-position 2)))

      (goto-char beg)
      (setq beg (line-beginning-position))
      (goto-char end)
      (unless (= (point) (line-beginning-position))
        (setq end (line-beginning-position 2)))

      (goto-char beg)
      (setq str (replace-regexp-in-string "[ \t]*\n" "" (replace-regexp-in-string "^[ \t]+" "" (buffer-substring-no-properties beg end))))
      ;; (message "str=%s" str)
      (kill-new str)
      (goto-char orig-pos)))
  )

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down
;;----------------------------------------------------------------------------

;; someone mentioned that blink cursor could slow Emacs24.4
;; I couldn't care less about cursor, so turn it off explicitly
;; https://github.com/redguardtoo/emacs.d/issues/208
;; but somebody mentioned that blink cursor is needed in dark theme
;; so it should not be turned off by default
;; (blink-cursor-mode -1)


(defun create-scratch-buffer nil
  "Create a new scratch buffer to work in.  (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a `before-save-hook', and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

;; {{ save history
;; On Corp machines, I don't have permission to access history,
;; so safe-wrap is used
(safe-wrap
 (if (file-writable-p (file-truename "~/.emacs.d/data/history"))
     (progn
       (setq history-length 8000)
       (setq savehist-additional-variables
             '(search-ring regexp-search-ring kill-ring))
       (savehist-mode 1))
   (message "Failed to access ~/.emacs.d/data/history")))
;; }}

;; ------------------------------------------------------------

;; Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E"
  (let (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))

    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))

(defun diff-region-tag-selected-as-a ()
  "Select a region to compare"
  (interactive)
  (when (region-active-p)
    (let (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b`"))

(defun diff-region-compare-with-b ()
  "Compare current region with region selected by `diff-region-tag-selected-as-a' "
  (interactive)
  (if (region-active-p)
      (let (rlt-buf
            diff-output tmp
            (fa (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory))))
            (fb (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory)))))
        ;;  save current content as file B
        (when fb
          (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
          (write-region (car tmp) (cadr tmp) fb))

        (setq rlt-buf (get-buffer-create "*Diff-region-output*"))
        (when (and fa (file-exists-p fa) fb (file-exists-p fb))
          ;; save region A as file A
          (save-current-buffer
            (set-buffer (get-buffer-create "*Diff-regionA*"))
            (write-region (point-min) (point-max) fa))
          ;; diff NOW!
          (setq diff-output (shell-command-to-string (format "diff -Nabur %s %s" fa fb)))
          ;; show the diff output
          (if (string= diff-output "")
              ;; two regions are same
              (message "Two regions are SAME!")
            ;; show the diff
            (save-current-buffer
              (switch-to-buffer-other-window rlt-buf)
              (set-buffer rlt-buf)
              (erase-buffer)
              (insert diff-output)
              (diff-mode))))

        ;; clean the temporary files
        (if (and fa (file-exists-p fa))
            (delete-file fa))
        (if (and fb (file-exists-p fb))
            (delete-file fb)))
    (message "Please select region at first!")))

;; ------------------------------------------------------------

;; zeal-at-point {{
(defvar zeal-docsets '(angularjs
                       bash bootstrap c c++ css cmake
                       elisp express flask go haskell html jinja lisp "emacs lisp"
                       latex less markdown nodejs opengl perl
                       python sailsjs scipy typescript jquery))
(defvar zeal-search-history nil)
(defun zeal-search (&optional arg)
  (interactive)
  (let* ((thing (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (thing-at-point 'symbol)))
         (search (concat (ivy-read (concat "Docset to search "
                                           (unless (null thing) (concat "(" thing ")"))
                                           ": ")
                                   zeal-docsets
                                   :history 'zeal-search-history
                                   :initial-input
                                   (car (split-string (symbol-name major-mode) "-")))
                        ":")))
    (if (null thing)
        (setq search (read-string "Zeal search:" search))
      (setq search (concat search thing)))
    (if (executable-find "zeal")
        (start-process "Zeal" nil "zeal" "-q" search)
      (message "Zeal wasn't found"))))

(global-set-key (kbd "C-h z") 'zeal-search)
;; }}


;; {{start dictionary lookup
;; use below commands to create dicitonary
;; mkdir -p ~/.stardict/dic
;; # wordnet English => English
;; curl http://abloz.com/huzheng/stardict-dic/dict.org/stardict-dictd_www.dict.org_wn-2.4.2.tar.bz2 | tar jx -C ~/.stardict/dic
;; # Langdao Chinese => English
;; curl http://abloz.com/huzheng/stardict-dic/zh_CN/stardict-langdao-ec-gb-2.4.2.tar.bz2 | tar jx -C ~/.stardict/dic
;;
(autoload 'sdcv-search "sdcv" nil t)
(global-set-key (kbd "C-c d") 'sdcv-search)
;; }}

;; midnight mode purges buffers which haven't been displayed in 3 days
(require 'midnight)
(setq midnight-mode t)

;; make rainbow-delimiters more saturate
;; I don't know why it make daemon mode crash
(with-eval-after-load 'rainbow-delimiters
  (dotimes (index rainbow-delimiters-max-face-count)
    (let ((face
           (intern (format "rainbow-delimiters-depth-%d-face" (+ 1 index)))))
      (set-face-attribute face nil
                          :foreground
                          (color-saturate-name
                           (face-attribute face :foreground)
                           30)))))

;; {{ emacs2ram sync
(global-set-key [f6] 'emacs2ram-sync)

(defun emacs2ram-sync ()
  "call emacs2ram sync"
  (interactive)
  (message (shell-command-to-string "emacs2ram sync")))

(defun emacs2ram-restore ()
  "call emacs2ram restore"
  (interactive)
  (let (ask)
    (setq ask (read-string "Are you sure to restore? " "no"))
    (if (or (string= ask "yes") (string= ask "y"))
        (message (shell-command-to-string "emacs2ram restore")))))
;; }}

;; blew code will make initialization fail in daemon mode
(custom-unicode-font-size 14)

(provide 'init-misc-lazy)
;;; init-misc-lazy.el ends here

