;; prolog system
(setq prolog-system 'swi)
;; css
(setq csv-separators '("," ";" "|" " "))

(defalias 'perl-mode 'cperl-mode)

(with-eval-after-load 'fcitx
  ;; init fcitx prefix keys
  (setq fcitx-use-dbus t)
  (fcitx-prefix-keys-add "C-h" "M-g"))

;; popwin setup
(autoload 'popwin-mode "popwin" nil t)

(with-eval-after-load 'popwin
  (global-set-key (kbd "C-z") popwin:keymap))

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


(global-set-key (kbd "C-h z") 'zeal-at-point)
(global-set-key (kbd "C-h Z") 'zeal-at-point-search)
(with-eval-after-load 'zeal-at-point
  (setf (cdr (assoc 'c++-mode zeal-at-point-mode-alist)) "cpp"))

;;  start dictionary lookup
(autoload 'sdcv-search "sdcv" nil t)
(global-set-key (kbd "C-c D") 'sdcv-search)

;; midnight mode purges buffers which haven't been displayed in 3 days
(require 'midnight)
(setq midnight-mode t)

(with-eval-after-load 'restclient
  (bind-keys :map restclient-mode-map
             ("C-c C-p") ("M-p" . restclient-jump-prev)
             ("C-c C-n") ("M-n" . restclient-jump-next)))

(add-hook 'restclient-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-restclient)))

(with-eval-after-load 'octave
  (defun inferior-octave-return-to-buffer ()
    (interactive)
    (let ((bufs (loop for buf in (buffer-list)
                      if (with-current-buffer buf (eq major-mode 'octave-mode))
                      collect (buffer-name buf))))
      (if (= 1 (length bufs))
          (pop-to-buffer (car bufs))
        (ivy-read "Select buffer: " bufs
                  :action #'pop-to-buffer))))
  (defun octave-mode-return-to-inferior ()
    (interactive)
    (let ((inferior (and (inferior-octave-process-live-p)
                        (process-buffer inferior-octave-process))))
      (if inferior
          (pop-to-buffer inferior)
        (message "No inferior-octave buffer is alive !"))))
  (setq inferior-octave-prompt-read-only t)
  (bind-key "C-c C-c"
            'run-octave
            octave-mode-map)
  (bind-key "C-c C-z"
            'octave-mode-return-to-inferior
            octave-mode-map)
  (bind-key "C-c C-z"
            'inferior-octave-return-to-buffer
            inferior-octave-mode-map))

(defconst ascii-before-chinese
  (rx (group-n 1 (in "a-zA-Z0-9!@#$%^&\\-+|)\\]}\\:;?><.,"))
      (group-n 2 (category chinese-two-byte))))
(defconst ascii-after-chinese
  (rx (group-n 1 (category chinese-two-byte))
      (group-n 2 (in "a-zA-Z0-9@#$%^&\\-+|(\\[{\\></"))))

(defun insert-space-around-chinese (&optional start end)
  (interactive)
  (if (region-active-p)
      (setq start (region-beginning)
            end (region-end))
    (setq start (point-min)
          end (point-max)))
  (save-excursion
    (goto-char start)
    (while (re-search-forward ascii-before-chinese end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char start)
    (while (re-search-forward ascii-after-chinese end t)
      (replace-match "\\1 \\2" nil nil))))

(global-set-key (kbd "M-Q") 'insert-space-around-chinese)


(provide 'init-extra)
