(autoload 'spaceline-install "spaceline" nil t)

(setq-default mode-line-format nil)
 (defface  window-numbering-face
    '((t (:foreground "DeepPink" :weight bold)))
    "Window number face"
    :group 'mode-line)

  (defface flycheck-mode-line-off-face
    '((t :weight normal :foreground "tan"))
    "Face for the modeline when flycheck mode is not activate"
    :group 'flycheck-faces)

  (defface flycheck-mode-line-run-face
    '((t :weight normal :foreground "DeepSkyBlue"))
    "Face for the modeline in buffers with Flycheck running."
    :group 'flycheck-faces)

  (defface flycheck-mode-line-error-face
    '((t :weight normal :foreground "DarkRed"))
    "Face for the modeline when flycheck exited with an error"
    :group 'flycheck-faces)

  (defface flycheck-mode-line-ok-face
    '((t  :weight normal :foreground "DarkOliveGreen"))
    "Face for the modeline when buffer is ok"
    :group 'flycheck-faces)

  (defface flycheck-mode-line-failed-face
    '((t :weight normal :foreground "OrangeRed"))
    "Face for the modeline when flycheck failed to parse buffer"
    :group 'flycheck-faces)

  (defface flycheck-mode-line-result-face
    '((t :weight normal :foreground "Orange"))
    "Face for the modeline when the buffer has warnings or errors"
    :group 'flycheck-faces)

(with-eval-after-load 'spaceline
  (spaceline-define-segment buffer-mod-or-tmp
    "Buffer is modified"
    (if (is-buffer-file-temp)
        "tmp"
      (when (buffer-modified-p)
        (propertize "mod"
                    'face nil
                    'help-echo "Buffer has been modified"))))

  (spaceline-define-segment buffer-ro
    "Buffer is read-only"
    (when buffer-read-only
      (propertize "ro"
                  'face nil
                  'help-echo "Buffer is read-only")))

  (spaceline-define-segment major-mode
    "Buffer major mode"
    (propertize "%m" 'face font-lock-builtin-face))

  (spaceline-define-segment process
    "The process associated with buffer"
    (when (spaceline--mode-line-nonempty mode-line-process)
      (s-trim (powerline-raw mode-line-process))))

  (spaceline-define-segment buffer-encoding
    "The line ending convention used in the buffer."
    (let ((buf-coding (format "%s" buffer-file-coding-system)))
      (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
          (match-string 1 buf-coding)
        buf-coding)))

  (spaceline-define-segment buffer-id
    "Buffer id include remote host"
    (let* ((host (and  default-directory
                      (file-remote-p default-directory 'host)))
           (num (if (bound-and-true-p window-numbering-mode)
                    (window-numbering-get-number-string)
                  ""))
           (real-id (if host
                        (concat "%b" "@" host)
                      "%b")))
      (if (> (recursion-depth) 0)
          (concat num " %[" real-id "%]")
        (concat num " " real-id))))

  (spaceline-define-segment flycheck-info
    "flycheck infomation"
    (if (and (boundp 'flycheck-mode)
            (symbol-value 'flycheck-mode))
        (let ((str (s-trim (flycheck-mode-line-status-text)))
              (msg "")
              (face nil))
          (cond ((string= str "")
                 (setq msg "ok")
                 (setq face 'flycheck-mode-line-ok-face))
                ((string= str "*")
                 (setq msg "run")
                 (setq face 'flycheck-mode-line-run-face))
                ((string= str "-")
                 (setq msg "no")
                 (setq face 'flycheck-mode-line-off-face))
                ((string= str "!")
                 (setq msg "fail")
                 (setq face 'flycheck-mode-line-failed-face))
                ((string= str "?")
                 (setq msg "err")
                 (setq face 'flycheck-mode-line-error-face))
                (t (setq face 'flycheck-mode-line-result-face)
                   (setq msg (s-replace ":" "" str))))
          (propertize msg 'face face))))

  (spaceline-define-segment workgroup
    "workgroups2 extensions"
    (when (and (boundp 'workgroups-mode)
              (symbol-value 'workgroups-mode))
      (let ((name (condition-case nil
                      (wg-workgroup-name (wg-current-workgroup))
                    (error ""))))
        (if (string= name "")
            (setq name "none"))
        (concat "<"
                (propertize name
                            'face 'font-lock-function-name-face
                            'help-echo (format "Current workgroup is %s" name))
                ">"))))

  (spaceline-define-segment version-control
    "vc info"
    (powerline-vc 'font-lock-constant-face 'r)))

(add-hook 'after-init-hook
          '(lambda ()
             (setq powerline-height (+ 2 current-unicode-font-size)
                   powerline-default-separator nil
                   spaceline-byte-compile t)
             (spaceline-install '((buffer-id :face highlight-face)
                                  (major-mode process)
                                  ((buffer-mod-or-tmp
                                    buffer-ro
                                    buffer-encoding) :separator "|"))
                                '(version-control
                                  (workgroup flycheck-info)
                                  "%I(%l-%c)%p"))

             (window-numbering-mode 1)
             (setq-default mode-line-format
                           '("%e" (:eval (spaceline-ml-main))))
             ;; no fringe
             (fringe-mode '(8 . 0))
               (require 'color nil :noerror)
               (let ((bg (face-attribute 'default :background)))
                 (set-face-attribute 'mode-line nil
                        :box nil
                        :background (color-lighten-name bg 5))
                 (set-face-attribute 'mode-line-inactive nil
                        :box nil
                        :background bg)
                 (set-face-attribute 'fringe nil
                                     :background bg))))

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'init-modeline)

;;; init-modeline ends here
