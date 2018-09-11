;;; -*- lexical-binding: t; -*-

(defface ccls-code-lens-face
  '((t :inherit shadow))
  "The face used for code lens overlays."
  :group 'ccls-code-lens)

(defface ccls-code-lens-mouse-face
  '((t :box t))
  "The face used for code lens overlays."
  :group 'ccls-code-lens)

(defun ccls//code-lens-overalys (&optional -beg -end)
  (--filter (overlay-get it 'ccls-code-lens)
            (overlays-in (or -beg (point-min)) (or -end (point-max)))))

(defun ccls//code-lens-string (-command)
  "."
  (propertize (plist-get -command :title)
              'face 'ccls-code-lens-face
              'mouse-face 'ccls-code-lens-mouse-face))

(defun ccls//display-code-lens (-result)
  "."
  (overlay-recenter (point-max))
  (mapc #'delete-overlay (ccls//code-lens-overalys))
  (let (buffers)
    (seq-doseq (lens -result)
      (cl-destructuring-bind (&key range command &allow-other-keys) lens
        (let ((buffer (find-buffer-visiting
                       (eglot--uri-to-path
                        (plist-get (plist-get command :arguments) :uri)))))
          (eglot--with-live-buffer buffer
            (save-restriction
              (save-excursion
                (widen)
                (when (not (member buffer buffers))
                  (mapc #'delete-overlay (ccls//code-lens-overalys))
                  (overlay-recenter (point-max))
                  (push buffer buffers))
                (cl-destructuring-bind (&key start end) range
                  (let ((ov (make-overlay (eglot--lsp-position-to-point start)
                                          (eglot--lsp-position-to-point end)
                                          buffer)))
                    (overlay-put ov 'ccls-code-lens t)
                    (overlay-put ov 'after-string
                                 (concat " "
                                         (ccls//code-lens-string command)))))))))))))

;;;###autoload
(defun ccls//toggle-code-lens ()
  "Request code lens from ccls."
  (interactive)
  (let ((ovs (ccls//code-lens-overalys)))
    (if ovs
        (mapc #'delete-overlay ovs)
      (ccls//display-code-lens
       (jsonrpc-request (eglot--current-server-or-lose)
                        :textDocument/codeLens
                        (eglot--TextDocumentPositionParams))))))
