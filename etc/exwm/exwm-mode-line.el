(add-to-list 'mode-line-config-alist
             '(exwm%exwm-window? (exwm%mode-line-workspace
                                  mode-line%exwm-left-button
                                  mode-line%buffer-id)
                                 ()
                                 :no-tail
                                 :no-center))

(defun exwm%exwm-window? (bn &optional action)
  (with-current-buffer bn
    (eq major-mode 'exwm-mode)))

(defun exwm%create-button (button-line
                           button-name
                           button-face
                           mouse-1-action)
  (propertize button-name
              'face button-face
              'mouse-face 'mode-line-highlight
              'local-map
              (let ((map (make-sparse-keymap)))
                (define-key map (vector button-line 'mouse-1)
                  `(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-1-action)))
                map)))

(defun mode-line%exwm-left-button ()
  (list " "
        (exwm%create-button
         'mode-line "[X]" 'error
         '(kill-buffer))
        " "
        (exwm%create-button
         'mode-line "[-]" 'font-lock-builtin-face
         '(if (one-window-p)
              (switch-to-buffer (other-buffer))
            (delete-window)))))

;; set $ws_chrome "2:"
;; set $ws_nautilus "3:"
;; set $ws_doc "4:"
;; set $ws_display "5:"
;; set $ws_office "6:"
;; set $ws_remote "7:"

;; (defvar workspace-icon-list
;;   '("" "" ""))

(defun exwm%mode-line-workspace ()
  (when mode-line-active?
    (set-window-parameter (selected-window)
                          'workspace-index
                          exwm-workspace-current-index))
  (let ((index (window-parameter (selected-window) 'workspace-index)))
    (list (mode-line%window-number)
          (propertize (format "W%s" (or index "?"))
                      'face font-lock-string-face))))

(provide 'exwm-mode-line)
