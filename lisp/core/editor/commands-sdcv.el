;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(require 'view)
(require 'ansi-color)


(defvar sdcv-wait-timeout 2
  "The max time (in seconds) to wait for the sdcv process to produce some output.")

(defvar sdcv-wait-interval 0.01
  "The interval (in seconds) to sleep each time to wait for sdcv's output.")

(defconst sdcv-process-name "SDCV")
(defconst sdcv-process-buffer-name " *sdcv-mode-process*")

(defvar sdcv-word-prompts '("Enter word or phrase: "
                            "请输入单词或短语："
                            "請輸入單字或片語：")
  "A list of prompts that sdcv use to prompt for word.")

(defvar sdcv-choice-prompts '("Your choice[-1 to abort]: "
                              "您的选择为："
                              "您的選擇為：")
  "A list of prompts that sdcv use to prompt for a choice of multiple candicates.")

(defvar sdcv-result-patterns '("^Found [0-9]+ items, similar to [*?/|]*\\(.+?\\)[*?]*\\."
                               "^发现 [0-9]+ 条记录和 [*?/|]*\\(.+?\\)[*?]* 相似。")
  "A list of patterns to extract result word of sdcv. Special characters are stripped.")

(defvar sdcv-buffer-name "*sdcv*"
  "The name of the buffer of sdcv.")

(defvar sdcv-program-path "sdcv" "The path of sdcv program.")

(defvar sdcv-dictionary-path nil "The path of dictionaries.")

(defvar sdcv-item-regexp-partial ").*?-->\\(.*\\)")



(defsubst sdcv-timeout-p (-start-time)
  (>= (float-time (time-since -start-time)) sdcv-wait-timeout))

(defsubst sdcv-buffer-tail (-length)
  "Get a substring of length LENGTH at the end of current buffer."
  (buffer-substring-no-properties
   (max
    (point-min)
    (- (point-max) -length))
   (point-max)))

(defun sdcv-match-tail (-prompts)
  (cl-loop
   for prompt in -prompts
   for beg = (max (point-min) (- (point-max) (length prompt)))
   for end = (point-max)
   when (string-equal prompt (buffer-substring-no-properties beg end))
   return (progn
            (delete-region beg end)
            t)))

(defun sdcv-get-process ()
  "Get or create the sdcv process."
  (or (get-process sdcv-process-name)
      (with-current-buffer (get-buffer-create sdcv-process-buffer-name)
        (erase-buffer)
        (let ((process (apply 'start-process
                              sdcv-process-name
                              sdcv-process-buffer-name
                              sdcv-program-path
                              "-c"
                              (when sdcv-dictionary-path
                                (list "--data-dir" sdcv-dictionary-path)))))
          (set-process-query-on-exit-flag process nil)
          ;; kill the initial prompt
          (let ((time (current-time)))
            (while (and (not (sdcv-match-tail sdcv-word-prompts))
                        (not (sdcv-timeout-p time)))
              (sleep-for sdcv-wait-interval))

            (when (> (float-time (time-since time)) sdcv-wait-timeout)
              (kill-process process)
              (user-error "ERROR: timeout waiting for sdcv")))
          process))))

(defun sdcv-do-lookup (-word)
  "Send the -word to the sdcv process and return the result."
  (let ((process (sdcv-get-process)))
    (process-send-string process (concat -word "\n"))
    (with-current-buffer (process-buffer process)
      (unwind-protect
          (let* ((time (current-time)))
            (prog1 (catch 'done
                     (while (not (sdcv-timeout-p time))
                       (when (sdcv-match-tail sdcv-word-prompts)
                         (throw 'done (ansi-color-apply (buffer-string))))

                       (when (sdcv-match-tail sdcv-choice-prompts)
                         (process-send-string process "-1\n"))

                       (sleep-for sdcv-wait-interval)))
              (when (sdcv-timeout-p time)
                (kill-process process)
                (user-error "ERROR: timeout waiting for sdcv"))))
        (erase-buffer)))))




(defun sdcv-get-buffer ()
  "Get the sdcv buffer. Create one if there's none."
  (let ((buffer (get-buffer-create sdcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-mode)
        (org-mode)
        (setq buffer-read-only t)
        (let ((map (copy-keymap (current-local-map))))
          (define-key map "q" #'quit-window)
          (define-key map "\\" #'View-search-regexp-backward)
          (define-key map "/" #'View-search-regexp-forward)
          (define-key map "r" #'swiper-isearch-backward)
          (define-key map "s" #'swiper-isearch)
          (define-key map "\n" #'View-scroll-line-forward)
          (define-key map "y" #'View-scroll-line-backward)
          (define-key map " " #'View-scroll-page-forward)
          (define-key map "u" #'View-scroll-half-page-backward)
          (define-key map (kbd "DEL") #'View-scroll-page-backward)
          (define-key map "n" #'next-line)
          (define-key map "p" #'previous-line)
          (define-key map "d" #'sdcv-search)
          (define-key map "?" #'describe-mode)
          (dotimes (i 10)
            (define-key map (number-to-string i) #'sdcv-goto-number))
          (use-local-map map))))
    buffer))

;;;###autoload
(defun sdcv-search-word (-word)
  "Search WORD through the command-line tool sdcv.
The result will be displayed in buffer named with `sdcv-buffer-name' with `org-mode'.

Word may contain some special characters:
    *       match zero or more characters
    ?       match zero or one character
    /       used at the beginning, for fuzzy search
    |       used at the beginning, for data search
    \       escape the character right after"
  (interactive
   (list
    (let ((word (if (region-active-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (current-word))))
      (read-string (format "Search (default %s): " word) nil nil word))))
  (with-current-buffer (sdcv-get-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (or (sdcv-do-lookup -word) ""))
      (goto-char (point-min)))
    (pop-to-buffer (current-buffer))))

(defun sdcv-goto-number (-num-str)
  (interactive (list (this-command-keys)))
  (let ((regexp (concat -num-str sdcv-item-regexp-partial)))
    (if-let (word (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward regexp nil t)
                      (match-string 1))))
        (sdcv-search-word word)
      (sdcv-search))))
