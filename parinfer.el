(require 'parinferlib)
(require 'aggressive-indent)
(require 'cl-lib)

;; -----------------------------------------------------------------------------
;; Constants
;; -----------------------------------------------------------------------------
(defconst defun-regex "^[^ \n\t\"]")

(defvar parinfer-style 'paren)
(make-variable-buffer-local 'parinfer-style)

(defvar parinfer-first-load t)
(make-variable-buffer-local 'parinfer-first-load)

(defvar parinfer-indent-lighter " Parinfer:Indent")
(defvar parinfer-paren-lighter  " Parinfer:Paren")

(defun company-aggressive-indent-hook-fn (ignored)
  (when (not (in-comment-or-string-p))
    (call-interactively 'aggressive-indent-indent-defun)))

(defun parinfer-swith-to-indent-mode ()
  (if (not parinfer-first-load)
      (progn
        (parinfer-indent-all)
        (setq parinfer-style 'indent)
        (message "Parinfer: Indent Mode")
        (when (bound-and-true-p company-mode)
          (remove-hook 'company-completion-finished-hook 'company-aggressive-indent-hook-fn t))
        (force-mode-line-update))
    (when (parinfer-indent-with-confirm)
      (setq parinfer-style 'indent)
      (message "Parinfer: Indent Mode")
      (when (bound-and-true-p company-mode)
        (remove-hook 'company-completion-finished-hook 'company-aggressive-indent-hook-fn t))
      (force-mode-line-update))))

(defun parinfer-swith-to-paren-mode ()
  (setq parinfer-style 'paren)
  (message "Parinfer: Paren Mode")
  (when (bound-and-true-p company-mode)
    (add-hook 'company-completion-finished-hook 'company-aggressive-indent-hook-fn t t))
  (force-mode-line-update))

(defun toggle-parinfer-mode ()
  (interactive)
  (if (eq 'paren parinfer-style)
      (parinfer-swith-to-indent-mode)
    (parinfer-swith-to-paren-mode)))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------
(defun in-comment-or-string-p ()
  "http://ergoemacs.org/emacs/elisp_determine_cursor_inside_string_or_comment.html"
  (or (nth 3 (syntax-ppss))
      (nth 4 (syntax-ppss))))

(defun in-comment ()
  (interactive)
  (print (in-comment-or-string-p)))

(defun get-col-number ()
  (interactive)
  (current-column))

(defun buffer-string-no-properties ()
  (buffer-substring-no-properties
   (point-min)
   (point-max)))

(defun ruthlessly-kill-line ()
  (move-beginning-of-line 1)
  (kill-line 1)
  (setq kill-ring (cdr kill-ring)))

(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun delete-current-line ()
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

(defun current-line-empty ()
  (string= (thing-at-point 'line t)
           "\n"))

(defun goto-next-defun ()
  "goto next defun, skip comment or string."
  (let ((pt (point)))
    (when (not (eq (point-max) pt))
      (if (search-forward-regexp defun-regex nil t)
          (if (= (1+ pt) (point))
              (goto-next-defun)
            (while (and (in-comment-or-string-p)
                        (not (eq (point-max) (point))))
              (search-forward-regexp defun-regex nil t))
            (backward-char))
        (end-of-buffer)))))

(defun goto-next-defun* ()
  "fortest"
  (interactive)
  (goto-next-defun))

(defun goto-previous-defun ()
  "goto previous defun, skip comment or string"
  (search-backward-regexp defun-regex nil t)
  (while (and (in-comment-or-string-p)
              (not (eq (point-min) (point))))
    (search-backward-regexp defun-regex nil t)))

(defun goto-previous-defun* ()
  (interactive)
  (goto-previous-defun))

(defun point-cursor-line (pt)
  "Zero-based line number for point."
  (save-excursion
    (goto-char pt)
    (1- (line-number-at-pos))))

(defun point-cursor-x (pt)
  "Zero-based column number for point"
  (save-excursion
    (goto-char pt)
    (current-column)))

;; -----------------------------------------------------------------------------
;; Parinfer functions
;; -----------------------------------------------------------------------------

(defun parinfer-diff ()
  (interactive)
  (let* ((orig-text (buffer-substring-no-properties (point-min) (point-max)))
         (new-buffer (generate-new-buffer "*Parinfer Result*"))
         (orig-buffer (current-buffer))
         (result (parinferlib-indent-mode orig-text nil)))
    (with-current-buffer new-buffer
      (erase-buffer)
      (insert (plist-get result :text))
      (ediff-buffers orig-buffer new-buffer))))

(defun parinfer-indent ()
  (interactive)
  (let* ((start (save-excursion (goto-previous-defun) (point)))
         (end (save-excursion (goto-next-defun) (point)))
         (text (buffer-substring-no-properties start end))
         (line-number (line-number-at-pos))
         (cursor-line (- line-number (line-number-at-pos start)))
         (cursor-x (current-column))
         (opts (list :cursor-x cursor-x :cursor-line cursor-line))
         (result (parinferlib-indent-mode text opts)))
    (when (and (plist-get result :success)
               (plist-get result :changed-lines))
      (delete-region start end)
      (insert (plist-get result :text))
      (goto-line line-number)
      (beginning-of-line 1)
      (forward-char (plist-get result :cursor-x))))) 

(defun parinfer-indent-all ()
  (interactive)
  (let* ((cursor-line (1- (line-number-at-pos)))
         (cursor-x (current-column))
         (opts (list :cursor-line cursor-line :cursor-x cursor-x))
         (text (buffer-string-no-properties))
         (result (parinferlib-indent-mode text opts))
         (changed-lines (plist-get result :changed-lines)))
    (when (and (plist-get result :success)
               changed-lines)
      (cl-loop for l in changed-lines do
               (goto-line (1+ (plist-get l :line-no)))
               (delete-region (line-beginning-position)
                              (line-end-position))
               (insert (plist-get l :line)))
      (goto-line (1+ cursor-line))
      (beginning-of-line 1)
      (forward-char (plist-get result :cursor-x)))))

(defun parinfer-indent-with-confirm ()
  (interactive)
  (let* ((cursor-line (1- (line-number-at-pos)))
         (cursor-x (current-column))
         (opts (list :cursor-line cursor-line :cursor-x cursor-x))
         (text (buffer-string-no-properties))
         (result (parinferlib-indent-mode text opts))
         (success (plist-get result :success))
         (changed-lines (plist-get result :changed-lines)))
    (if (not success)
        (progn
          (message "Pairs unmatched, swith to Paren mode. When pair fiexed, You can switch to indent mode.")
          nil)
      (if (and changed-lines
               (not (string= text (plist-get result :text))))
          (if (y-or-n-p "Caution! Buffer will be modified if you swith to Indent mode, continue? Y for indent mode, N for paren mode.")
              (progn (cl-loop for l in changed-lines do
                              (goto-line (1+ (plist-get l :line-no)))
                              (delete-region (line-beginning-position)
                                             (line-end-position))
                              (insert (plist-get l :line)))
                     (goto-line (1+ cursor-line))
                     (beginning-of-line 1)
                     (forward-char (plist-get result :cursor-x))
                     (setq parinfer-first-load nil)
                     t)
            nil)
        t))))

(defun parinfer-paren ()
  (interactive)
  (let* ((start (save-excursion (goto-previous-defun) (point)))
         (end (save-excursion (goto-next-defun) (point)))
         (text (buffer-substring-no-properties start end))
         (line-number (line-number-at-pos))
         (cursor-line (- line-number (line-number-at-pos start)))
         (cursor-x (current-column))
         (opts (list :cursor-x cursor-x :cursor-line cursor-line))
         (result (parinferlib-indent-mode text opts)))
    (when (not (plist-get result :changed-lines))
      (call-interactively 'aggressive-indent-indent-defun))))

(defun parinfer-hook-fn ()
  (cond
   ((eq 'paren parinfer-style) (parinfer-paren))
   ((eq 'indent parinfer-style) (parinfer-indent))
   (t "nothing")))

(defun parinfer-newline ()
  (interactive)
  (newline-and-indent)
  (parinfer-hook-fn))

(defun parinfer-backward-delete-char ()
  (interactive)
  (backward-delete-char 1)
  (parinfer-hook-fn))

(defun parinfer-backward-kill-word ()
  (interactive)
  (call-interactively 'backward-kill-word)
  (parinfer-hook-fn))

(defun parinfer-delete-char ()
  (interactive)
  (delete-char 1)
  (parinfer-hook-fn))

(defun parinfer-kill-word ()
  (interactive)
  (call-interactively 'kill-word)
  (parinfer-hook-fn))

(defun parinfer-kill-line ()
  (interactive)
  (call-interactively 'kill-line)
  (parinfer-hook-fn))

(defvar parinfer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-9") 'toggle-parinfer-mode)
    (define-key map (kbd "<backspace>") 'parinfer-backward-delete-char)
    (define-key map (kbd "M-<backspace>") 'parinfer-backward-kill-word)
    (define-key map (kbd "C-k") 'parinfer-kill-line)
    (define-key map (kbd "C-d") 'parinfer-backward-delete-char)
    (define-key map (kbd "M-d") 'parinfer-backward-delete-char)
    map))

(defun enable-parinfer ()
  (run-hooks 'parinfer-mode-enable-hook)
  (add-hook 'post-self-insert-hook 'parinfer-hook-fn t t)
  ;; Always use whitespace for indentation.
  (setq-mode-local parinfer-mode indent-tabs-mode nil)
  (parinfer-swith-to-indent-mode))

(defun disable-parinfer ()
  (run-hooks 'parinfer-mode-disable-hook)
  (remove-hook 'post-self-insert-hook 'parinfer-hook-fn t))

(defun current-lighter ()
  (if (eq 'paren parinfer-style)
      parinfer-paren-lighter
    parinfer-indent-lighter))

;;;###autoload
(define-minor-mode parinfer-mode
  "Parinfer mode."
  nil (:eval (current-lighter)) parinfer-mode-map
  (if parinfer-mode
      (enable-parinfer)
    (disable-parinfer)))

(provide 'parinfer)
