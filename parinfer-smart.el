;;; -*- lexical-binding: t; -*-

(require 'selected)
(require 'cl)
(require 'paredit)


;; -----------------------------------------------------------------------------
;; DEVELOPMENT
;; -----------------------------------------------------------------------------

(defvar-local parinfer--process-serial 0)

(defun parinfer--log (s &rest args)
  (let ((serial parinfer--process-serial))
    (with-current-buffer "*parinfer-log*"
      (goto-char (point-min))
      (insert (format "#%06d" serial))
      (insert (apply #'format s args))
      (insert "\n"))))

(defmacro parinfer--measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fms" (* (float-time (time-since time)) 1000))))

(defun parinfer--bench ()
  (interactive)
  (parinfer--measure-time
   (parinfer--process-change)))

;; -----------------------------------------------------------------------------
;; CONSTANTS
;; -----------------------------------------------------------------------------

(defconst parinfer--whitespace 32)
(defconst parinfer--doublequote 34)
(defconst parinfer--semicolon 59)
(defconst parinfer--paren-open 40)
(defconst parinfer--paren-close 41)
(defconst parinfer--bracket-open 91)
(defconst parinfer--bracket-close 93)
(defconst parinfer--brace-open 123)
(defconst parinfer--brace-close 125)
(defconst parinfer--backslash 92)

(defconst parinfer--error-eol-backslash "Line cannot end in a hanging backslash.")
(defconst parinfer--error-quote-danger "Quotes must balanced inside comment blocks.")
(defconst parinfer--error-unclosed-quote "String is missing a closing quote.")
(defconst parinfer--error-unclosed-paren "Unclosed open-paren.")

;; -----------------------------------------------------------------------------
;; VARIABLES
;; -----------------------------------------------------------------------------


(defvar parinfer--ignore-commands
  '(undo undo-tree-undo undo-tree-redo
         parinfer-shift-left parinfer-shift-right
         clojure-align yank yank-pop indent-region))

(defvar-local parinfer--in-string nil
  "If we are in string.")

(defvar-local parinfer--quote-danger nil
  "If we are quote danger (unclose string in comment).")

(defvar-local parinfer--in-comment nil
  "If we are in comment.")

(defvar-local parinfer--in-char nil
  "If we are after a backslash.
This mean, the current position is a character.")

(defvar-local parinfer--next-indent nil
  "The x of next indentation.")

(defvar-local parinfer--trail nil
  "The position of the last trail.")

(defvar-local parinfer--empty-line nil
  "This line is empty or will be empty.")

(defvar-local parinfer--paren-stack nil
  "Save the paren of current line.
Element struct: (point . ch)")

(defvar-local parinfer--line-indents nil
  "Stack of indent of each line.
Element struct: ((line-begin-point . line-end-point) . delta)")

(defvar-local parinfer--opener-stack nil
  "Save opener that come from `parinfer--paren-stack'.")

(defvar-local parinfer--op-stack nil
  "Save all we need to do with current buffer.
Element struct: (point . val)
If val > 0, it represent a char to insert;
If val < 0, it represent a deletion length.")

(defvar-local parinfer--edit-begin nil
  "The line beginning of the cursor line.
All text after this position, before `parinfer-edit-end' will be preserved.")

(defvar-local parinfer--edit-end nil
  "The cursor x position.
All text after `parinfer--edit-begin' and before this position will be preserved.")

(defvar-local parinfer--lock-begin nil
  "The beginning of the last sexp of cursor line, if this sexp is a multiline sexp, otherwise nil.")

(defvar-local parinfer--lock-end nil
  "The end of the last sexp of cursor line.")

(defvar-local parinfer--lock-line-begin nil)

(defvar-local parinfer--lock-line-count nil)

(defvar-local parinfer--lock-paren-stack nil
  "")

(defvar-local parinfer--buffer-will-change nil)

(defvar-local parinfer--scope-begin nil
  "The beginning of the sexp where the cursor in.")

(defvar-local parinfer--scope-end nil
  "The end of the sexp where the cursor in.")

(defvar-local parinfer--at-indent nil
  "If we are at the beginning of code.
Set to t when we at the indentation position. and set to nil when we meet non-closer char.")

;; (defvar-local parinfer--delta nil
;;   "The x that cursor moved before/after buffer changed.")

(defvar-local parinfer--prev-x nil
  "The x before text changed.")

(defvar-local parinfer--processing nil
  "A flag indicate parinfer mode is processing buffer.")

(defvar-local parinfer--reindent-position nil)

;; -----------------------------------------------------------------------------
;; UTILITIES
;; -----------------------------------------------------------------------------

(defun parinfer--opener-to-closer (ch)
  (if (= ch parinfer--paren-open)
      parinfer--paren-close
    (if (= ch parinfer--bracket-open)
        parinfer--bracket-close
      parinfer--brace-close)))

(defun parinfer--closer-to-opener (ch)
  (if (= ch parinfer--paren-close)
      parinfer--paren-open
    (if (= ch parinfer--bracket-close)
        parinfer--bracket-open
      parinfer--brace-open)))

(defun parinfer--get-buffer-max-line ()
  (1- (line-number-at-pos (point-max))))

;; -----------------------------------------------------------------------------
;; PREDICATES
;; -----------------------------------------------------------------------------

(defun parinfer--handle-indent-delta-p ()
  (not (region-active-p)))

(defun parinfer--skip-p ()
  (or
   (bound-and-true-p multiple-cursors-mode)
   (bound-and-true-p cua-mode)
   (seq-contains parinfer--ignore-commands this-command)))

(defun parinfer--line-end-p ()
  (= (point) (line-end-position)))

(defun parinfer--opener-p (ch)
  (and ch
       (or (= ch parinfer--paren-open)
           (= ch parinfer--bracket-open)
           (= ch parinfer--brace-open))))

(defun parinfer--closer-p (ch)
  (and ch
       (or (= ch parinfer--paren-close)
           (= ch parinfer--bracket-close)
           (= ch parinfer--brace-close))))

(defun parinfer--empty-line-p ()
  (string-match "^ *$"
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))

(defun parinfer--has-trail-p ()
  "Must call when we make sure we are after trail."
  (not
   (or parinfer--empty-line
       parinfer--in-string)))

(defun parinfer--has-indent-p ()
  "Must call when we make sure we are at the line beginning."
  (not parinfer--in-string))

(defun parinfer--in-edit-p ()
  (<= parinfer--edit-begin (point) parinfer--edit-end))

(defun parinfer--in-lock-p ()
  (or (and (not parinfer--lock-end)
           (> (point) parinfer--lock-begin))
      (and parinfer--lock-end
           (<= (point) parinfer--lock-end)
           (> (point) parinfer--lock-begin))))

;; -----------------------------------------------------------------------------
;; NAVIGATIONS
;; -----------------------------------------------------------------------------

(defun parinfer--goto-next-meaningful-position (end)
  "Goto the next position, where have one of following:
semicolon, doublequote, backslash, opener, closer"
  (if parinfer--in-comment
      (skip-syntax-forward "^\"^\\" end)
    (skip-syntax-forward "^<^\"^(^)^\\" end)))

(defun parinfer--goto-previous-trail ()
  "Assume we are not in comment or at the beginning of the comment.
If there's whitespaces before parens and after orig cursor x,
these whitespaces will be marked delete."
  ;; Seems useless
  ;; (when (and (equal (char-after) parinfer--semicolon)
  ;;            (equal (char-before) parinfer--whitespace))
  ;;   (backward-char))
  (let ((before-paren nil)
        (in-code nil))
    (while (and (not in-code)
                (not (parinfer--in-edit-p)))
      (let ((ch (char-before)))
        (cond
         ((equal ch parinfer--whitespace)
          (when before-paren
            (push (cons (1- (point)) -1) parinfer--op-stack))
          (backward-char))

         ((parinfer--closer-p ch)
          (setq before-paren t)
          (backward-char))

         (t (setq in-code t)))))))

;; ---------

(defun parinfer--remove-line-begin-closer ()
  (let ((found nil))
    (while (parinfer--closer-p (char-after))
      (push (cons (point) -1)
            parinfer--op-stack)
      (forward-char)
      (setq found t))
    found))

(defun parinfer--goto-indentation ()
  "Goto the indentation, and mark all leading closer delete.
If this is a comment only line or empty-line, set `parinfer--empty-line' t."
  (setq parinfer--empty-line nil)
  (back-to-indentation)
  (parinfer--remove-line-begin-closer)
  (let ((end (line-end-position)))
    (while (and (< (point) end)
                (equal parinfer--whitespace (char-after)))
      (forward-char))
    (when (or (equal (char-after) parinfer--semicolon)
              (= (point) end))
      (setq parinfer--empty-line t))))

;; -----------------------------------------------------------------------------
;; EXECUTORS
;; -----------------------------------------------------------------------------

(defun parinfer--indent-range (begin end delta)
  "Indent lines from begin to end, skip the line at begin, include the line at end."
  (save-excursion
    (let ((begin-line (line-number-at-pos begin))
          (end-line (line-number-at-pos end)))
      ;; (parinfer--log "indent form %d to %d"
      ;;                begin-line
      ;;                end-line)
      (goto-line begin-line)
      (cl-loop for i from begin-line to (1- end-line) do
               (forward-line)
               (let ((begin (line-beginning-position))
                     (end (line-end-position)))
                 (push (cons (cons begin end) delta)
                       parinfer--line-indents))))))

(defun parinfer--execute-op-1 (op)
  (let ((pos (car op))
        (val (cdr op)))
    ;; (parinfer--log "val %s line %d x %d"
    ;;                val
    ;;                (save-excursion
    ;;                  (goto-char pos)
    ;;                  (line-number-at-pos))
    ;;                (save-excursion
    ;;                  (goto-char pos)
    ;;                  (- (point) (line-beginning-position))))
    (if (> val 0)
        (progn
          (goto-char (min pos (point-max)))
          (insert val))
      (progn
        (goto-char pos)
        (backward-delete-char val)))))

(defun parinfer--op-sort-fn (x y)
  (> (+ (* 2 (car x))
        (if (< (cdr x) 0) 1 0))
     (+ (* 2 (car y))
        (if (< (cdr y) 0) 1 0))))

(defun parinfer--execute-op-stack ()
  ;; (parinfer--log "op-stack:%s" parinfer--op-stack)
  (let ((ordered-op-stack (sort parinfer--op-stack
                                #'parinfer--op-sort-fn)))
    ;; (parinfer--log "ordered-op-stack:%s" ordered-op-stack)
    (mapc #'parinfer--execute-op-1
          ordered-op-stack)
    (setq parinfer--op-stack nil)))

;; -----------------------------------------------------------------------------
;; PROCESSING
;; -----------------------------------------------------------------------------

(defun parinfer--initial-states ()
  ;; (parinfer--log "[parinfer--initial-states]")
  (setq parinfer--in-char nil
        parinfer--in-comment nil
        parinfer--in-string nil
        parinfer--quote-danger nil
        parinfer--scope-begin nil
        parinfer--scope-end nil
        parinfer--edit-begin (line-beginning-position)
        parinfer--edit-end (if (region-active-p)
                               (line-beginning-position)
                             (point))
        parinfer--lock-begin (point)
        parinfer--lock-end nil
        parinfer--lock-line-count nil
        parinfer--lock-line-begin nil
        parinfer--paren-stack nil
        parinfer--opener-stack nil
        parinfer--trail nil
        parinfer--op-stack nil
        parinfer--line-indents nil
        parinfer--reindent-position nil
        parinfer--prev-x nil)
  ;; (parinfer--log "prev-x: %s delta: %s" parinfer--prev-x parinfer--delta)
  )

(defun parinfer--process-code-end ()
  "When we at the end of code, we should search back for the trail position."
  (when (parinfer--has-trail-p)
    (save-excursion
      (parinfer--goto-previous-trail)
      (setq parinfer--trail (point)))))

(defun parinfer--on-doublequote ()
  (if parinfer--in-comment
      (setq parinfer--quote-danger (not parinfer--quote-danger))
    (setq parinfer--in-string (not parinfer--in-string))))

(defun parinfer--on-semicolon ()
  (unless (or parinfer--in-string
              parinfer--in-comment)
    (setq parinfer--in-comment t)
    (parinfer--process-code-end)))

(defun parinfer--on-backslash ()
  (forward-char)
  (when (= (point) (line-end-position))
    (error parinfer--error-eol-backslash)))

(defun parinfer--on-parens (ch)
  (unless (or parinfer--in-comment
              parinfer--in-string)
    (push (cons (point) ch)
          parinfer--paren-stack)))

(defun parinfer--on-end-line ()
  (if parinfer--in-comment
      (if parinfer--quote-danger
          (error parinfer--error-quote-danger)
        (setq parinfer--in-comment nil))
    (unless parinfer--in-string
      (parinfer--process-code-end))))

(defun parinfer--get-line-indent (pos)
  (while (> pos (cdaar parinfer--line-indents))
    (pop parinfer--line-indents))
  (when (and parinfer--line-indents
             (>= pos (caaar parinfer--line-indents)))
    (cdar parinfer--line-indents)))

(defun parinfer--process-closer (indent-x pos ch)
  "Process the closer paren, according to following rules:
0. If closer in lock range:
  ???
1. If closer is incorrect, remove closer;
2. If the closer is after trail position:
  2.1) if the opener's is greater than indent-x, remove closer.
  2.2) else, fix trail position."
  (if parinfer--opener-stack
      (let* ((opener (car parinfer--opener-stack))
             (x (car opener))
             (opener-ch (cdr opener)))
        (if (and parinfer--lock-end
                 (<= parinfer--lock-begin pos parinfer--lock-end))
            (progn (when (>= pos parinfer--trail)
                     (setq parinfer--trail (1+ pos)))
                   (pop parinfer--opener-stack))
          (if (= (parinfer--opener-to-closer opener-ch) ch)
              (if (>= pos parinfer--trail)
                  (if (> indent-x x)
                      (push (cons pos -1) parinfer--op-stack)
                    (progn (pop parinfer--opener-stack)
                           (setq parinfer--trail (1+ pos))))
                (pop parinfer--opener-stack))
            (push (cons pos -1)
                  parinfer--op-stack))))
    (push (cons pos -1)
          parinfer--op-stack)))

(defun parinfer--forward-sexp ()
  (unless (ignore-errors (forward-sexp) t)
    (forward-char)))

(defun parinfer--process-indent-delta ()
  (save-excursion
    (let ((stack ())
          (line-end (line-end-position)))
      (skip-syntax-forward "^(^)" line-end)
      (while (not (= (point) line-end))
        (let ((ch (char-after)))
          (if (parinfer--opener-p ch)
              (push (point) stack)
            (pop stack)))
        (forward-char)
        (skip-syntax-forward "^(^)" line-end))
      (when stack
        (setq parinfer--lock-begin (car stack))
        (while (and (char-after)
                    stack)
          (forward-char)
          (skip-syntax-forward "^(^)")
          (let ((ch (char-after)))
            (if (parinfer--opener-p ch)
                (push (point) stack)
              (pop stack))))
        (setq parinfer--lock-end (point))
        (setq parinfer--lock-line-begin (line-number-at-pos parinfer--lock-begin))
        (setq parinfer--lock-line-count
              (1- (count-lines parinfer--lock-begin parinfer--lock-end)))
        ;; (parinfer--log "[parinfer--process-indent-delta]b: %s e: %s c: %s"
        ;;                parinfer--lock-begin
        ;;                parinfer--lock-end
        ;;                parinfer--lock-line-count)
        ))))

(defun parinfer--apply-indent-delta ()
  (when (and parinfer--lock-line-count
             (not (= (line-beginning-position) (line-end-position))))
    ;; (parinfer--log "[parinfer--apply-indent-delta]from: %s line: %s"
    ;;                parinfer--lock-begin
    ;;                parinfer--lock-line-count)
    (save-excursion
      (goto-line parinfer--lock-line-begin)
      (cl-loop for i from 1 to parinfer--lock-line-count do
               (forward-line)
               (lisp-indent-line)))))

(defun parinfer--process-opener (pos ch)
  (let ((x (- pos (save-excursion (goto-char pos)
                                  (line-beginning-position)))))
    (push (cons x ch)
          parinfer--opener-stack)))

(defun parinfer--process-paren-stack (indent-x)
  (cl-loop for paren in (reverse parinfer--paren-stack) do
           (let ((pos (car paren))
                 (ch (cdr paren)))
             (if (parinfer--opener-p ch)
                 (parinfer--process-opener pos ch)
               (parinfer--process-closer indent-x pos ch))))
  (setq parinfer--paren-stack nil))

(defun parinfer--insert-trail-paren (indent-x)
  (when parinfer--trail
    (unless (and parinfer--lock-end
                 (<= parinfer--lock-begin parinfer--trail parinfer--lock-end))
      (goto-char parinfer--trail)
      (let ((break nil))
        (while (and parinfer--opener-stack (not break))
          (let* ((opener (car parinfer--opener-stack))
                 (ch (cdr opener))
                 (x (car opener)))
            (if (>= x indent-x)
                (progn (push (cons parinfer--trail (parinfer--opener-to-closer ch))
                             parinfer--op-stack)
                       (pop parinfer--opener-stack))
              (setq break t))))))))

(defun parinfer--process-indentation ()
  (when (parinfer--has-indent-p)
    (parinfer--goto-indentation)
    (unless parinfer--empty-line
      (save-excursion
        (let ((indent-x (- (point) (line-beginning-position))))
          (parinfer--process-paren-stack indent-x)
          (parinfer--insert-trail-paren indent-x))))))

(defun parinfer--log-state (ch)
  ;; (parinfer--log "l:%d ch:%s s:%s c:%s t:%s e:%s o:%s"
  ;;                (line-number-at-pos)
  ;;                ch
  ;;                parinfer--in-string
  ;;                parinfer--in-comment
  ;;                parinfer--trail
  ;;                parinfer--empty-line
  ;;                parinfer--opener-stack)
  )

(defun parinfer--process-line ()
  (save-excursion
    (parinfer--process-indentation)
    ;; (parinfer--log-state "ind")
    (let ((line-end (line-end-position)))
      (while (not (= (point) line-end))
        (let ((ch (char-after)))
          (cond
           ((= ch parinfer--backslash)
            (parinfer--on-backslash))
           ((= ch parinfer--doublequote)
            (parinfer--on-doublequote))
           ((= ch parinfer--semicolon)
            (parinfer--on-semicolon))
           ((or (parinfer--opener-p ch)
                (parinfer--closer-p ch))
            (parinfer--on-parens ch)))
          ;; (parinfer--log-state ch)
          )
        (forward-char)
        (parinfer--goto-next-meaningful-position line-end))
      (parinfer--on-end-line)
      ;; (parinfer--log-state "end")
      )))

(defun parinfer--process-buffer ()
  (save-excursion
    (let ((max-line (parinfer--get-buffer-max-line)))
      (goto-char (point-min))
      (cl-loop for i from 0 to max-line do
               (parinfer--process-line)
               (forward-line))
      (goto-char (point-max))
      (when parinfer--in-string
        (error parinfer--error-unclosed-quote))
      (parinfer--process-paren-stack 0)
      (parinfer--insert-trail-paren 0)
      (parinfer--execute-op-stack)
      nil)))

(defun parinfer--process-move ()
  ;; (parinfer--log "[parinfer--process-move]")
  (setq parinfer--processing t)
  (when parinfer--reindent-position
    (save-excursion
      (goto-char parinfer--reindent-position)
      (unless (save-excursion (back-to-indentation) (= (point) (line-end-position)))
        (forward-line)
        (when parinfer--lock-line-count (forward-line parinfer--lock-line-count))
        (let ((end-line-beginning (save-excursion
                                    (goto-char (point-max)) (line-beginning-position))))
          (while (and (< (point) (point-max))
                      (or
                       (save-excursion (back-to-indentation) (= (point) (line-end-position)))
                       (not (= (progn (back-to-indentation) (point))
                               (progn (lisp-indent-line) (point))))))
            ;; (parinfer--log "[parinfer--process-move]indent ln: %s" (line-number-at-pos))
            (forward-line)))))))

(defun parinfer--process-change ()
  ;; (setq parinfer--process-serial (1+ parinfer--process-serial))
  (parinfer--initial-states)
  (when (parinfer--handle-indent-delta-p)
    (parinfer--process-indent-delta))
  (parinfer--process-buffer)
  (when (parinfer--handle-indent-delta-p)
    (parinfer--apply-indent-delta))
  ;; allow reindent
  (setq parinfer--buffer-will-change nil)
  (setq parinfer--reindent-position (point)))

(defun parinfer--post-command-hook ()
  (condition-case ex
      (unless (parinfer--skip-p)
        (if parinfer--buffer-will-change
            (parinfer--process-change)
          (parinfer--process-move)))
    (error
     (let ((error-message (cadr ex)))
       (message error-message))))
  (setq parinfer--processing nil))

(defun parinfer--before-change-hook (start end)
  (unless (or parinfer--processing
              parinfer--buffer-will-change
              (parinfer--skip-p))
    (setq parinfer--prev-x (- (point) (line-beginning-position))
          parinfer--processing t
          parinfer--buffer-will-change t)))

(defun parinfer-mode-enable ()
  (interactive)
  (when (bound-and-true-p paredit-mode)
    (error "warn: parinfer-mode can't work will with paredit-mode."))
  (define-key selected-keymap (kbd "<tab>") 'parinfer-shift-right)
  (define-key selected-keymap (kbd "<backtab>") 'parinfer-shift-left)
  (selected-minor-mode 1)
  (add-hook 'post-command-hook #'parinfer--post-command-hook t t)
  (add-hook 'before-change-functions #'parinfer--before-change-hook t t))

(defun parinfer-mode-disable ()
  (interactive)
  (remove-hook 'post-command-hook #'parinfer--post-command-hook t)
  (remove-hook 'before-change-functions #'parinfer--before-change-hook t))

(defun parinfer-bench ()
  (interactive)
  (parinfer--measure-time
   (parinfer--process-change)))

(defun parinfer-bench2 ()
  (interactive)
  (parinfer--measure-time
   (dotimes (i 1)
     (parinfer--process-indent-delta))))

;; -----------------------------------------------------------------------------
;; COMMANDS
;; -----------------------------------------------------------------------------

(defun parinfer--shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (setq parinfer--mark mark)
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (parinfer--process-change)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))



(defun parinfer-shift-right (count)
  (interactive "p")
  (let ((begin (save-mark-and-excursion
                 (goto-char (region-beginning))
                 (line-number-at-pos)))
        (end (save-mark-and-excursion
               (goto-char (region-end))
               (line-number-at-pos))))
    (parinfer--shift-text 2)
    (goto-line begin)
    (set-mark-command nil)
    (goto-line end)
    (goto-char (line-end-position))
    (setq deactivate-mark nil)))

(defun parinfer-shift-left (count)
  (interactive "p")
  (let ((begin (save-mark-and-excursion
                 (goto-char (region-beginning))
                 (line-number-at-pos)))
        (end (save-mark-and-excursion
               (goto-char (region-end))
               (line-number-at-pos))))
    (parinfer--shift-text -2)
    (goto-line begin)
    (set-mark-command nil)
    (goto-line end)
    (goto-char (line-end-position))
    (setq deactivate-mark nil)))

(defvar parinfer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "}") 'self-insert-command)
    (define-key map (kbd "{") 'self-insert-command)
    (define-key map (kbd "C-)") 'paredit-forward-slurp-sexp)
    (define-key map (kbd "C-(") 'paredit-backward-slurp-sexp)
    (define-key map (kbd "C-}") 'paredit-forward-barf-sexp)
    (define-key map (kbd "C-{") 'paredit-backward-barf-sexp)
    (define-key map (kbd "M-\"") 'paredit-meta-doublequote)
    (define-key map (kbd "M-r") 'paredit-raise-sexp)
    (define-key map (kbd "M-s") 'paredit-splice-sexp)
    (define-key map (kbd "M-S") 'paredit-split-sexp)
    map))

;;;###autoload
(define-minor-mode parinfer-mode
  "Parinfer mode."
  nil "Parinfer" parinfer-mode-map
  (if parinfer-mode
      (parinfer-mode-enable)
    (parinfer-mode-disable)))

(provide 'parinfer-smart)
