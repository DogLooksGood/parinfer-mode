;;; -*- lexical-binding: t; -*-

(require 'selected)
(require 'cl-lib)
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
(defconst parinfer--error-unmatched-close-paren "Unmatched close-paren.")
(defconst parinfer--error-leading-close-paren "Line cannot lead with a close-paren.")

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

(defvar-local parinfer--trail nil
  "The position of the last trail.")

(defvar-local parinfer--empty-line nil
  "This line is empty or will be empty.")

(defvar-local parinfer--paren-stack nil
  "Save the paren of current line.
Element struct: (point . ch)")

(defvar-local parinfer--scope-paren-stack nil)

(defvar-local parinfer--process-indent nil)

(defvar-local parinfer--opener-stack nil
  "Save opener that come from `parinfer--paren-stack'.")

(defvar-local parinfer--op-stack nil
  "Save all we need to do with current buffer.
Element struct: (point . val)
If val > 0, it represent a char to insert;
If val < 0, it represent a deletion length.")

(defvar-local parinfer--last-error nil)

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

(defvar-local parinfer--lock-line-end nil)

(defvar-local parinfer--cursor-line-end nil)

(defvar-local parinfer--lock-paren-stack nil
  "")

(defvar-local parinfer--buffer-will-change nil)

(defvar-local parinfer--scope-end nil)

(defvar-local parinfer--scope-end-line nil
  "The end line of the sexp where the cursor in.")

(defvar-local parinfer--processing nil
  "A flag indicate parinfer mode is processing buffer.")

(defvar-local parinfer--reindent-position nil)

(defvar-local parinfer-mode nil)

(defvar-local parinfer--process-line-begin nil)

(defvar-local parinfer--process-line-end nil)

(defvar-local parinfer--from-comment nil)

;; -----------------------------------------------------------------------------
;; CUSTOMIZE
;; -----------------------------------------------------------------------------

(defcustom parinfer-preview-cursor-scope t
  "Allow temporary leading closer?"
  :type 'boolean
  :group 'parinfer)

(defcustom parinfer-partial-process nil
  "Only process the current top-level sexp?"
  :type 'boolean
  :group 'parinfer)

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
  (and parinfer--buffer-will-change
       (not parinfer--from-comment)
       (not (region-active-p))
       (not (nth 4 (syntax-ppss)))))

(defun parinfer--in-comment-p ()
  (save-excursion
    (when (< (point) (line-end-position)) (forward-char))
    (nth 4 (syntax-ppss))))

(defun parinfer--skip-p ()
  (or
   (and (bound-and-true-p yas-minor-mode)
        (yas-active-snippets))
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

(defun parinfer--delete-command-p ()
  (or (equal this-command 'delete-char)
      (equal this-command 'delete-backward-char)
      (equal this-command 'kill-line)
      (equal this-command 'backward-kill-word)
      (equal this-command 'kill-word)))

(defun parinfer--has-trail-p ()
  "Must call when we make sure we are after trail."
  (not
   (or parinfer--empty-line
       parinfer--in-string)))

(defun parinfer--has-indent-p ()
  "Must call when we make sure we are at the line beginning."
  (not parinfer--in-string))

(defun parinfer--in-edit-p ()
  (and parinfer--edit-begin
       (<= parinfer--edit-begin (point) parinfer--edit-end)))

(defun parinfer--in-lock-p ()
  (and parinfer--lock-begin
       (or (and (not parinfer--lock-end)
                (> (point) parinfer--lock-begin))
           (and parinfer--lock-end
                (<= (point) parinfer--lock-end)
                (> (point) parinfer--lock-begin)))))

;; -----------------------------------------------------------------------------
;; NAVIGATIONS
;; -----------------------------------------------------------------------------

(defun parinfer--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

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
        (in-code nil)
        (indent-pos (save-excursion
                      (back-to-indentation)
                      (point))))
    (while (and (not in-code)
                ;; (not (parinfer--in-edit-p))
                (> (point) indent-pos))
      (let ((ch (char-before)))
        (cond
         ((and (equal ch parinfer--whitespace)
               (not (parinfer--in-edit-p)))
          (when before-paren
            (push (cons (1- (point)) -1) parinfer--op-stack))
          (backward-char))

         ((parinfer--closer-p ch)
          (setq before-paren t)
          (backward-char))

         (t (setq in-code t)))))))

;; ---------

(defun parinfer--remove-line-begin-closer ()
  "Remove the leading closer,
but preserve those in the cursor line if `parinfer--previw-cursor-scope' is t.

If `parinfer--edit-begin' is nil, mean there's no buffer change.
In this case, we just remove all leading closers."
  (if parinfer--edit-begin
      (let ((found nil))
        ;; (parinfer--log "p: %s, e: %s, %s, cl:%s"
        ;;                (point)
        ;;                (< (point) parinfer--edit-end)
        ;;                parinfer--edit-end
        ;;                (> (point) parinfer--cursor-line-end))
        (while (and (parinfer--closer-p (char-after))
                    (or (not parinfer-preview-cursor-scope)
                        (< (point) parinfer--edit-end)
                        (> (point) parinfer--cursor-line-end)
                        ))
          (push (cons (point) -1)
                parinfer--op-stack)
          (forward-char)
          (setq found t))
        found)
    (progn
      (let ((count 0)
            (x nil))
        (while (parinfer--closer-p (char-after))
          (forward-char)
          (setq count (1+ count)))
        (unless (zerop count)
          (save-excursion
            (ignore-errors (backward-sexp)
                           (setq x (- (point) (line-beginning-position)))))
          (when x
            (delete-region (line-beginning-position) (point))
            (lisp-indent-line x)))
        nil))))

(defun parinfer--goto-indentation ()
  "Goto the indentation, and mark all leading closer delete.
If this is a comment only line or empty-line, set `parinfer--empty-line' t."
  (setq parinfer--empty-line nil)
  (back-to-indentation)
  (let ((found (parinfer--remove-line-begin-closer)))
    (let ((end (line-end-position)))
      (while (and (< (point) end)
                  (equal parinfer--whitespace (char-after)))
        (forward-char))
      (if (or (equal (char-after) parinfer--semicolon)
              (= (point) end))
          (setq parinfer--empty-line t)
        (when found
          (parinfer--add-error-overlay
           (line-beginning-position)
           (save-excursion (back-to-indentation) (point)))
          (error parinfer--error-leading-close-paren))))))

;; -----------------------------------------------------------------------------
;; EXECUTORS
;; -----------------------------------------------------------------------------

(defun parinfer--execute-op-1 (op)
  (let ((pos (car op))
        (val (cdr op)))
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

(defun parinfer--handle-indent (pos ch)
  (cond
   ((not parinfer--lock-begin))

   ((not parinfer--process-indent))

   ((parinfer--closer-p ch)
    (when (and (not parinfer--scope-end)
               (> pos (or parinfer--lock-end parinfer--lock-begin)))
      (if parinfer--scope-paren-stack
          (pop parinfer--scope-paren-stack)
        (setq parinfer--scope-end pos))))

   ((< pos parinfer--lock-begin))

   ((and (< pos parinfer--cursor-line-end)
         (or (not parinfer--lock-end)
             ;; Lock will never be line end.
             (< parinfer--lock-end (1- parinfer--cursor-line-end))))
    (save-excursion
      (if (ignore-errors (forward-sexp) t)
          (if (equal (parinfer--closer-to-opener (char-before))
                     ch)
              (setq parinfer--lock-end (1- (point)))
            (setq parinfer--process-indent nil))
        (progn (setq parinfer--process-indent nil)
               (setq parinfer--lock-end nil)))))

   ((and (not parinfer--scope-end)
         (> pos (or parinfer--lock-end parinfer--lock-begin)))
    (push (cons pos ch) parinfer--scope-paren-stack)))


    ;; (parinfer--log "[parinfer--process-indent]%s - b: %s e: %s se: %s"
    ;;              parinfer--process-indent
    ;;              parinfer--lock-begin
    ;;              parinfer--lock-end
    ;;              parinfer--scope-end)
    )

(defun parinfer--initial-states-for-move ()
  (setq parinfer--in-comment nil
        parinfer--in-string nil
        parinfer--quote-danger nil
        parinfer--paren-stack nil
        parinfer--edit-begin nil
        parinfer--scope-paren-stack nil
        parinfer--scope-end-line nil
        parinfer--last-error nil
        parinfer--opener-stack nil
        parinfer--process-indent nil
        parinfer--scope-end nil
        parinfer--trail nil
        parinfer--op-stack nil
        parinfer--reindent-position nil))

(defun parinfer--initial-states ()
  ;; (parinfer--log "[parinfer--initial-states]")
  (let ((in-comment (parinfer--in-comment-p)))
    (setq parinfer--in-comment nil
          parinfer--in-string nil
          parinfer--quote-danger nil
          parinfer--edit-begin (if (or in-comment parinfer--from-comment)
                                   nil
                                 (line-beginning-position))
          parinfer--edit-end (if (region-active-p)
                                 (line-beginning-position)
                               (point))
          parinfer--lock-begin (if parinfer--from-comment nil (point))
          parinfer--lock-end nil
          parinfer--lock-line-end nil
          parinfer--lock-line-begin (line-number-at-pos parinfer--lock-begin)
          parinfer--cursor-line-end (line-end-position)
          parinfer--paren-stack nil
          parinfer--scope-paren-stack nil
          parinfer--scope-end-line nil
          parinfer--last-error nil
          parinfer--opener-stack nil
          parinfer--process-indent t
          parinfer--scope-end nil
          parinfer--trail nil
          parinfer--op-stack nil
          parinfer--reindent-position nil))
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
    (if parinfer--in-string
        (setq parinfer--in-string nil)
      (setq parinfer--in-string (point)))))

(defun parinfer--on-semicolon ()
  (unless (or parinfer--in-string
              parinfer--in-comment)
    (setq parinfer--in-comment (point))
    (parinfer--process-code-end)))

(defun parinfer--on-backslash ()
  (forward-char)
  (when (= (point) (line-end-position))
    (parinfer--add-error-overlay (1- (point)))
    (error parinfer--error-eol-backslash)))

(defun parinfer--on-parens (ch)
  (unless (or parinfer--in-comment
              parinfer--in-string)
    (push (cons (point) ch)
          parinfer--paren-stack)
    (parinfer--handle-indent (point) ch)))

(defun parinfer--on-end-line ()
  (if parinfer--in-comment
      (if parinfer--quote-danger
          (progn
            (parinfer--add-error-overlay parinfer--in-comment (point))
            (error parinfer--error-quote-danger))
        (setq parinfer--in-comment nil))
    (unless parinfer--in-string
      (parinfer--process-code-end))))

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
                   (if (equal (parinfer--opener-to-closer opener-ch)
                              ch)
                       (pop parinfer--opener-stack)
                     (progn
                       (parinfer--add-error-overlay pos)
                       (parinfer--add-error-overlay-to-opener pos)
                       (error parinfer--error-unmatched-close-paren))))
          (if (= (parinfer--opener-to-closer opener-ch) ch)
              (if (>= pos parinfer--trail)
                  (if (and (> indent-x x)
                           (not (and parinfer--buffer-will-change
                                     parinfer--edit-begin
                                     parinfer--edit-end
                                     (< parinfer--edit-begin pos parinfer--edit-end))))
                      (push (cons pos -1) parinfer--op-stack)
                    (progn (pop parinfer--opener-stack)
                           (setq parinfer--trail (1+ pos))))
                (pop parinfer--opener-stack))
            (if (>= pos parinfer--trail)
                (push (cons pos -1)
                      parinfer--op-stack)
              (progn
                (parinfer--add-error-overlay pos)
                (parinfer--add-error-overlay-to-opener pos)
                (error parinfer--error-unmatched-close-paren))))))
    (push (cons pos -1)
          parinfer--op-stack)))

(defun parinfer--forward-sexp ()
  (unless (ignore-errors (forward-sexp) t)
    (forward-char)))

(defun parinfer--apply-indent-delta ()
  (when (and parinfer--lock-line-end
             parinfer--process-indent
             (not (= (line-beginning-position) (line-end-position))))
    ;; (parinfer--log "[parinfer--apply-indent-delta]from: %s line: %s"
    ;;                parinfer--lock-line-begin
    ;;                parinfer--lock-line-count)
    (save-excursion
      (parinfer--goto-line parinfer--lock-line-begin)
      (cl-loop for i from (1+ parinfer--lock-line-begin) to parinfer--lock-line-end do
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
    (unless (and parinfer--lock-begin
                 parinfer--lock-end
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
  (parinfer--log "l:%d ch:%s s:%s c:%s t:%s e:%s o:%s"
                 (line-number-at-pos)
                 ch
                 parinfer--in-string
                 parinfer--in-comment
                 parinfer--trail
                 parinfer--empty-line
                 parinfer--opener-stack))

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

(defun parinfer--process-buffer-with-perf-hack ()
  (save-excursion
    (parinfer--get-process-range)
    (goto-char (point-min))
    (forward-line (1- parinfer--process-line-begin))
    (cl-loop for i from parinfer--process-line-begin
             to parinfer--process-line-end do
             (parinfer--process-line)
             (forward-line))
    (goto-char (point-min))
    (forward-line (1- parinfer--process-line-end))
    (goto-char (line-end-position))
    (when parinfer--in-string
      (parinfer--add-error-overlay parinfer--in-string (point))
      (error parinfer--error-unclosed-quote))
    (parinfer--process-paren-stack 0)
    (parinfer--insert-trail-paren 0)
    (when parinfer--lock-end
        (setq parinfer--lock-line-end (line-number-at-pos parinfer--lock-end)))
      (when parinfer--scope-end
        (setq parinfer--scope-end-line (line-number-at-pos parinfer--scope-end)))
    (parinfer--execute-op-stack)))

(defun parinfer--process-buffer ()
  (save-excursion
    (let ((max-line (parinfer--get-buffer-max-line)))
      (goto-char (point-min))
      (cl-loop for i from 0 to max-line do
               (parinfer--process-line)
               (forward-line))
      (goto-char (point-max))
      (when parinfer--in-string
        (parinfer--add-error-overlay parinfer--in-string (point))
        (error parinfer--error-unclosed-quote))
      (parinfer--process-paren-stack 0)
      (parinfer--insert-trail-paren 0)
      ;; set lock-end scope-end
      (when parinfer--lock-end
        (setq parinfer--lock-line-end (line-number-at-pos parinfer--lock-end)))
      (when parinfer--scope-end
        (setq parinfer--scope-end-line (line-number-at-pos parinfer--scope-end)))
      (parinfer--execute-op-stack)
      nil)))

(defun parinfer--process-move ()
  ;; (parinfer--log "[parinfer--process-move]")
  (setq parinfer--processing t)
  (when (and parinfer--reindent-position
             parinfer--edit-begin
             (not (<= parinfer--edit-begin (point) parinfer--edit-end)))
    (save-excursion
      (goto-char parinfer--reindent-position)
      (unless (save-excursion (back-to-indentation) (= (point) (line-end-position)))
        ;; the first line need to reindent too.
        ;; lisp-indent-line will always indent to the correct indentation,
        ;; according to the outer sexp, not previous line.
        (if parinfer--scope-end-line
          (cl-loop for i from parinfer--lock-line-begin to parinfer--scope-end-line do
                   (lisp-indent-line)
                   (forward-line))
          ;; (lisp-indent-line)
          )))
    ;; (parinfer--process-change)
    (parinfer--remove-error-overlay)
    (parinfer--initial-states-for-move)
    (if parinfer-partial-process
        (parinfer--process-buffer-with-perf-hack)
      (parinfer--process-buffer))
    (setq parinfer--reindent-position nil)))

(defun parinfer--process-change ()
  ;; (setq parinfer--process-serial (1+ parinfer--process-serial))
  (parinfer--remove-error-overlay)
  (parinfer--initial-states)
  (let ((handle-indent-delta (parinfer--handle-indent-delta-p)))
    (if parinfer-partial-process
        (parinfer--process-buffer-with-perf-hack)
      (parinfer--process-buffer))
    (when handle-indent-delta
      (parinfer--apply-indent-delta))
    ;; allow reindent
    (setq parinfer--buffer-will-change nil)
    (setq parinfer--reindent-position (point))
    (unless parinfer--scope-end
      (setq parinfer--scope-end parinfer--edit-end
            parinfer--scope-end-line (line-number-at-pos)))))

;; -----------------------------------------------------------------------------
;; UNDO
;; -----------------------------------------------------------------------------

(defun parinfer--handle-special-command ()
  (when (or (equal this-command 'undo)
            (equal this-command 'undo-tree-undo)
            (equal this-command 'undo-tree-redo)
            (equal this-command 'yank)
            (equal this-command 'yank-pop))
    (setq parinfer--reindent-position (point)
          parinfer--edit-begin (line-beginning-position)
          parinfer--edit-end (point))))


;; -----------------------------------------------------------------------------
;; HOOK FUNCTIONS
;; -----------------------------------------------------------------------------


(defun parinfer--post-command-hook ()
  (parinfer--handle-special-command)
  (condition-case ex
      (unless (parinfer--skip-p)
        (if parinfer--buffer-will-change
            (parinfer--process-change)
          (parinfer--process-move)))
    (error
     (let ((error-message (cadr ex)))
       (unless (equal parinfer--last-error error-message)
         (message error-message)
         (setq parinfer--last-error error-message)))))
  (setq parinfer--processing nil))

(defun parinfer--before-change-hook (_start _end)
  (unless (or parinfer--processing
              parinfer--buffer-will-change
              (parinfer--skip-p))
    (setq parinfer--processing t
          parinfer--buffer-will-change t
          parinfer--from-comment (parinfer--in-comment-p))))

(defun parinfer--before-save-hook ()
  (when parinfer--reindent-position
    (save-excursion
      (goto-char parinfer--reindent-position)
      (unless (save-excursion (back-to-indentation) (= (point) (line-end-position)))
        (if parinfer--scope-end-line
          (cl-loop for i from parinfer--lock-line-begin to parinfer--scope-end-line do
                   (lisp-indent-line)
                   (forward-line))
          (lisp-indent-line))))
    (parinfer--remove-error-overlay)
    (parinfer--initial-states-for-move)
    (if parinfer-partial-process
        (parinfer--process-buffer-with-perf-hack)
      (parinfer--process-buffer))
    (setq parinfer--reindent-position nil)))

;; -----------------------------------------------------------------------------
;; TOGGLE MODES
;; -----------------------------------------------------------------------------

(defun parinfer-mode-enable ()
  (interactive)
  (if-let* ((msg (parinfer--lint)))
      (progn (message msg)
             (setq parinfer-mode nil))
    (progn
      (parinfer--enable-dim-paren)
      (define-key selected-keymap (kbd "<tab>") 'parinfer-shift-right)
      (define-key selected-keymap (kbd "<backtab>") 'parinfer-shift-left)
      (selected-minor-mode 1)
      (add-hook 'post-command-hook #'parinfer--post-command-hook nil t)
      (add-hook 'before-change-functions #'parinfer--before-change-hook t t)
      (add-hook 'before-save-hook #'parinfer--before-save-hook t t))))

(defun parinfer-mode-disable ()
  (interactive)
  (parinfer--disable-dim-paren)
  (remove-hook 'post-command-hook #'parinfer--post-command-hook t)
  (remove-hook 'before-change-functions #'parinfer--before-change-hook t)
  (remove-hook 'before-save-hook #'parinfer--before-save-hook t))

(defun parinfer-bench ()
  (interactive)
  (parinfer--measure-time
   (parinfer--process-change)))

;; -----------------------------------------------------------------------------
;; Dim parens
;; -----------------------------------------------------------------------------

(defun parinfer--dim-paren-search (limit)
  (let ((result nil)
        (finish nil)
        (bound (+ (point) limit)))
    (while (not finish)
      (if (re-search-forward "\\s)" bound t)
          (when (and (= 0 (string-match-p "\\s)* *\\(?:;.*\\)?$"
                                          (buffer-substring-no-properties (point) (line-end-position))))
                     (not (eq (char-before (1- (point))) parinfer--backslash)))
            (setq result (match-data)
                  finish t))
        (setq finish t)))
    result))

(defface parinfer--dim-face
   '((((class color) (background dark))
      (:foreground "grey40"))
     (((class color) (background light))
      (:foreground "grey60")))
   "Parinfer dim paren face."
   :group 'parinfer)

(defun parinfer--enable-dim-paren ()
  (font-lock-add-keywords nil '((parinfer--dim-paren-search . 'parinfer--dim-face))))

(defun parinfer--disable-dim-paren ()
  (font-lock-remove-keywords nil '((parinfer--dim-paren-search . 'parinfer--dim-face))))

;; -----------------------------------------------------------------------------
;; Overlay
;; -----------------------------------------------------------------------------

(defface parinfer--error-face
  '((((supports :underline (:style wave)))
     (:underline (:style wave :color "red")
                 :foreground "red")))
  "Face for parinfer error."
  :group 'parinfer)

(defun parinfer--remove-error-overlay ()
  (remove-overlays (point-min) (point-max) 'parinfer--error-overlay))

(defun parinfer--add-error-overlay (begin &optional end)
  (let ((end (or end (1+ begin))))
    (let ((overlay (make-overlay begin end)))
      (overlay-put overlay 'name 'parinfer--error-overlay)
      (overlay-put overlay 'face 'parinfer--error-face))))

(defun parinfer--add-error-overlay-to-opener (pos)
  (save-excursion
    (ignore-errors
      (goto-char pos)
      (forward-char)
      (backward-sexp)
      (parinfer--add-error-overlay (point)))))

;; -----------------------------------------------------------------------------
;; Linter
;; -----------------------------------------------------------------------------

(defun parinfer--lint ()
  "Called before enable parinfer.
Check the followings:
1. If there're indentation with \t;
2. If the buffer will change;
3. If process have error;
4. If paredit is enabled."
  (unless (string-prefix-p "*temp*" (string-trim (buffer-name)))
    (let ((err nil))
      (when (save-excursion (goto-char (point-min))
                            (search-forward "\t" (point-max) t))
        (setq err "Can't enable parinfer due to inconsistent indentation."))

      (when (bound-and-true-p paredit-mode)
        (setq err "Can't enable parinfer when paredit-mode/smartparens-mode is enabled."))

      (let ((buffer-text (buffer-substring-no-properties (point-min) (point-max)))
            (mm major-mode))
        (with-temp-buffer
          (insert buffer-text)
          (funcall mm)
          (parinfer--initial-states)
          (condition-case ex
              (parinfer--process-buffer)
            (error
             (setq err (concat "Can't enable parinfer due to error: " (cadr ex)))))
          (unless (string-equal (buffer-substring-no-properties (point-min) (point-max))
                                buffer-text)
            (setq err "Can't enable parinfer due to buffer will be changed."))))
      err)))

;; -----------------------------------------------------------------------------
;; PERF HACK
;; -----------------------------------------------------------------------------

(defun parinfer--def-p ()
  (not (or (equal (char-after) parinfer--whitespace)
           (= (line-beginning-position) (line-end-position))
           (equal (char-after) parinfer--semicolon)
           (or (nth 3 (syntax-ppss))))))

;; abount 5ms
;; (parinfer--measure-time
;;  (dotimes (i 100)
;;    (parinfer--non-string-or-comment-p)))

(defun parinfer--get-process-range ()
  "Process from last top-level def(begin of buffer if not found) to next top-level def(end of buffer if not found).
return (begin . end)."
  (save-excursion
    (let ((begin nil)
          (end nil))
      (goto-char (line-beginning-position))
      (let ((orig (point)))
        (when (not (= (point) (point-min)))
          (forward-line -1))
        (while (and (not (parinfer--def-p))
                    (< (point-min) (point)))
          (forward-line -1))
        (setq begin (line-number-at-pos))
        (goto-char orig)
        (forward-line 1)
        (while (and (not (parinfer--def-p))
                    (< (point) (point-max)))
          (forward-line 1))
        (if (= (point) (point-max))
            (setq end (line-number-at-pos))
          (setq end (1- (line-number-at-pos)))))
      (setq parinfer--process-line-begin begin
            parinfer--process-line-end end))))

;; -----------------------------------------------------------------------------
;; COMMANDS
;; -----------------------------------------------------------------------------

(defun parinfer--shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
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
               (when (= (point) (line-beginning-position))
                 (backward-char))
               (line-number-at-pos))))
    (parinfer--shift-text count)
    (parinfer--goto-line begin)
    (set-mark-command nil)
    (parinfer--goto-line end)
    (goto-char (line-end-position))
    (setq deactivate-mark nil)))

(defun parinfer-shift-left (count)
  (interactive "p")
  (let ((begin (save-mark-and-excursion
                 (goto-char (region-beginning))
                 (line-number-at-pos)))
        (end (save-mark-and-excursion
               (goto-char (region-end))
               (when (= (point) (line-beginning-position))
                 (backward-char))
               (line-number-at-pos))))
    (parinfer--shift-text (- count))
    (parinfer--goto-line begin)
    (set-mark-command nil)
    (parinfer--goto-line end)
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
  nil " Parinfer" parinfer-mode-map
  (if parinfer-mode
      (parinfer-mode-enable)
    (parinfer-mode-disable)))

(provide 'parinfer-smart)
