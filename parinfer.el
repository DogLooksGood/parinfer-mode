;;; parinfer.el --- Simpler Lisp editing

;; Copyright (c) 2016, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/parinfer-mode
;; Version: 0.2.0
;; Package-Requires: ((dash "2.13.0") (aggressive-indent "1.8.1") (cl-lib "0.5"))
;; Keywords: Parinfer

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Installation

;; * Clone this repo.
;; #+BEGIN_SRC shell
;;   cd /some/path/parinfer-mode
;;   git clone https://github.com/DogLooksGood/parinfer-mode.git
;; #+END_SRC
;; * Emacs configurations.
;; #+BEGIN_SRC emacs-lisp
;;   ;; Add parinfer-mode to load-path.
;;   (add-to-list 'load-path "~/some/path/parinfer-mode")
;;   ;; Require it!
;;   (require 'parinfer)
;; #+END_SRC

;;; Commentary:
;; For now, project will be updated frequently.
;; The document is hosted on github.
;; https://github.com/DogLooksGood/parinfer-mode

;; * Credits
;; - [[https://github.com/shaunlebron][shaunlebron]] :: Create Parinfer.
;; - [[https://github.com/oakmac][oakmac]] :: Bring Parinfer to Emacs.
;; - [[https://github.com/tumashu][tumashu]] :: Help me a lot in writing this plugin.
;; - [[https://github.com/purcell][purcell]] & [[https://github.com/syohex][syohex]] :: Advice and Tips for writing Emacs plugin.
;; * License
;; Licensed under the GPLv3.

;;; Code:

;; -----------------------------------------------------------------------------
;; Requires
;; -----------------------------------------------------------------------------
(require 'parinferlib)
(require 'parinfer-theme)
(require 'dash)
(require 'aggressive-indent)
(require 'mode-local)
(require 'cl-lib)
(require 'ediff)

;; -----------------------------------------------------------------------------
;; Constants & Variables
;; -----------------------------------------------------------------------------
(defconst parinfer--defun-regex "^[^ \n\t\";]"
  "Regex for finding the beginning of S-exp.")

(defvar parinfer--mode 'paren
  "Parinfer mode style, 'paren or 'indent.")
(make-variable-buffer-local 'parinfer--mode)

(defvar parinfer--first-load t
  "NOT MODIFY THIS, If haven't switch to indent mode yet.")
(make-variable-buffer-local 'parinfer--first-load)

(defvar parinfer--region-shifted nil
  "If shift the region after mark activate.")
(make-variable-buffer-local 'parinfer--region-shifted)

(defvar parinfer--text-modified nil
  "If last command modified text.")
(make-variable-buffer-local 'parinfer--text-modified)

(defvar parinfer-lighters
  '(" Parinfer:Indent" . " Parinfer:Paren")
  "Parinfer lighters in mode line.

The car of it is used in parinfer indent mode, the cdr
used in parinfer paren mode.")

(defvar parinfer-mode-enable-hook nil
  "Call after parinfer mode is enabled.")

(defvar parinfer-mode-disable-hook nil
  "Call after parinfer mode is disabled.")

(defvar parinfer-switch-mode-hook nil
  "Call after parinfer mode switch between Indent Mode & Paren Mode.

One argument for hook function, MODE present for the mode will be used.")

(defvar parinfer--last-line-number -1
  "Holds the last line number after invoke-parinfer-when-necessary.")
(make-variable-buffer-local 'parinfer--last-line-number)

(defvar parinfer-preview-cursor-scope nil
  "In Indent Mode, if show the cursor's scope on an empty line by inserting close-parens after it.")

(defvar parinfer-indent-mode-dim-close-parens t
  "Dimming close parens in Indent Mode.")

(defvar parinfer-delay-invoke-threshold 6000
  "Threshold for 'parinfer-mode' delay processing.")

(defvar parinfer-delay-invoke-idle 0.3
  "The delay time(seconds) for parinfer delay processing.")

(defvar parinfer--delay-timer nil
  "Current delay timer.")
(make-variable-buffer-local 'parinfer--delay-timer)

(defvar parinfer-strategy
  '((default
     evil-delete-char self-insert-command delete-indentation kill-line
     comment-dwim kill-word delete-char newline kill-region)
    (instantly
     evil-delete evil-change evil-change-line evil-paste-before evil-paste-after
     evil-delete-line evil-delete-char evil-delete-backward-char evil-substitute
     evil-change-whole-line evil-force-normal-state evil-normal-state
     evil-shift-left evil-shift-right delete-region)
    (skip
     evil-previous-line evil-forward-char evil-backward-char evil-next-line
     evil-forward-word evil-forward-word-begin evil-backward-word-begin evil-backward-end
     evil-scroll-page-down evil-scroll-up))
  "Parinfer invoke strategy.)

This variable is an association list, user can use `parinfer-strategy-parse'
to parse it and use `parinfer-strategy-add' to set it.

Its elements is like below:

 (STRATEGY-NAME COMMAND COMMAND1 ...  CMDS-REGEXP CMDS-REGEXP1 ...)

The COMMAND is a symbol and CMDS-REGEXP is a regexp string which
used to match command.

 strategy name    Description
 --------------   -------------------------------------------
 default          Invoke parinfer (delay on large sexp)
 instantly        Invoke parinfer instantly
 disable          Do not invoke parinfer")

;; -----------------------------------------------------------------------------
;; Macros
;; -----------------------------------------------------------------------------

(defmacro parinfer-silent (&rest body)
  "Local set function `message' to `format'."
  `(cl-letf (((symbol-function 'message) #'format))
     ,@body))

(defmacro parinfer-paren-run (&rest body)
  "Run BODY in paren mode.  Keep S-sexp in correct indentation."
  (let ((toggle (make-symbol "toggle")))
    `(let ((,toggle (eq parinfer--mode 'indent)))
       (parinfer-silent
         (when ,toggle
            (parinfer--switch-to-paren-mode)
          ,@body
          (when ,toggle
            (parinfer--reindent-sexp)
            (parinfer--indent-and-switch-to-indent-mode)))))))

(defmacro parinfer-run (&rest body)
  "Run BODY, then invode parinfer(depend on current parinfermode) immediately."
  `(progn
     ,@body
     (setq parinfer--text-modified t)
     (parinfer--invoke-parinfer)))

(defmacro parinfer--switch-to (mode &rest body)
  "Macro which used to switch indent/paren mode."
  (let ((m (make-symbol "mode")))
    `(let ((,m ,mode))
       ,@body
       (run-hook-with-args 'parinfer-switch-mode-hook ,m)
       (when (bound-and-true-p company-mode)
         (add-hook 'company-completion-cancelled-hook 'parinfer--company-cancel t t)
         (remove-hook 'company-completion-finished-hook 'parinfer--company-finish t))
       (when parinfer-indent-mode-dim-close-parens
         (parinfer--set-rainbow-delimiters ,m)
         (parinfer--set-dim-parens ,m))
       (force-mode-line-update))))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun parinfer-strategy-parse (strategy-name)
  "Parse strategy, which is named STRATEGY-NAME in `parinfer-strategy'.

Its output is like the below:

  (:commands (cmd1 cmd2 cmd3)
   :regexps (regexp1 regexp2 regexp3))"
  (let ((list (cdr (assq strategy-name parinfer-strategy))))
    `(:commands ,(cl-remove-if #'stringp list)
      :regexps  ,(cl-remove-if #'symbolp list))))

(defun parinfer-strategy-add (strategy-name commands)
  "Set COMMANDS to use parinfer strategy, which is named STRATEGY-NAME.
The results will save to `parinfer-strategy'.

COMMANDS can be:

1. A command (symbol)
2. A commands list (symbol list)
3. A regexp which is used to match commands
4. A list of regexps which are used to match commands"
  (let* ((commands (if (listp commands)
                       commands
                     (list commands)))
         (orig-value (cdr (assq strategy-name parinfer-strategy)))
         (new-value (cl-remove-duplicates
                     (append orig-value commands)
                     :test #'equal)))
    (push (cons strategy-name new-value)
          parinfer-strategy)))

(defun parinfer--set-text-modified ()
  "Set ‘parinfer--text-modified’ to t."
  (when (and (symbolp this-command)
             (-contains-p
              (plist-get (parinfer-strategy-parse 'default) :commands)
              this-command))
    (setq parinfer--text-modified t)))

(defun parinfer--unset-text-modified ()
  "Set ‘parinfer--text-modified’ to nil."
  (setq parinfer--text-modified nil))

(defun parinfer--set-rainbow-delimiters (mode)
  "Set rainbow delimiters depend MODE."
  (cl-case mode
    (indent (when (bound-and-true-p rainbow-delimiters-mode)
              (rainbow-delimiters-mode-disable)))
    (paren (when (fboundp 'rainbow-delimiters-mode)
             (rainbow-delimiters-mode-enable)))))

(defun parinfer--switch-to-indent-mode-1 ()
  "Swith to indent mode auxiliary function."
  (parinfer--switch-to
   'indent
   (setq parinfer--mode 'indent)
   (setq parinfer--first-load nil)
   (message "Parinfer: Indent Mode")))

(defun parinfer--switch-to-indent-mode ()
  "Switch to Indent Mode, this will apply indent fix on whole buffer.)
If this is the first switching for current buffer and indent mode will change
Buffer text, we should see a confirm message."
  (if (not parinfer--first-load)
      (progn
        (parinfer-indent-buffer)
        (parinfer--switch-to-indent-mode-1))
    (when (parinfer-indent-with-confirm)
      (parinfer--switch-to-indent-mode-1))))

(defun parinfer--indent-and-switch-to-indent-mode ()
  "Switch to Indent mode and call Indent Mode immediately."
  (interactive)
  (parinfer--switch-to-indent-mode-1)
  (parinfer--invoke-parinfer-when-necessary))

(defun parinfer--company-cancel (&ignored)
  "Invoke when company cancelled, ignore IGNORED."
  (parinfer-indent))

(defun parinfer--company-finish (&ignored)
  "Invoke when company finished, ignore IGNORED. "
  (parinfer--reindent-sexp))

(defun parinfer--switch-to-paren-mode ()
  "Switch to paren mode."
  (parinfer--switch-to
   'paren
   (when parinfer--delay-timer
     (parinfer--clean-up))
   (setq parinfer--mode 'paren)
   (message "Parinfer: Paren Mode")))

(defun parinfer--in-comment-or-string-p ()
  "Return if we are in comment or string."
  (let ((f (get-text-property (point) 'face)))
    (or (nth 3 (syntax-ppss))
        (nth 4 (syntax-ppss))
        (eq f 'font-lock-comment-face)
        (eq f 'font-lock-comment-delimiter-face))))

(defun parinfer--goto-line (n)
  "Goto the beginning of line N."
  (goto-char (point-min))
  (forward-line (1- n))
  (beginning-of-line))

(defun parinfer--goto-next-defun ()
  "Goto next defun, skip comment or string."
  (interactive)
  (let ((pt (point)))
    (when (not (eq (point-max) pt))
      (if (search-forward-regexp parinfer--defun-regex nil t)
          (if (= (1+ pt) (point))
              (parinfer--goto-next-defun)
            (let ((last-pos (point)))
              (while (and last-pos
                          (parinfer--in-comment-or-string-p)
                          (not (eq (point-max) (point))))

                (search-forward-regexp parinfer--defun-regex nil t)
                (if (eq (point) last-pos)
                    (setq last-pos nil)
                  (setq last-pos (point)))))
            (backward-char))
        (goto-char (point-max))))))

(defun parinfer--goto-previous-defun-1 ()
  "Goto previous defun auxiliary function."
  (search-backward-regexp parinfer--defun-regex nil t)
  (let ((last-pos (point)))
    (while (and last-pos
                (parinfer--in-comment-or-string-p)
                (not (eq (point-min) (point))))
      (search-backward-regexp parinfer--defun-regex nil t)
      (if (eq (point) last-pos)
          (setq last-pos nil)
        (setq last-pos (point))))))

(defun parinfer--goto-previous-defun ()
  "Goto previous defun, skip comment or string."
  (interactive)
  (parinfer--goto-previous-defun-1)
  (parinfer--goto-previous-defun-1))

(defun parinfer--lighter ()
  "Return the lighter for specify mode."
  (if (eq 'paren parinfer--mode)
      (cdr parinfer-lighters)
    (car parinfer-lighters)))

(defun parinfer--ediff-init-keys ()
  "Inits for ediff.  since we don't need all features, simplify opeators."
  (local-set-key (kbd "q") 'parinfer-ediff-quit))

(defun parinfer--cursor-x ()
  "Get the cursor-x which is need by parinferlib computation."
  (length
   (buffer-substring-no-properties
    (line-beginning-position)
    (point))))

(defun parinfer--reindent-sexp ()
  "Call aggressive-indent."
  (when (not (parinfer--in-comment-or-string-p))
    (aggressive-indent-indent-defun)))

(defun parinfer--invoke-parinfer-instantly (&optional pos)
  "Call Parinfer at POS immediately."
  (if (and pos (not (eq pos (point))))
      (let ((ln (line-number-at-pos))
            (x (length
                (buffer-substring-no-properties (line-beginning-position)
                                                (point)))))
        (goto-char pos)
        (parinfer--invoke-parinfer-instantly)
        (parinfer--goto-line ln)
        (forward-char x))
     (parinfer-silent
      (cond
       ((eq 'paren parinfer--mode) (parinfer-paren))
       ((eq 'indent parinfer--mode) (parinfer-indent-instantly))
       (t "nothing")))))

(defun parinfer--invoke-parinfer (&optional pos)
  "Supposed to be called after each content change.)
POS is the position we want to call parinfer."
  (if (and pos (not (eq pos (point))))
      (let ((current-pos (point)))
        (goto-char pos)
        (parinfer--invoke-parinfer)
        (goto-char current-pos))
    (parinfer-silent
      (cond
       ((eq 'paren parinfer--mode) (parinfer-paren))
       ((eq 'indent parinfer--mode) (parinfer-indent))
       (t "nothing")))))

(defun parinfer--should-disable-p ()
  "Should parinfer disabled at this moment."
  (or (and (bound-and-true-p multiple-cursors-mode)
           (region-active-p))
      (and (symbolp this-command)
           (eq this-command 'yank))))

(defun parinfer--strategy-match-p (command strategy-name)
  (let* ((output (parinfer-strategy-parse strategy-name))
         (cmds (plist-get output :commands))
         (regexps (plist-get output :regexps)))
    (if (member command cmds)
        t
      (-any-p (lambda (regexp)
                (string-match-p
                 regexp (symbol-name command)))
              regexps))))

(defun parinfer--should-skip-this-command-p ()
  "Should parinfer skip this command."
  (parinfer--strategy-match-p this-command 'skip))

(defun parinfer--should-invoke-instantly-p ()
  "Should parinfer be invoked instantly."
  (parinfer--strategy-match-p this-command 'instantly))

(defun parinfer--should-invoke-p ()
  (parinfer--strategy-match-p this-command 'default))

(defun parinfer--should-clean-up-p ()
  (and (eq 'indent parinfer--mode)
       parinfer--text-modified
       (not (equal parinfer--last-line-number (line-number-at-pos)))))

(defun parinfer--clean-up ()
  (when parinfer--delay-timer
    (cancel-timer parinfer--delay-timer)
    (setq parinfer--delay-timer nil))
  (parinfer--invoke-parinfer-instantly
       (save-excursion
         (parinfer--goto-line parinfer--last-line-number)
         (line-beginning-position))))

(defun parinfer--invoke-parinfer-when-necessary ()
  "Invoke parinfer when necessary."
  (when this-command
    (cond
     ((not (symbolp this-command))
      nil)

     ((parinfer--should-disable-p) nil)

     ((parinfer--should-clean-up-p)
      (parinfer--clean-up))

     ((parinfer--in-comment-or-string-p) nil)

     ((parinfer--should-skip-this-command-p) nil)

     ((parinfer--should-invoke-instantly-p)
      (parinfer--invoke-parinfer-instantly (point)))

     ((parinfer--should-invoke-p)
      (parinfer--invoke-parinfer (point)))

     (t nil)))
  (setq parinfer--last-line-number (line-number-at-pos (point))))

(defun parinfer--active-line-region ()
  "Auto adjust region so that the shift can work properly."
  (let* ((begin (region-beginning))
         (end (region-end))
         (new-begin (save-excursion (goto-char begin) (line-beginning-position))))
    (goto-char new-begin)
    (set-mark-command nil)
    (goto-char end)
    (setq deactivate-mark nil)
    (setq parinfer--region-shifted t)))

(defun parinfer--shift (distance)
  "Shift text.  For right, DISTANCE > 0; left, DISTANCE < 0."
  (when (use-region-p)
    (when (not parinfer--region-shifted)
      (parinfer--active-line-region))
    (let ((mark (mark)))
      (save-excursion
        (indent-rigidly (region-beginning)
                        (region-end)
                        distance)
        (push-mark mark t t)
        (setq deactivate-mark nil)))))

(defun parinfer-mode-enable ()
  "Enable 'parinfer-mode'."
  ;; Always use whitespace for indentation.
  (setq-mode-local parinfer-mode indent-tabs-mode nil)
  (setq parinfer--last-line-number (line-number-at-pos (point)))
  (run-hooks 'parinfer-mode-enable-hook)
  (add-hook 'post-command-hook 'parinfer--invoke-parinfer-when-necessary t t)
  (add-hook 'post-command-hook 'parinfer--set-text-modified t t)
  (add-hook 'activate-mark-hook 'parinfer--regin-mode-enable t t)
  (add-hook 'deactivate-mark-hook 'parinfer--region-mode-disable t t)
  (parinfer--switch-to-indent-mode))

(defun parinfer-mode-disable ()
  "Disable 'parinfer-mode'."
  (run-hooks 'parinfer-mode-disable-hook)
  (remove-hook 'activate-mark-hook 'parinfer--regin-mode-enable t)
  (remove-hook 'post-command-hook 'parinfer--set-text-modified t)
  (remove-hook 'deactivate-mark-hook 'parinfer--region-mode-disable t)
  (remove-hook 'post-command-hook 'parinfer--invoke-parinfer-when-necessary t)
  (parinfer--region-mode-disable))

(defun parinfer--regin-mode-enable ()
  "Run when region activated."
  (parinfer-region-mode 1))

(defun parinfer--region-mode-disable ()
  "Run when region deactivated, indent code if ‘parinfer--mode’ is 'indent."
  (when (and (eq 'indent parinfer--mode)
             parinfer--region-shifted)
    (parinfer-indent-buffer)
    (when (not (ignore-errors (parinfer--reindent-sexp)))
      (parinfer-indent-buffer))
    (setq parinfer--region-shifted nil))
  (parinfer-region-mode -1))

(defun parinfer--prepare ()
  "Prepare input arguments for parinferlib."
  (let* ((window-start-pos (window-start))
         (start (save-excursion (parinfer--goto-previous-defun) (point)))
         (end (save-excursion (parinfer--goto-next-defun) (point)))
         (text (buffer-substring-no-properties start end))
         (line-number (line-number-at-pos))
         (cursor-line (- line-number (line-number-at-pos start)))
         (cursor-x (parinfer--cursor-x))
         (opts (list :cursor-x cursor-x :cursor-line cursor-line :preview-cursor-scope parinfer-preview-cursor-scope))
         (orig (list :start start :end end :window-start-pos window-start-pos :line-number line-number)))
    (list :text text :opts opts :orig orig)))

(defun parinfer--apply-result (result context)
  "Apply parinfer RESULT to current buffer.
CONTEXT is the context for parinfer execution."
  (let* ((orig (plist-get context :orig))
         (start (plist-get orig :start))
         (end (plist-get orig :end))
         (window-start-pos (plist-get orig :window-start-pos))
         (line-number (plist-get orig :line-number)))
    (when (and (plist-get result :success)
               (plist-get result :changed-lines))
        (delete-region start end)
        (insert (plist-get result :text))
        (parinfer--goto-line line-number)
        (forward-char (plist-get result :cursor-x))
        (set-window-start (selected-window) window-start-pos))
    (parinfer--unset-text-modified)))

(defun parinfer--execute-instantly (context)
  "Execute parinfer instantly with context CONTEXT."
  (let* ((opts (plist-get context :opts))
         (text (plist-get context :text))
         (result (parinferlib-indent-mode text opts)))
    (parinfer--apply-result result context)))

(defun parinfer--execute (context)
  "Execute parinfer with context CONTEXT."
  (when parinfer--delay-timer
    (cancel-timer parinfer--delay-timer)
    (setq parinfer--delay-timer nil))
  (let ((text (plist-get context :text)))
    (if (> (length text) parinfer-delay-invoke-threshold)
        (setq parinfer--delay-timer
              (run-with-idle-timer
               parinfer-delay-invoke-idle
               nil
               #'parinfer-indent-instantly))
      (parinfer--execute-instantly context))))
;; -----------------------------------------------------------------------------
;; Parinfer commands
;; -----------------------------------------------------------------------------

(defun parinfer-untabify-buffer ()
  "Untabify whole buffer.)
Currently parinfer can not handle indentation with tab.  Use this to remove tab indentation of your code."
  (interactive)
  (untabify (point-min) (point-max)))

(defun parinfer-auto-fix ()
  "Untabify whole buffer then reindent whole buffer."
  (interactive)
  (parinfer-untabify-buffer)
  (call-interactively 'mark-whole-buffer)
  (call-interactively 'indent-region)
  (call-interactively 'keyboard-quit)
  (call-interactively 'parinfer-indent-buffer))

(defun parinfer-indent ()
  "Parinfer indent."
  (let ((context (parinfer--prepare)))
    (parinfer--execute context)))

(defun parinfer-indent-instantly ()
  "Parinfer indent instantly."
  (let ((context (parinfer--prepare)))
    (parinfer--execute-instantly context)))

(defun parinfer-indent-buffer ()
  "Call parinfer indent on whole buffer."
  (interactive)
  (let* ((window-start-pos (window-start))
         (cursor-line (1- (line-number-at-pos)))
         (cursor-x (parinfer--cursor-x))
         (opts (list :cursor-x cursor-x :cursor-line cursor-line :preview-cursor-scope parinfer-preview-cursor-scope))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (result (parinferlib-indent-mode text opts))
         (changed-lines (plist-get result :changed-lines)))
    (when (and (plist-get result :success)
               changed-lines)
      (cl-loop for l in changed-lines do
               (parinfer--goto-line (1+ (plist-get l :line-no)))
               (delete-region (line-beginning-position)
                              (line-end-position))
               (insert (plist-get l :line)))
      (parinfer--goto-line (1+ cursor-line))
      (forward-char (plist-get result :cursor-x))
      (set-window-start (selected-window) window-start-pos))))

(defun parinfer-indent-with-confirm ()
  "Call parinfer indent on whole buffer.)
if there's any change, display a confirm message in minibuffer."
  (interactive)
  (let* ((window-start-pos (window-start))
         (cursor-line (1- (line-number-at-pos)))
         (cursor-x (parinfer--cursor-x))
         (opts (list :cursor-line cursor-line :cursor-x cursor-x))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (result (parinferlib-indent-mode text opts))
         (success (plist-get result :success))
         (changed-lines (plist-get result :changed-lines)))
    (if (not success)
        (progn
          (message "Pairs unmatched, swith to Paren mode. When pair fiexed, You can switch to indent mode.")
          nil)
      (if (and changed-lines
               (not (string= text (plist-get result :text))))
          (if (y-or-n-p "Caution! Buffer will be modified if you swith to Indent mode, continue? ")
              (progn (cl-loop for l in changed-lines do
                              (parinfer--goto-line (1+ (plist-get l :line-no)))
                              (delete-region (line-beginning-position)
                                             (line-end-position))
                              (insert (plist-get l :line)))
                     (parinfer--goto-line (1+ cursor-line))
                     (forward-char (plist-get result :cursor-x))
                     (set-window-start (selected-window) window-start-pos)
                     (setq parinfer--first-load nil)
                     t)
            nil)
        t))))

(defun parinfer-paren ()
  "Do parinfer paren  on current & previous top level S-exp."
  (ignore-errors (parinfer--reindent-sexp)))

(defun parinfer-ediff-quit ()
  "Quit ‘parinfer-diff’ directly, without confirm."
  (interactive)
  (ediff-really-quit nil)
  (with-current-buffer "*Parinfer Result*"
    (kill-buffer-and-window)))

(defun parinfer-backward-delete-char ()
  "Replacement in command ‘parinfer-mode’ for ‘backward-delete-char’ command."
  (interactive)
  (parinfer-run
   (if (eq 'paren parinfer--mode)
       (if (string-match-p "^[[:space:]]+$"
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (point)))
           (delete-indentation)
         (backward-delete-char 1))
     (backward-delete-char 1))))

(defun parinfer-backward-kill-word ()
  "Replacement in symbol 'parinfer-mode' for 'backward-kill-word' command."
  (interactive)
  (parinfer-run
   (call-interactively 'backward-kill-word)))

(defun parinfer-delete-char ()
  "Replacement in 'parinfer-mode' for 'delete-char' command."
  (interactive)
  (parinfer-run
   (delete-char 1)))

(defun parinfer-kill-word ()
  "Replacement in 'parinfer-mode' for 'kill-word' command."
  (interactive)
  (parinfer-run
   (call-interactively 'kill-word)))

(defun parinfer-kill-line ()
  "Replacement in 'parinfer-mode' for 'kill-line' command."
  (interactive)
  (parinfer-run
   (call-interactively 'kill-line)))

(defun parinfer-yank ()
  "Replacement in 'parinfer-mode' for 'yank' command."
  (interactive)
  (call-interactively 'yank)
  (setq parinfer--text-modified t)
  (parinfer-indent-buffer)
  (message "%s" parinfer--text-modified))

(defun parinfer-kill-region ()
  "Replacement in 'parinfer-mode' for 'kill-region' command."
  (interactive)
  (parinfer-run
   (call-interactively 'kill-region)))

(defun parinfer-newline ()
  "Replacement in 'parinfer-mode' for 'newline' command."
  (interactive)
  (parinfer-run
   (call-interactively 'newline)))

(defun parinfer-semicolon ()
  "Insert semicolon, always indent after insertion.
This is the very special situation, since we always need invoke parinfer after
every semicolon input."
  (interactive)
  (call-interactively 'self-insert-command)
  (parinfer-indent)
  (setq parinfer--text-modified t))

(defun parinfer-comment-dwim ()
  "Replacement in 'parinfer-mode' for 'comment-dwim' command."
  (interactive)
  (parinfer-run
   (call-interactively 'comment-dwim)))

(defun parinfer-delete-indentation ()
  "Replacement in 'parinfer-mode' for 'delete-indentation' command."
  (interactive)
  (parinfer-paren-run
   (call-interactively 'delete-indentation)))

(defun parinfer-toggle-mode ()
  "Switch parinfer mode between Indent Mode and Paren Mode."
  (interactive)
  (if (eq 'paren parinfer--mode)
      (parinfer--switch-to-indent-mode)
    (parinfer--switch-to-paren-mode)))

(defun parinfer-diff ()
  "Diff current code and the code after applying Indent Mode in Ediff.))
Use this to browse and apply the changes."
  (interactive)
  (let* ((orig-text (buffer-substring-no-properties (point-min) (point-max)))
         (new-buffer (generate-new-buffer "*Parinfer Result*"))
         (orig-buffer (current-buffer))
         (m major-mode)
         (result (parinferlib-indent-mode orig-text nil)))
    (with-current-buffer new-buffer
      (erase-buffer)
      (insert (plist-get result :text))
      (funcall m)
      (ediff-buffers orig-buffer new-buffer '(parinfer--ediff-init-keys)))))

(defun parinfer-shift-right ()
  "In Indent Mode with region active, shift text left."
  (interactive)
  (when (eq 'indent parinfer--mode)
    (parinfer--shift 1)))

(defun parinfer-shift-left ()
  "In Indent Mode with region active, shift text left."
  (interactive)
  (when (eq 'indent parinfer--mode)
    (parinfer--shift -1)))

;; -----------------------------------------------------------------------------
;; Keymaps
;; -----------------------------------------------------------------------------

(defvar parinfer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap backward-delete-char-untabify] 'parinfer-backward-delete-char)
    (define-key map [remap delete-backward-char] 'parinfer-backward-delete-char)
    (define-key map ";" 'parinfer-semicolon)
    (define-key map [remap yank] 'parinfer-yank)
    map))

(defvar parinfer-region-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'parinfer-shift-right)
    (define-key map (kbd "S-<tab>") 'parinfer-shift-left)
    (define-key map (kbd ">") 'parinfer-shift-right)
    (define-key map (kbd "<") 'parinfer-shift-left)
    (define-key map (kbd "<backspace>") 'delete-region)
    map))

;; -----------------------------------------------------------------------------
;; Mode
;; -----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode parinfer-mode
  "Parinfer mode."
  nil (:eval (parinfer--lighter)) parinfer-mode-map
  (if parinfer-mode
      (parinfer-mode-enable)
    (parinfer-mode-disable)))

;;;###autoload
(define-minor-mode parinfer-region-mode
  "Available when region is active."
  :init-value nil
  :keymap parinfer-region-mode-map)

;; -----------------------------------------------------------------------------
;; Additional commands, WIP.
;; -----------------------------------------------------------------------------
(defun parinfer-reverse-transpose-sexps ()
  "Reverse transpose."
  (interactive)
  (when (not (ignore-errors (transpose-sexps -1)))
    (forward-sexp)
    (ignore-errors (transpose-sexps -1))))

(defun parinfer-transpose-sexps ()
  "Transpose."
  (interactive)
  (when (not (ignore-errors (transpose-sexps 1)))
    (forward-sexp)
    (ignore-errors (transpose-sexps 1))))

(provide 'parinfer)
;;; parinfer.el ends here
