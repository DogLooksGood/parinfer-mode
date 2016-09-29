;;; parinfer.el --- Simpler Lisp editing

;; Copyright (c) 2016, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/parinfer-mode
;; Version: 0.1.4
;; Package-Requires: ((dash "2.13.0") (aggressive-indent "1.8.1") (cl-lib "0.5")
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

;; * Toggle Indent and Paren mode.
;; Use ~parinfer-toggle-mode~.

;; I recommand to add a keybinding for ~parinfer-toggle-mode~, since it will be used frequently.
;; #+BEGIN_SRC emacs-lisp
;;   (define-key parinfer-mode-map (kbd "C-,") 'parinfer-toggle-mode)
;; #+END_SRC
;; When the first time, you switch to Indent Mode, if your code will be modified by parinfer,
;; You will see a confirm message in minibuffer.  Type ~y~ for continue, ~n~ to stay in paren mode.

;; Use ~parinfer-diff~ to see how parinfer will change the buffer with Ediff.

;; Some keybindings in Ediff:
;; | Key  | Description                                               |
;; |------+-----------------------------------------------------------|
;; | ~q~  | Quit diff.                                                |
;; | ~b~  | B->A in Ediff, this can apply change to your origin code.  |
;; | ~ra~ | Restore A in Ediff, this can revert change.               |
;; | ~n~  | Move to next difference.                                  |
;; | ~p~  | Move to previous difference.                              |

;; Normally, after indenting the whole buffer with ~C-x h~ ~C-M-\~, you can switch to Indent Mode safely.

;; * FAQ
;; ** Project status.
;; I'm already using parinfer-mode for Clojure And Elisp.  It should be stable and should work as expected.
;; If there's any bug or uncomfortable stuff, open an issue please.

;; ** I found command XXX break matched parens!
;; If XXX is a built-in or wild used command, please open a issue, I'll do a fix.

;; Alternatively, you can fix it too.  There're two macros.

;; *** parinfer-run
;; This macro will run the BODY code, then invoke parinfer to fix parentheses(if we are in indent-mode).
;; #+BEGIN_SRC emacs-lisp
;;   ;; This is a sample, parinfer-mode have already remap yank with parinfer-yank.

;;   (defun parinfer-yank ()
;;     (interactive)
;;     (parinfer-run
;;      (call-interactively 'yank)))

;;   ;; Replace yank to parinfer-yank.
;;   (define-key parinfer-mode-map [remap yank] 'parinfer-yank)
;; #+END_SRC

;; *** parinfer-paren-run
;; This macro will always run BODY in paren-mode, avoid changing the S-exp struct.
;; #+BEGIN_SRC emacs-lisp
;;   ;; This is a sample, parinfer-mode already remap delete-indentation with parinfer-delete-indentation.

;;   (defun parinfer-delete-indentation ()
;;     (interactive)
;;     (parinfer-paren-run
;;      (call-interactively 'delete-indentation)))

;;   ;; Replace delete-indentation to parinfer-indentation.
;;   (define-key parinfer-mode-map [remap delete-indentation] 'parinfer-delete-indentation)
;; #+END_SRC

;; ** Parinfer-mode toggle indent mode is changing the indentation.
;; The indentation of code should not be changed by indent mode.  When you meet this, your code probably have indentation with *TAB*.

;; Currently Parinfer can not handle tab indentation, you can change all tab indentation to whitespace for current buffer with ~M-x parinfer-untabify-buffer~.

;; ** Use with Evil?
;; Parinfer mode only works in insert-state.

;; But there's already a plugin called [[https://github.com/luxbock/evil-cleverparens][evil-cleverparens]] , that handles parentheses nicely for evil normal or visual states.

;; If you are using evil, try using ~evil-cleverparens~ + ~parinfer-mode~ .

;; ** Use in Cider REPL?
;; Not yet, I simply use ~electric-pair-mode~ for auto pairs.
;; #+BEGIN_SRC emacs-lisp
;;   (add-hook 'cider-repl-mode-hook #'electric-pair-mode)
;; #+END_SRC

;; ** Performance.
;; On each text modification, the current & previous top-level form will be computed.
;; When switching to Indent mode, whole buffer will be computed.
;; No performance issue now.

;; ** Hooks?
;; ~parinfer-mode-enable-hook~ and ~parinfer-mode-disable-hook~.

;; ** Preview cursor scope?
;; Not support yet.

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
(require 'paredit)
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

(defvar parinfer-indent-lighter " Parinfer:Indent"
  "Lighter for indent mode in mode line.")

(defvar parinfer-paren-lighter  " Parinfer:Paren"
  "Lighter for paren mode in mode line.")

(defvar parinfer-mode-enable-hook nil
  "Call after parinfer mode is enabled.")

(defvar parinfer-mode-disable-hook nil
  "Call after parinfer mode is disabled.")

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

(defvar parinfer-clean-up-command-list
  (list 'evil-delete-char 'self-insert-command)
  "After these commands, parinfer will invoke when cursor leave current line.")

(defvar parinfer-skip-command-list
  (list 'evil-previous-line 'evil-forward-char 'evil-backward-char 'evil-next-line
        'evil-forward-word 'evil-forward-word-begin 'evil-backward-word-begin 'evil-backward-end
        'evil-scroll-page-down 'evil-scroll-up)
  "Do not invoke parinfer after these commands.")

(defvar parinfer-skip-command-prefix-list '("eval-" "parinfer-")
  "Do not invoke parinfer after commands with these prefixs.")

(defvar parinfer-instantly-invoke-command-list
  (list 'evil-delete 'evil-change 'evil-change-line 'evil-paste-before 'evil-paste-after
        'evil-delete-line 'evil-delete-char 'evil-delete-backward-char 'evil-substitute
        'evil-change-whole-line 'evil-force-normal-state 'evil-normal-state
        'evil-shift-left 'evil-shift-right)
  "Invoke parinfer instantly after these commands.")

(defvar parinfer-instantly-invoke-command-prefix-list ()
  "Invoke parinfer instantly after commands with these prefixs.")

;; -----------------------------------------------------------------------------
;; Macros
;; -----------------------------------------------------------------------------

(defmacro parinfer-paren-run (&rest body)
  "Run BODY in paren mode.  Keep S-sexp in correct indentation."
  `(progn
     (let* ((current-style parinfer--mode)
            (toggle (eq current-style 'indent)))
       (cl-letf (((symbol-function 'message) #'format))
         (when toggle
           (parinfer--switch-to-paren-mode))
         ,@body
         (when toggle
           (parinfer--reindent-sexp)
           (parinfer--indent-and-switch-to-indent-mode))))))

(defmacro parinfer-run (&rest body)
  "Run BODY, then invode parinfer(depend on current parinfermode) immediately."
  `(progn
     ,@body
     (parinfer--invoke-parinfer)))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun parinfer--set-text-modified ()
  "Set ‘parinfer--text-modified’ to t."
  (when (and (symbolp this-command)
             (member this-command parinfer-clean-up-command-list))
    (setq parinfer--text-modified t)))

(defun parinfer--unset-text-modified ()
  "Set ‘parinfer--text-modified’ to nil."
  (setq parinfer--text-modified nil))

(defun parinfer--disable-rainbow-delimiters ()
  "Disable rainbow delimiters if it's enabled."
  (when (bound-and-true-p rainbow-delimiters-mode)
    (rainbow-delimiters-mode-disable)))

(defun parinfer--enable-rainbow-delimiters ()
  "Enable rainbow delimiters if it's been installed."
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode-enable)))

(defun parinfer--switch-to-indent-mode-aux ()
  "Swith to indent mode aux function."
  (setq parinfer--mode 'indent)
  (setq parinfer--first-load nil)
  (message "Parinfer: Indent Mode")
  (when (bound-and-true-p company-mode)
    (add-hook 'company-completion-cancelled-hook 'parinfer-indent t t)
    (remove-hook 'company-completion-finished-hook 'parinfer--reindent-sexp t))
  (when parinfer-indent-mode-dim-close-parens
    (parinfer--disable-rainbow-delimiters)
    (parinfer--enable-dim-parens))
  (force-mode-line-update))

(defun parinfer--switch-to-indent-mode ()
  "Switch to Indent Mode, this will apply indent fix on whole buffer.)
If this is the first switching for current buffer and indent mode will change
Buffer text, we should see a confirm message."
  (if (not parinfer--first-load)
      (progn
        (parinfer-indent-buffer)
        (parinfer--switch-to-indent-mode-aux))
    (when (parinfer-indent-with-confirm)
      (parinfer--switch-to-indent-mode-aux))))

(defun parinfer--indent-and-switch-to-indent-mode ()
  "Switch to Indent mode and call Indent Mode immediately."
  (interactive)
  (parinfer--switch-to-indent-mode-aux)
  (parinfer--invoke-parinfer-when-necessary))

(defun parinfer--switch-to-paren-mode ()
  "Switch to paren mode."
  (setq parinfer--mode 'paren)
  (message "Parinfer: Paren Mode")
  (when (bound-and-true-p company-mode)
    (remove-hook 'company-completion-cancelled-hook 'parinfer-indent t)
    (add-hook 'company-completion-finished-hook 'parinfer--reindent-sexp t t))
  (when parinfer-indent-mode-dim-close-parens
    (parinfer--disable-dim-parens)
    (parinfer--enable-rainbow-delimiters))
  (force-mode-line-update))

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

(defun parinfer--goto-previous-defun-aux ()
  "Goto previous defun aux function."
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
  (parinfer--goto-previous-defun-aux)
  (parinfer--goto-previous-defun-aux))

(defun parinfer--lighter ()
  "Return the lighter for specify mode."
  (if (eq 'paren parinfer--mode)
      parinfer-paren-lighter
    parinfer-indent-lighter))

(defun parinfer--ediff-init-keys ()
  "Inits for ediff.  since we don't need all features, simplify opeators."
  (local-set-key (kbd "q") 'parinfer-ediff-quit))

(defun parinfer--cursor-x ()
  "Get the cursor-x which is need by parinferlib computation."
  (length
   (buffer-substring-no-properties
    (line-beginning-position)
    (point))))

(defun parinfer--reindent-sexp (&optional ignored)
  "Call aggressive-indent.))
IGNORED is for compatible with hook."
  (when (not (parinfer--in-comment-or-string-p))
    (aggressive-indent-indent-defun)))

(defun parinfer--invoke-parinfer-instantly (&optional pos)
  "Call Parinfer at POS immediately."
  (if (and pos (not (eq pos (point))))
      (let ((current-pos (point)))
        (goto-char pos)
        (parinfer--invoke-parinfer-instantly)
        (goto-char current-pos))
    (cl-letf (((symbol-function 'message) #'format))
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
    (cl-letf (((symbol-function 'message) #'format))
      (cond
       ((eq 'paren parinfer--mode) (parinfer-paren))
       ((eq 'indent parinfer--mode) (parinfer-indent))
       (t "nothing")))))

(defun parinfer--should-disable-p ()
  "Should parinfer disabled at this moment."
  (and (bound-and-true-p multiple-cursors-mode)))

(parinfer--should-disable-p)

(defun parinfer--should-skip-this-command-p ()
  "Should parinfer skip this command."
  (if (member this-command parinfer-skip-command-list)
      t
    (-any-p (lambda (prefix) (string-prefix-p prefix (symbol-name this-command)))
            parinfer-skip-command-prefix-list)))

(defun parinfer--should-invoke-instantly-p ()
  "Should parinfer be invoked instantly."
  (if (member this-command parinfer-instantly-invoke-command-list)
      t
    (-any-p (lambda (prefix) (string-prefix-p prefix (symbol-name this-command)))
            parinfer-instantly-invoke-command-prefix-list)))

(parinfer--should-skip-this-command-p)

(defun parinfer--invoke-parinfer-when-necessary ()
  "Invoke parinfer when necessary."
  (when this-command
    (cond
     ((not (symbolp this-command))
      nil)
     
     ((parinfer--should-skip-this-command-p) nil)

     ((parinfer--in-comment-or-string-p) nil)

     ;; Disable when region is active.
     ((region-active-p) nil)

     ((parinfer--should-disable-p) nil)

     ((and (eq 'indent parinfer--mode)
           parinfer--text-modified
           (not (equal parinfer--last-line-number (line-number-at-pos))))
      (parinfer--invoke-parinfer-instantly (save-excursion
                                             (parinfer--goto-line parinfer--last-line-number)
                                             (line-beginning-position))))

     ((parinfer--should-invoke-instantly-p)
      (parinfer--invoke-parinfer-instantly (point)))

     ((eq this-command 'self-insert-command)
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
    (when (not (ignore-errors (parinfer--reindent-sexp nil)))
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

(defun parinfer--apply-result (result ctx)
  "Apply parinfer RESULT to current buffer.
CTX is the context for parinfer execution."
  (let* ((orig (plist-get ctx :orig))
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
  
(defun parinfer--execute-instantly (ctx)
  "Execute parinfer instantly with context CTX."
  (let* ((opts (plist-get ctx :opts))
         (text (plist-get ctx :text))
         (result (parinferlib-indent-mode text opts)))
    (parinfer--apply-result result ctx)))

(defun parinfer--execute (ctx)
  "Execute parinfer with context CTX."
  (when parinfer--delay-timer
    (cancel-timer parinfer--delay-timer)
    (setq parinfer--delay-timer nil))
  (let ((text (plist-get ctx :text)))
    (if (> (length text) parinfer-delay-invoke-threshold)
        (setq parinfer--delay-timer
              (run-with-idle-timer
               parinfer-delay-invoke-idle
               nil
               #'parinfer-indent-instantly))
      (parinfer--execute-instantly ctx))))
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
  (call-interactively 'keyboard-quit))

(defun parinfer-indent ()
  "Parinfer indent."
  (interactive)
  (let ((ctx (parinfer--prepare)))
    (parinfer--execute ctx)))

(defun parinfer-indent-instantly ()
  "Parinfer indent instantly."
  (let ((ctx (parinfer--prepare)))
    (parinfer--execute-instantly ctx)))

;; (defun parinfer-indent ()
;;   "Call parinfer indent on current & previous top level S-exp."
;;   (interactive)
;;   (let* ((window-start-pos (window-start))
;;          (start (save-excursion (parinfer--goto-previous-defun) (point)))
;;          (end (save-excursion (parinfer--goto-next-defun) (point)))
;;          (text (buffer-substring-no-properties start end))
;;          (line-number (line-number-at-pos))
;;          (cursor-line (- line-number (line-number-at-pos start)))
;;          (cursor-x (parinfer--cursor-x))
;;          (opts (list :cursor-x cursor-x :cursor-line cursor-line :preview-cursor-scope parinfer-preview-cursor-scope))
;;          (result (parinferlib-indent-mode text opts)))
;;     (when (and (plist-get result :success)
;;                (plist-get result :changed-lines))
;;       (delete-region start end)
;;       (insert (plist-get result :text))
;;       (parinfer--goto-line line-number)
;;       (forward-char (plist-get result :cursor-x))
;;       (set-window-start (selected-window) window-start-pos))))

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
  (interactive)
  (ignore-errors (parinfer--reindent-sexp nil)))

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
  (parinfer-run
   (call-interactively 'yank)))

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
  "Insert semicolon, always indent after insertion."
  (interactive)
  (call-interactively 'self-insert-command)
  (parinfer-indent))

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
    (define-key map [remap delete-indentation] 'parinfer-delete-indentation)
    (define-key map [remap kill-line] 'parinfer-kill-line)
    (define-key map [remap yank] 'parinfer-yank)
    (define-key map [remap comment-dwim] 'parinfer-comment-dwim)
    (define-key map [remap kill-region] 'parinfer-kill-region)
    (define-key map [remap kill-word] 'parinfer-kill-word)
    (define-key map [remap delete-char] 'parinfer-delete-char)
    (define-key map [remap newline] 'parinfer-newline)
    (define-key map ";" 'parinfer-semicolon)
    (define-key map [remap delete-backward-char] 'parinfer-backward-delete-char)
    (define-key map [remap backward-delete-char-untabify] 'parinfer-backward-delete-char)
    map))

(defvar parinfer-region-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'parinfer-shift-right)
    (define-key map (kbd "S-<tab>") 'parinfer-shift-left)
    (define-key map (kbd ">") 'parinfer-shift-right)
    (define-key map (kbd "<") 'parinfer-shift-left)
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


