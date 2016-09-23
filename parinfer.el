;;; parinfer.el --- Simpler Lisp editing

;; Copyright (c) 2016, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/parinfer-mode
;; Version: 0.0.2
;; Package-Requires: ((paredit "24") (aggressive-indent "1.8.1") (cl-lib "0.5"))
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

;; * Enable parinfer-mode.
;; ~M-x parinfer-mode~
;;
;; or
;; #+BEGIN_SRC emacs-lisp
;;   (add-hook 'clojure-mode-hook #'parinfer-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;; #+END_SRC
;; Not work in Cider REPL now.
;;
;; * Toggle Indent and Paren mode.
;; Use ~parinfer-toggle-mode~.
;;
;; I recommand to add a keybinding for ~parinfer-toggle-mode~, since it will be used frequently.
;; #+BEGIN_SRC emacs-lisp
;;   (define-key parinfer-mode-map (kbd "C-,") 'parinfer-toggle-mode)
;; #+END_SRC
;; When the first time, you switch to Indent Mode, if your code will be modified by parinfer,
;; You will see a confirm message in minibuffer.  Type ~y~ for continue, ~n~ to stay in paren mode.
;;
;; Use ~parinfer-diff~ to see how parinfer will change the buffer with Ediff.
;;
;; Normally, after indenting the whole buffer with ~C-x h~ ~C-M-\~, you can switch to Indent Mode safely.
;;
;; * Work with Evil?
;; Not yet, some works are needed.  Will come soon.
;;
;; * Performance.
;; On each text modification, the current top-level form will be computed.
;; When switching to Indent mode, whole buffer will be computed.
;; No performance issue now.
;;
;; * Hooks?
;; ~parinfer-mode-enable-hook~ and ~parinfer-mode-disable-hook~.
;;
;; * Preview cursor scope?
;; Not support yet.
;;
;; * Credits
;; - [[https://github.com/oakmac][oakmac]] :: Bring Parinfer to Emacs.
;; - [[https://github.com/tumashu][tumashu]] :: Help me a lot in writing this plugin.
;;
;; * License
;; Licensed under the GPLv3.

;;; Code:

;; -----------------------------------------------------------------------------
;; Requires
;; -----------------------------------------------------------------------------
(require 'parinferlib)
(require 'aggressive-indent)
(require 'mode-local)
(require 'paredit)
(require 'cl-lib)
(require 'ediff)

;; -----------------------------------------------------------------------------
;; Constants & Variables
;; -----------------------------------------------------------------------------
(defconst parinfer-defun-regex "^[^ \n\t\"]"
  "Regex for finding the beginning of S-exp.")

(defvar parinfer-style 'paren
  "Parinfer mode style, 'paren or 'indent.")
(make-variable-buffer-local 'parinfer-style)

(defvar parinfer-first-load t
  "NOT MODIFY THIS, If haven't switch to indent mode yet.")
(make-variable-buffer-local 'parinfer-first-load)

(defvar parinfer-region-shifted nil
  "If shift the region after mark activate.")
(make-variable-buffer-local 'parinfer-region-shifted)

(defvar parinfer-indent-lighter " Parinfer:Indent"
  "Lighter for indent mode in mode line.")

(defvar parinfer-paren-lighter  " Parinfer:Paren"
  "Lighter for paren mode in mode line.")

(defvar parinfer-mode-enable-hook nil
  "Call after parinfer mode is enabled.")

(defvar parinfer-mode-disable-hook nil
  "Call after parinfer mode is disabled.")

(defvar parinfer-paren-modify-parentheses nil
  "If paren style can modify parentheses?")

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun parinfer-switch-to-indent-mode-aux ()
  "Swith to indent mode aux function."
  (setq parinfer-style 'indent)
  (message "Parinfer: Indent Mode")
  (when (bound-and-true-p company-mode)
    (remove-hook 'company-completion-finished-hook 'parinfer-reindent-sexp t))
  (force-mode-line-update))

(defun parinfer-switch-to-indent-mode ()
  "Switch to Indent Mode, this will apply indent fix on whole buffer.
If this is the first switching for current buffer and indent mode will change
Buffer text, we should see a confirm message."
  (if (not parinfer-first-load)
      (progn
        (parinfer-indent-buffer)
        (parinfer-switch-to-indent-mode-aux))
    (when (parinfer-indent-with-confirm)
      (parinfer-switch-to-indent-mode-aux))))

(defun parinfer-switch-to-paren-mode ()
  "Switch to paren mode."
  (setq parinfer-style 'paren)
  (message "Parinfer: Paren Mode")
  (when (bound-and-true-p company-mode)
    (add-hook 'company-completion-finished-hook 'parinfer-reindent-sexp t t))
  (force-mode-line-update))

(defun parinfer-in-comment-or-string-p ()
  "Return if we are in comment or string."
  (let ((f (get-text-property (point) 'face)))
    (or (eq f 'font-lock-comment-face)
        (eq f 'font-lock-comment-delimiter-face)
        (string= ";" (string (char-after)))
        (nth 3 (syntax-ppss))
        (nth 4 (syntax-ppss)))))

(defun parinfer-goto-line (n)
  "Goto the beginning of line N."
  (goto-char (point-min))
  (forward-line (1- n))
  (beginning-of-line))

(defun parinfer-goto-next-defun ()
  "Goto next defun, skip comment or string."
  (interactive)
  (let ((pt (point)))
    (when (not (eq (point-max) pt))
      (if (search-forward-regexp parinfer-defun-regex nil t)
          (if (= (1+ pt) (point))
              (parinfer-goto-next-defun)
            (let ((last-pos (point)))
              (while (and last-pos
                          (parinfer-in-comment-or-string-p)
                          (not (eq (point-max) (point))))
                (search-forward-regexp parinfer-defun-regex nil t)
                (if (eq (point) last-pos)
                    (setq last-pos nil)
                  (setq last-pos (point)))))
            (backward-char))
        (goto-char (point-max))))))

(defun parinfer-goto-previous-defun-aux ()
  "Goto previous defun aux function."
  (search-backward-regexp parinfer-defun-regex nil t)
  (let ((last-pos (point)))
    (while (and last-pos
                (parinfer-in-comment-or-string-p)
                (not (eq (point-min) (point))))
      (search-backward-regexp parinfer-defun-regex nil t)
      (if (eq (point) last-pos)
          (setq last-pos nil)
        (setq last-pos (point))))))

(defun parinfer-goto-previous-defun ()
  "Goto previous defun, skip comment or string."
  (interactive)
  (parinfer-goto-previous-defun-aux)
  (parinfer-goto-previous-defun-aux))

(defun parinfer-lighter ()
  "Return the lighter for specify mode."
  (if (eq 'paren parinfer-style)
      parinfer-paren-lighter
    parinfer-indent-lighter))

(defun parinfer-ediff-startup-hook ()
  "Inits for ediff.  since we don't need all features, simplify opeators."
  (local-set-key (kbd "q") 'parinfer-ediff-quit))

(defun parinfer-cursor-x ()
  "Get the cursor-x which is need by parinferlib computation."
  (length
   (buffer-substring-no-properties
    (line-beginning-position)
    (point))))

;; -----------------------------------------------------------------------------
;; Parinfer functions
;; -----------------------------------------------------------------------------

(defun parinfer-indent ()
  "Call parinfer indent on current & previous top level S-exp."
  (interactive)
  (let* ((window-start-pos (window-start))
         (start (save-excursion (parinfer-goto-previous-defun) (point)))
         (end (save-excursion (parinfer-goto-next-defun) (point)))
         (text (buffer-substring-no-properties start end))
         (line-number (line-number-at-pos))
         (cursor-line (- line-number (line-number-at-pos start)))
         (cursor-x (parinfer-cursor-x))
         (opts (list :cursor-x cursor-x :cursor-line cursor-line))
         (result (parinferlib-indent-mode text opts)))
    (when (and (plist-get result :success)
               (plist-get result :changed-lines))
      (delete-region start end)
      (insert (plist-get result :text))
      (parinfer-goto-line line-number)
      (forward-char (plist-get result :cursor-x))
      (set-window-start (selected-window) window-start-pos))))

(defun parinfer-indent-buffer ()
  "Call parinfer indent on whole buffer."
  (interactive)
  (let* ((window-start-pos (window-start))
         (cursor-line (1- (line-number-at-pos)))
         (cursor-x (parinfer-cursor-x))
         (opts (list :cursor-line cursor-line :cursor-x cursor-x))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (result (parinferlib-indent-mode text opts))
         (changed-lines (plist-get result :changed-lines)))
    (when (and (plist-get result :success)
               changed-lines)
      (cl-loop for l in changed-lines do
               (parinfer-goto-line (1+ (plist-get l :line-no)))
               (delete-region (line-beginning-position)
                              (line-end-position))
               (insert (plist-get l :line)))
      (parinfer-goto-line (1+ cursor-line))
      (forward-char (plist-get result :cursor-x))
      (set-window-start (selected-window) window-start-pos))))

(defun parinfer-indent-with-confirm ()
  "Call parinfer indent on whole buffer.
if there's any change, display a confirm message in minibuffer."
  (interactive)
  (let* ((window-start-pos (window-start))
         (cursor-line (1- (line-number-at-pos)))
         (cursor-x (parinfer-cursor-x))
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
                              (parinfer-goto-line (1+ (plist-get l :line-no)))
                              (delete-region (line-beginning-position)
                                             (line-end-position))
                              (insert (plist-get l :line)))
                     (parinfer-goto-line (1+ cursor-line))
                     (forward-char (plist-get result :cursor-x))
                     (set-window-start (selected-window) window-start-pos)
                     (setq parinfer-first-load nil)
                     t)
            nil)
        t))))

(defun parinfer-paren ()
  "Do parinfer paren  on current & previous top level S-exp.
Set `parinfer-paren-modify-parentheses` to t let paren mode modify content."
  (interactive)
  (when (and (not (ignore-errors (parinfer-reindent-sexp nil)))
             parinfer-paren-modify-parentheses)
    (let* ((window-start-pos (window-start))
           (start (save-excursion (parinfer-goto-previous-defun) (point)))
           (end (save-excursion (parinfer-goto-next-defun) (point)))
           (text (buffer-substring-no-properties start end))
           (line-number (line-number-at-pos))
           (cursor-line (- line-number (line-number-at-pos start)))
           (cursor-x (parinfer-cursor-x))
           (opts (list :cursor-x cursor-x :cursor-line cursor-line))
           (result (parinferlib-paren-mode text opts)))
      (progn
        (delete-region start end)
        (insert (plist-get result :text))
        (parinfer-goto-line line-number)
        (forward-char (plist-get result :cursor-x))
        (set-window-start (selected-window) window-start-pos)))))

(defun parinfer-reindent-sexp (&optional ignored)
  "Call aggressive-indent.))
IGNORED is for compatible with hook."
  (when (not (parinfer-in-comment-or-string-p))
    (call-interactively 'aggressive-indent-indent-defun)))

(defun parinfer-hook-fn ()
  "Supposed to be called after each content change."
  (cl-letf (((symbol-function 'message) #'format))
    (cond
     ((eq 'paren parinfer-style) (parinfer-paren))
     ((eq 'indent parinfer-style) (parinfer-indent))
     (t "nothing"))))

(defun parinfer-ediff-quit ()
  "Quit ‘parinfer-diff’ directly, without confirm."
  (interactive)
  (ediff-really-quit nil)
  (with-current-buffer "*Parinfer Result*"
    (kill-buffer-and-window)))

(defun parinfer-newline ()
  "Replacement in variable ‘parinfer-mode’ for newline command."
  (interactive)
  (newline-and-indent)
  (parinfer-hook-fn))

(defun parinfer-backward-delete-char ()
  "Replacement in command ‘parinfer-mode’ for ‘backward-delete-char’ command."
  (interactive)
  (if (eq 'paren parinfer-style)
      (if (string-match-p "^[[:space:]]+$"
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (point)))
          (delete-indentation)
        (backward-delete-char 1))
    (backward-delete-char 1))
  (parinfer-hook-fn))

(defun parinfer-backward-kill-word ()
  "Replacement in symbol 'parinfer-mode' for 'backward-kill-word' command."
  (interactive)
  (call-interactively 'backward-kill-word)
  (parinfer-hook-fn))

(defun parinfer-delete-char ()
  "Replacement in 'parinfer-mode' for 'delete-char' command."
  (interactive)
  (delete-char 1)
  (parinfer-hook-fn))

(defun parinfer-kill-word ()
  "Replacement in 'parinfer-mode' for 'kill-word' command."
  (interactive)
  (call-interactively 'kill-word)
  (parinfer-hook-fn))

(defun parinfer-kill-line ()
  "Replacement in 'parinfer-mode' for 'kill-line' command."
  (interactive)
  (call-interactively 'kill-line)
  (parinfer-hook-fn))

(defun parinfer-yank ()
  "Replacement in 'parinfer-mode' for 'yank' command."
  (interactive)
  (call-interactively 'yank)
  (parinfer-hook-fn))

(defun parinfer-enable ()
  "Enable 'parinfer-mode'."
   ;; Always use whitespace for indentation.
  (setq-mode-local parinfer-mode indent-tabs-mode nil)
  (run-hooks 'parinfer-mode-enable-hook)
  (add-hook 'post-self-insert-hook 'parinfer-hook-fn t t)
  (add-hook 'activate-mark-hook 'parinfer-region-mode-enable t t)
  (add-hook 'deactivate-mark-hook 'parinfer-region-mode-disable t t)
  (parinfer-switch-to-indent-mode))

(defun parinfer-disable ()
  "Disable 'parinfer-mode'."
  (run-hooks 'parinfer-mode-disable-hook)
  (remove-hook 'activate-mark-hook 'parinfer-region-mode-enable t)
  (remove-hook 'deactivate-mark-hook 'parinfer-region-mode-disable t)
  (remove-hook 'post-self-insert-hook 'parinfer-hook-fn t)
  (parinfer-region-mode-disable))

(defun parinfer-region-mode-enable ()
  "Run when region activated."
  (parinfer-region-mode 1))

(defun parinfer-region-mode-disable ()
  "Run when region deactivated, indent code if ‘parinfer-style’ is 'indent."
  (when (and (eq 'indent parinfer-style)
             parinfer-region-shifted)
    (parinfer-indent)
    (when (not (ignore-errors (parinfer-reindent-sexp nil)))
      (parinfer-indent-buffer))
    (setq parinfer-region-shifted nil))
  (parinfer-region-mode -1))

(defun parinfer-toggle-mode ()
  "Switch parinfer mode between Indent Mode and Paren Mode."
  (interactive)
  (if (eq 'paren parinfer-style)
      (parinfer-switch-to-indent-mode)
    (parinfer-switch-to-paren-mode)))

(defun parinfer-diff ()
  "Diff current code and the code after applying Indent Mode in Ediff.)
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
      (ediff-buffers orig-buffer new-buffer '(parinfer-ediff-startup-hook)))))

(defun parinfer-line-region ()
  "Auto adjust region so that the shift can work properly."
  (let* ((begin (region-beginning))
         (end (region-end))
         (new-begin (save-excursion (goto-char begin) (line-beginning-position))))
    (goto-char new-begin)
    (set-mark-command nil)
    (goto-char end)
    (setq deactivate-mark nil)
    (setq parinfer-region-shifted t)))

(defun parinfer-shift (distance)
  "Shift text.  For right, DISTANCE > 0; left, DISTANCE < 0."
  (when (use-region-p)
    (when (not parinfer-region-shifted)
      (parinfer-line-region))
    (let ((mark (mark)))
      (save-excursion
        (indent-rigidly (region-beginning)
                        (region-end)
                        distance)
        (push-mark mark t t)
        (setq deactivate-mark nil)))))

(defun parinfer-shift-right ()
  "In Indent Mode with region active, shift text left."
  (interactive)
  (when (eq 'indent parinfer-style)
    (parinfer-shift 1)))

(defun parinfer-shift-left ()
  "In Indent Mode with region active, shift text left."
  (interactive)
  (when (eq 'indent parinfer-style)
    (parinfer-shift -1)))


;; -----------------------------------------------------------------------------
;; Keymaps
;; -----------------------------------------------------------------------------

(defvar parinfer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-y") 'parinfer-yank)
    (define-key map (kbd "<backspace>") 'parinfer-backward-delete-char)
    (define-key map (kbd "M-<backspace>") 'parinfer-backward-kill-word)
    (define-key map (kbd "C-k") 'parinfer-kill-line)
    (define-key map (kbd "C-d") 'parinfer-backward-delete-char)
    (define-key map (kbd "M-d") 'parinfer-backward-delete-char)
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
  nil (:eval (parinfer-lighter)) parinfer-mode-map
  (if parinfer-mode
      (parinfer-enable)
    (parinfer-disable)))

;;;###autoload
(define-minor-mode parinfer-region-mode
  "Available when region is active."
  :init-value nil
  :keymap parinfer-region-mode-map)

;; -----------------------------------------------------------------------------
;; Macros
;; -----------------------------------------------------------------------------

(defmacro parinfer-paren-run (&rest body)
  "Run BODY in paren mode."
  `(progn
     (let* ((current-style parinfer-style)
            (toggle (eq current-style 'indent)))
       (when toggle
         (parinfer-switch-to-paren-mode))
       ,@body
       (when toggle
         (parinfer-switch-to-indent-mode)))))

;; -----------------------------------------------------------------------------
;; Additional commands port from Paredit, etc. WIP
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

(defun parinfer-forward-slurp-sexp ()
  "Useless now."
  (interactive)
  (parinfer-paren-run
   (paredit-forward-slurp-sexp)))

(defun parinfer-backward-slurp-sexp ()
  "Useless now."
  (interactive)
  (parinfer-paren-run
   (paredit-backward-slurp-sexp)))

(defun parinfer-forward-barf-sexp ()
  "Useless now."
  (interactive)
  (parinfer-paren-run
   (paredit-forward-barf-sexp)))

(defun parinfer-backward-barf-sexp ()
  "Useless now."
  (interactive)
  (parinfer-paren-run
   (paredit-backward-barf-sexp)))

(defun parinfer-raise-sexp ()
  "Useless now."
  (interactive)
  (parinfer-paren-run
   (paredit-raise-sexp)))

(defun parinfer-convolute-sexp ()
  "Useless now."
  (interactive)
  (parinfer-paren-run
   (paredit-convolute-sexp)))

(provide 'parinfer)
;;; parinfer.el ends here




