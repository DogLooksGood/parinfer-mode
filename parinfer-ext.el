;;; parinfer-ext.el --- Extensions of parinfer-mode

;; Copyright (c) 2016, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/parinfer-mode
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

;;; Commentary:

;; Extensions of parinfer

;;; Code:

(require 'parinfer)

(defgroup parinfer-ext
  nil
  "Parinfer customize group."
  :group 'faces)

;; -----------------------------------------------------------------------------
;; Pretty Parens
;; -----------------------------------------------------------------------------

(defface parinfer-pretty-parens:dim-paren-face
   '((((class color) (background dark))
      (:foreground "grey40"))
     (((class color) (background light))
      (:foreground "grey60")))
   "Parinfer dim paren face."
   :group 'parinfer-ext)

(parinfer-define-extension pretty-parens
  "Pretty parens.

Use rainbow-delimiters for Paren Mode, and dim-style parens for Indent Mode."
  :paren
  (font-lock-remove-keywords
   nil '((")\\|}\\|]" . 'parinfer-pretty-parens:dim-paren-face)))
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode-enable))
  (font-lock-flush)

  :indent
  (when (bound-and-true-p rainbow-delimiters-mode)
    (rainbow-delimiters-mode-disable))
  (font-lock-add-keywords
   nil '((")\\|}\\|]" . 'parinfer-pretty-parens:dim-paren-face)))
  (font-lock-flush))

;; -----------------------------------------------------------------------------
;; company
;; -----------------------------------------------------------------------------

(defun parinfer-company:cancel (&ignored)
  "Invoke when company cancelled, ignore IGNORED."
  (parinfer-indent))

(defun parinfer-company:finish (&ignored)
  "Invoke when company finished, ignore IGNORED. "
  (parinfer--reindent-sexp))

(parinfer-define-extension company
  "Compatibility fix for company-mode."
  :indent
  (when (bound-and-true-p company-mode)
    (add-hook 'company-completion-cancelled-hook
              'parinfer-company:cancel t t)
    (remove-hook 'company-completion-finished-hook
                 'parinfer-company:finish t))
  :paren
  (when (bound-and-true-p company-mode)
    (add-hook 'company-completion-finished-hook
              'parinfer-company:finish t t)
    (remove-hook 'company-completion-cancelled-hook
                 'parinfer-company:cancel t)))

;; -----------------------------------------------------------------------------
;; lispy
;; -----------------------------------------------------------------------------

(defvar lispy-mode-map)

(defun parinfer-lispy:space ()
  (interactive)
  (if (or (eq (point) (line-beginning-position))
          (eq (point) (line-end-position)))
      (call-interactively 'self-insert-command)
    (progn
      (call-interactively 'self-insert-command)
      (when (parinfer-lispy:paren-left-and-between-parens-p)
        (backward-char)))))

(defun parinfer-lispy:forward ()
  (interactive)
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'lispy-forward))

(defun parinfer-lispy:backward ()
  (interactive)
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'lispy-backward))

(defun parinfer-lispy:paren-char-p (c)
  (or (eq c 40)
      (eq c 91)
      (eq c 123)))

(defun parinfer-lispy:paren-left-and-between-parens-p ()
  (let ((ca (char-after))
        (cb (char-before (- (point) 1))))
    (and (not (parinfer--in-comment-or-string-p))
         (parinfer-lispy:paren-char-p ca)
         (parinfer-lispy:paren-char-p cb))))

(defun parinfer-lispy:parens ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'lispy-parens)
    (call-interactively 'self-insert-command)))

(defun parinfer-lispy:brackets ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'lispy-brackets)
    (call-interactively 'self-insert-command)))

(defun parinfer-lispy:braces ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'lispy-braces)
    (call-interactively 'self-insert-command)))

(defun parinfer-lispy:init ()
  (define-key lispy-mode-map (kbd "(") 'parinfer-lispy:parens)
  (define-key lispy-mode-map (kbd ")") 'self-insert-command)
  (define-key lispy-mode-map (kbd "[") 'parinfer-lispy:brackets)
  (define-key lispy-mode-map (kbd "]") 'self-insert-command)
  (define-key lispy-mode-map (kbd "{") 'parinfer-lispy:braces)
  (define-key lispy-mode-map (kbd "}") 'self-insert-command)
  (define-key lispy-mode-map (kbd ";") 'parinfer-semicolon)
  (define-key lispy-mode-map [remap lispy-kill] 'kill-line)
  (define-key lispy-mode-map [remap lispy-tick] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-tilde] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-newline-and-indent-plain] 'newline)
  (define-key lispy-mode-map [remap lispy-quotes] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-yank] 'parinfer-yank)
  (define-key lispy-mode-map [remap lispy-colon] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-hash] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-hat] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-delete-backward] 'parinfer-backward-delete-char)
  (define-key lispy-mode-map [remap lispy-space] 'parinfer-lispy:space))

(parinfer-define-extension lispy
  "Integration with Lispy."

  :indent
  (lispy-mode 1)

  :paren
  (lispy-mode -1)

  :mount
  (require 'eldoc)
  (eldoc-add-command-completions "lispy-" "parinfer-")
  (parinfer-strategy-add 'default
    '(parinfer-lispy:parens
      parinfer-lispy:braces
      parinfer-lispy:brackets
      parinfer-lispy:space))
  (add-hook 'lispy-mode-hook #'parinfer-lispy:init)

  :unmount
  (lispy-mode -1))

;; -----------------------------------------------------------------------------
;; Evil
;; -----------------------------------------------------------------------------

(parinfer-define-extension evil
  "Integration with Evil."
  :mount
  (parinfer-strategy-add 'default
    'evil-delete-char)
  (parinfer-strategy-add 'instantly
    '(evil-delete evil-change evil-change-line evil-paste-before evil-paste-after
      evil-delete-line evil-delete-char evil-delete-backward-char evil-substitute
      evil-change-whole-line evil-force-normal-state evil-normal-state
      evil-shift-left evil-shift-right))
  (parinfer-strategy-add 'skip
    '(evil-previous-line evil-forward-char evil-backward-char evil-next-line
      evil-forward-word evil-forward-word-begin evil-backward-word-begin
      evil-backward-end evil-scroll-page-down evil-scroll-up)))

;; -----------------------------------------------------------------------------
;; Smart yank
;; -----------------------------------------------------------------------------

(defun parinfer-smart-yank:paren-yank ()
  (interactive)
  (let ((yank-str nil))
    (with-temp-buffer
      (yank)
      (parinfer-indent-buffer)
      (setq yank-str (buffer-substring-no-properties (point-min) (point-max))))
    (parinfer-paren-run
     (insert yank-str)
     (parinfer--reindent-sexp))))

(defun parinfer-smart-yank:yank ()
  "Yank behaviour depend on current mode(Indent/Paren)."
  (interactive)
  (cl-case (parinfer-current-mode)
    (indent (call-interactively 'parinfer-yank))
    (paren (call-interactively 'parinfer-smart-yank:paren-yank))))

(parinfer-define-extension smart-yank
  "Yank depend on current mode."
  :mount
  (define-key parinfer-mode-map [remap yank] 'parinfer-smart-yank:yank))

;; -----------------------------------------------------------------------------
;; Smart TAB
;; -----------------------------------------------------------------------------

(defconst parinfer-smart-tab:close-paren-regex "\\()\\|]\\|}\\)")

(defvar parinfer-smart-tab:indicator-line nil)
(make-variable-buffer-local 'parinfer-smart-tab:indicator-line)

(defface parinfer-smart-tab:indicator-face
  '((((class color) (background dark))
     (:background "grey40"))
    (((class color) (background light))
     (:background "grey60")))
   "Parinfer Smart TAB indicator."
   :group 'parinfer-ext)

(defun parinfer-smart-tab:clean-indicator-pre ()
  (interactive)
  (when (and parinfer-smart-tab:indicator-line
             (symbolp this-command)
             (not (eq this-command 'parinfer-smart-tab:forward-char))
             (not (eq this-command 'parinfer-smart-tab:backward-char)))
    (save-excursion
      (parinfer--goto-line parinfer-smart-tab:indicator-line)
      (remove-text-properties
       (line-beginning-position)
       (line-end-position)
       '(font-lock-face 'parinfer-smart-tab:indicator-face)))))

(defun parinfer-smart-tab:clean-indicator ()
  (interactive)
  (when (and parinfer-smart-tab:indicator-line
             (symbolp this-command)
             (not (eq this-command 'parinfer-smart-tab:forward-char))
             (not (eq this-command 'parinfer-smart-tab:backward-char)))
    (if (and (eq (line-number-at-pos) parinfer-smart-tab:indicator-line)
             (parinfer--empty-line-p))
        (delete-region (point) (line-end-position))
      (save-excursion
        (parinfer--goto-line parinfer-smart-tab:indicator-line)
        (when (parinfer--empty-line-p)
          (delete-region (line-beginning-position) (line-end-position)))))
    (setq parinfer-smart-tab:indicator-line nil)))

(defun parinfer-smart-tab:at-close-paren-p ()
  (let ((c (char-after)))
    (or (eq c 41)
        (eq c 93)
        (eq c 125))))

(defun parinfer-smart-tab:should-ignore-positions (pos-list)
  (or (not pos-list)
      (= 1 (length pos-list))))

(defun parinfer-smart-tab:find-possible-positions ()
  (save-excursion
    (unless (= 1 (line-number-at-pos))
      (forward-line -1)
      (while (and (or (parinfer--in-comment-or-string-p)
                      (parinfer--empty-line-p))
                  (not (= 1 (line-number-at-pos))))
        (forward-line -1))
      (unless (or (parinfer--in-comment-or-string-p)
                  (parinfer--empty-line-p))
        (end-of-line)
        (let ((pos-list '(0)))
          (newline-and-indent)
          (message "%s" (- (point) (line-beginning-position)))
          (add-to-list 'pos-list (- (point) (line-beginning-position)))
          (delete-indentation)
          (backward-char)
          (while (parinfer-smart-tab:at-close-paren-p)
            (newline-and-indent)
            (add-to-list 'pos-list (- (point) (line-beginning-position)))
            (delete-indentation)
            (backward-char))
          (end-of-line)
          (while (eq 32 (char-before)) 
            (backward-delete-char 1))
          (-distinct pos-list))))))

(defun parinfer-smart-tab:mark-positions (pos-list)
  (unless (parinfer-smart-tab:should-ignore-positions pos-list)
    (setq parinfer-smart-tab:indicator-line (line-number-at-pos))
    (let ((ln (car pos-list))
          (current-x (- (point) (line-beginning-position))))
      (delete-region (line-beginning-position) (line-end-position))
      (cl-loop for i from 0 to ln do
               (if (-contains-p pos-list i)
                   (insert (propertize " " 'font-lock-face 'parinfer-smart-tab:indicator-face))
                 (insert " ")))
      (beginning-of-line)
      (if (> current-x ln)
          (progn (end-of-line) (backward-char))
        (forward-char current-x)))))

(defun parinfer-smart-tab:forward-char ()
  (interactive)
  (when (and (not (parinfer--in-comment-or-string-p))
             (parinfer--empty-line-p)
             (not parinfer-smart-tab:indicator-line))
    (let ((pos-list (parinfer-smart-tab:find-possible-positions)))
      (parinfer-smart-tab:mark-positions pos-list)))
  (if (and parinfer-smart-tab:indicator-line
           (eq (line-number-at-pos) parinfer-smart-tab:indicator-line))
      (progn
        (if (eq (point) (line-end-position))
            (beginning-of-line)
          (forward-char))
        (while (not (eq (face-at-point) 'parinfer-smart-tab:indicator-face))
          (if (eq (point) (line-end-position))
              (beginning-of-line)
            (forward-char))))
    (call-interactively 'forward-char)))

(defun parinfer-smart-tab:backward-char ()
  (interactive)
  (when (and (not (parinfer--in-comment-or-string-p))
             (parinfer--empty-line-p)
             (not parinfer-smart-tab:indicator-line))
    (let ((pos-list (parinfer-smart-tab:find-possible-positions)))
      (parinfer-smart-tab:mark-positions pos-list)))
  (if (and parinfer-smart-tab:indicator-line
           (eq (line-number-at-pos) parinfer-smart-tab:indicator-line))
      (progn
        (if (eq (point) (line-beginning-position))
            (end-of-line)
          (backward-char))
        (while (not (eq (face-at-point) 'parinfer-smart-tab:indicator-face))
          (if (eq (point) (line-beginning-position))
              (end-of-line)
            (backward-char))))
    (call-interactively 'backward-char)))

(parinfer-define-extension smart-tab
  "Smart forward-char & backward-char."
  :mount
  (add-hook 'post-command-hook 'parinfer-smart-tab:clean-indicator t t)
  (add-hook 'pre-command-hook 'parinfer-smart-tab:clean-indicator-pre t t)
  (define-key parinfer-mode-map [remap forward-char] 'parinfer-smart-tab:forward-char)
  (define-key parinfer-mode-map [remap backward-char] 'parinfer-smart-tab:backward-char)

  :unmount
  (remove-hook 'post-command-hook 'parinfer-smart-tab:clean-indicator t)
  (remove-hook 'pre-command-hook 'parinfer-smart-tab:clean-indicator-pre t))
  
(provide 'parinfer-ext)
;;; parinfer-ext.el ends here



          
          


          
