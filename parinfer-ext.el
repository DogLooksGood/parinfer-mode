;;; parinfer-ext.el --- Extensions of parinfer-mode

;; Copyright (c) 2016, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/parinfer-ext:mode
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

(defgroup parinfer-ext
  nil
  "Parinfer customize group."
  :group 'faces)

;; -----------------------------------------------------------------------------
;; dim-paren
;; -----------------------------------------------------------------------------

(defface parinfer-ext:dim-paren-face
   '((((class color) (background dark))
      (:foreground "grey40"))
     (((class color) (background light))
      (:foreground "grey60")))
   "Parinfer dim paren face."
   :group 'parinfer-ext)

(parinfer-define-extension dim-paren
  (cl-case mode
    (paren (font-lock-remove-keywords
            nil '((")\\|}\\|]" . 'parinfer-ext:dim-paren-face))))
    (indent (font-lock-add-keywords
             nil '((")\\|}\\|]" . 'parinfer-ext:dim-paren-face)))))
  (font-lock-flush))

;; -----------------------------------------------------------------------------
;; company
;; -----------------------------------------------------------------------------

(defun parinfer-ext:company-cancel (&ignored)
  "Invoke when company cancelled, ignore IGNORED."
  (parinfer-indent))

(defun parinfer-ext:company-finish (&ignored)
  "Invoke when company finished, ignore IGNORED. "
  (parinfer--reindent-sexp))

(parinfer-define-extension company
  (when (bound-and-true-p company-mode)
    (add-hook 'company-completion-cancelled-hook
              'parinfer-ext:company-cancel t t)
    (remove-hook 'company-completion-finished-hook
                 'parinfer-ext:company-finish t)))

;; -----------------------------------------------------------------------------
;; rainbow-delimiters
;; -----------------------------------------------------------------------------

(parinfer-define-extension rainbow-delimiters
  (cl-case mode
    (indent (when (bound-and-true-p rainbow-delimiters-mode)
              (rainbow-delimiters-mode-disable)))
    (paren (when (fboundp 'rainbow-delimiters-mode)
             (rainbow-delimiters-mode-enable)))))

;; -----------------------------------------------------------------------------
;; lispy
;; -----------------------------------------------------------------------------

(defun parinfer-ext:lispy-space ()
  (interactive)
  (if (or (eq (point) (line-beginning-position))
          (eq (point) (line-end-position)))
      (call-interactively 'self-insert-command)
    (progn
      (call-interactively 'self-insert-command)
      (when (parinfer-ext:lispy-left-between-parens-p)
        (backward-char)))))

(defun parinfer-ext:lispy-forward ()
  (interactive)
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'lispy-forward))

(defun parinfer-ext:lispy-backward ()
  (interactive)
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'lispy-backward))

(defun parinfer-ext:lispy-paren-char-p (c)
  (or (eq c 40)
      (eq c 91)
      (eq c 123)))

(defun parinfer-ext:lispy-left-between-parens-p ()
  (let ((ca (char-after))
        (cb (char-before (- (point) 1))))
    (and (not (parinfer--in-comment-or-string-p))
         (parinfer-ext:lispy-paren-char-p ca)
         (parinfer-ext:lispy-paren-char-p cb))))

(defun parinfer-ext:lispy-parens ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'lispy-parens)
    (call-interactively 'self-insert-command)))

(defun parinfer-ext:lispy-brackets ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'lispy-brackets)
    (call-interactively 'self-insert-command)))

(defun parinfer-ext:lispy-braces ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'lispy-braces)
    (call-interactively 'self-insert-command)))

(defun parinfer-ext:lispy-switch-mode-behaviour (mode)
  (if (eq mode 'indent)
      (lispy-mode 1)
    (lispy-mode -1)))

(defun parinfer-ext:lispy-init ()
  (define-key lispy-mode-map (kbd "(") 'parinfer-ext:lispy-parens)
  (define-key lispy-mode-map (kbd ")") 'self-insert-command)
  (define-key lispy-mode-map (kbd "[") 'parinfer-ext:lispy-brackets)
  (define-key lispy-mode-map (kbd "]") 'self-insert-command)
  (define-key lispy-mode-map (kbd "{") 'parinfer-ext:lispy-braces)
  (define-key lispy-mode-map (kbd "}") 'self-insert-command)
  (define-key lispy-mode-map (kbd ";") 'parinfer-ext:semicolon)
  (define-key lispy-mode-map [remap lispy-kill] 'kill-line)
  (define-key lispy-mode-map [remap lispy-tick] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-tilde] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-newline-and-indent-plain] 'newline)
  (define-key lispy-mode-map [remap lispy-quotes] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-yank] 'parinfer-ext:yank)
  (define-key lispy-mode-map [remap lispy-colon] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-hash] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-hat] 'self-insert-command)
  (define-key lispy-mode-map [remap lispy-delete-backward] 'parinfer-ext:backward-delete-char)
  (define-key lispy-mode-map [remap lispy-space] 'parinfer-ext:lispy-space))

(defun parinfer-ext:disable-lispy-mode ()
  (when (bound-and-true-p lispy-mode)
    (lispy-mode -1)))

(parinfer-define-extension lispy
  (add-hook 'parinfer-mode-enable-hook #'parinfer-ext:disable-lispy-mode)
  (add-hook 'parinfer-switch-mode-hook #'parinfer-ext:lispy-switch-mode-behaviour)
  (add-hook 'lispy-mode-hook #'parinfer-ext:lispy-init)
  (parinfer-strategy-add
   'default
   '(parinfer-ext:lispy-parens
     parinfer-ext:lispy-braces
     parinfer-ext:lispy-brackets
     parinfer-ext:lispy-space)))

(provide 'parinfer-ext)
;;; parinfer-ext.el ends here
