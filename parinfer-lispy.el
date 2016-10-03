;;; parinfer-lispy.el --- Integration for Parinfer and Lispy

;;; Code

;; -----------------------------------------------------------------------------
;; Fixed commands
;; -----------------------------------------------------------------------------

(defun parinfer-lispy-space ()
  (interactive)
  (call-interactively 'self-insert-command)
  (when (parinfer--lispy-left-between-parens-p)
    (backward-char)))

(defun parinfer-lispy-forward ()
  (interactive)
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'lispy-forward))

(defun parinfer-lispy-backward ()
  (interactive)
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'lispy-backward))

(defun parinfer--lispy-paren-char-p (c)
  (or (eq c 40)
      (eq c 91)
      (eq c 123)))

(defun parinfer--lispy-left-between-parens-p ()
  (unless (or (eq (point) (line-beginning-position))
              (eq (point) (line-end-position)))
    (let ((ca (char-after))
          (cb (char-before)))
      (message "%s,%s" (parinfer--lispy-paren-char-p ca)
               (parinfer--lispy-paren-char-p cb))
      (and (not (parinfer--in-comment-or-string-p))
           (parinfer--lispy-paren-char-p ca)
           (parinfer--lispy-paren-char-p cb)))))

(defun parinfer-lispy-parens ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'lispy-parens)
    (call-interactively 'self-insert-command)))

(defun parinfer-lispy-brackets ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'lispy-brackets)
    (call-interactively 'self-insert-command)))

(defun parinfer-lispy-braces ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'lispy-braces)
    (call-interactively 'self-insert-command)))

(defun parinfer--lispy-switch-mode-behaviour (mode)
  (if (eq mode 'indent)
      (lispy-mode 1)
    (lispy-mode -1)))

(defun parinfer--lispy-init ()
  (define-key lispy-mode-map (kbd "(") 'parinfer-lispy-parens)
  (define-key lispy-mode-map (kbd ")") 'self-insert-command)
  (define-key lispy-mode-map (kbd "[") 'parinfer-lispy-brackets)
  (define-key lispy-mode-map (kbd "]") 'self-insert-command)
  (define-key lispy-mode-map (kbd "{") 'parinfer-lispy-braces)
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
  (define-key lispy-mode-map [remap lispy-space] 'parinfer-lispy-space))

(defun parinfer--lispy-disable-if-enabled ()
  (when (bound-and-true-p lispy-mode)
    (lispy-mode -1)))

;; -----------------------------------------------------------------------------
;; Public APIs
;; -----------------------------------------------------------------------------

(defun parinfer-lispy-rocks ()
  (add-hook 'parinfer-mode-enable-hook #'parinfer--lispy-disable-if-enabled)
  (add-hook 'parinfer-switch-mode-hook #'parinfer--lispy-switch-mode-behaviour)
  (add-hook 'lispy-mode-hook #'parinfer--lispy-init)
  (parinfer-strategy-add 'default '(parinfer-lispy-parens
                                    parinfer-lispy-braces
                                    parinfer-lispy-brackets
                                    parinfer-lispy-space)))

(provide 'parinfer-lispy)
;;; parinfer-lispy.el ends here
