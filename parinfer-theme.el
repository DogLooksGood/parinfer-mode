;;; parinfer-theme.el --- prettify parens for parinfer-mode
;; gray close parens for parinfer-mode indent style.

;;; Code

(defgroup parinfer-group
  nil
  "Parinfer customize group."
  :group 'faces)

(defface parinfer-dim-paren-face
   '((((class color) (background dark))
      (:foreground "grey40"))
     (((class color) (background light))
      (:foreground "grey60")))
   "Parinfer dim paren face."
   :group 'parinfer-group)

(defun parinfer--set-dim-parens (mode)
  "Set dim close parens, which is depended MODE."
  (cl-case mode
    (paren (font-lock-remove-keywords
            nil '((")\\|}\\|]" . 'parinfer-dim-paren-face))))
    (indent (font-lock-add-keywords
             nil '((")\\|}\\|]" . 'parinfer-dim-paren-face)))))
  (font-lock-flush))

(provide 'parinfer-theme)
;;; parinfer-theme.el ends here
