;;; parinfer-theme.el --- prettify parens for parinfer-mode
;; gray close parens for parinfer-mode indent style.

;;; Code

(defgroup parinfer-face
  nil
  "Parinfer face group"
  :group 'faces)

(defface parinfer-dim-paren-face
   '((((class color) (background dark))
      (:foreground "grey40"))
     (((class color) (background light))
      (:foreground "grey60")))
   "Parinfer dim paren face."
   :group 'parinfer-face)

(defun parinfer--enable-dim-parens ()
  (font-lock-add-keywords nil
                          '((")\\|}\\|]" . 'parinfer-dim-paren-face)))
  (font-lock-flush))

(defun parinfer--disable-dim-parens ()
  (font-lock-remove-keywords nil
                          '((")\\|}\\|]" . 'parinfer-dim-paren-face)))
  (font-lock-flush))

(provide 'parinfer-theme)
;;; parinfer-theme.el ends here
