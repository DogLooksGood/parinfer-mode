;;; parinfer.el --- Simpler Lisp editing

;; TODO: Info and license


;;; Installation

;; TODO: Docs for usages


;;; Commentary

;; TODO: Docs

;;; Code:

;; Constants


;; -----------------------------------------------------------------------------
;;
;; Custom Variables
;;
;;   The customizable variables.
;;
;; -----------------------------------------------------------------------------

(defvar parinfer--ignore-commands
  '(undo undo-tree-undo undo-tree-redo
         parinfer-shift-left parinfer-shift-right
         clojure-align yank yank-pop indent-region)
  "The commands to ignored by parinfer.")

(defvar parinfer-mode-hook nil
  "Hook runs when parinfer-mode is enabled.")

;; -----------------------------------------------------------------------------
;;
;; Constants
;;
;; -----------------------------------------------------------------------------

(defconst parinfer--lighter "Parinfer"
  "Lighter showed in mode line.")

;; ----- Chars -----

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

;; ----- Errors -----

(defconst parinfer--error-eol-backslash "Line cannot end in a hanging backslash.")

(defconst parinfer--error-quote-danger "Quotes must balanced inside comment blocks.")

(defconst parinfer--error-unclosed-quote "String is missing a closing quote.")

(defconst parinfer--error-unclosed-paren "Unclosed open-paren.")

(defconst parinfer--error-unmatched-close-paren "Unmatched close-paren.")

(defconst parinfer--error-leading-close-paren "Line cannot lead with a close-paren.")

;; -----------------------------------------------------------------------------
;;
;; Internal Variables
;;
;;   Variables used internally by parinfer, should not be known by user.
;;
;; -----------------------------------------------------------------------------


;; ----- Parser States -----

(defvar-local parinfer--in-string nil
  "If we are in string.")

(defvar-local parinfer--quote-danger nil
  "If we are quote danger (unclose string in comment).")

(defvar-local parinfer--in-comment nil
  "If we are in comment.")

;; -----------------------------------------------------------------------------
;;
;; Helpers
;;
;;   Utilities.
;;
;; -----------------------------------------------------------------------------

(defun parinfer-lighter ()
  "Get lighter of `parinfer-mode'."
  "Parinfer")

;; -----------------------------------------------------------------------------
;;
;; Parsers
;;
;;  Parinfer parsers here.
;;
;; -----------------------------------------------------------------------------

(defun parinfer--parse-buffer (&optional buf)
  "Parse buffer to get the parinfer result(contains changes to be applied on buffer later).
If BUF is nil, current buffer will be parsed.")

;; -----------------------------------------------------------------------------
;;
;; Processors
;;
;;   Apply parinfer changes to buffer.
;;
;; -----------------------------------------------------------------------------

(defun parinfer--apply-changes-on-buffer (changes &optional buf)
  "Apply CHANGES on BUF.
If BUF is nil, current buffer will be used.")

;; -----------------------------------------------------------------------------
;;
;; Overlays
;;
;;   Used to show parinfer parse/process on the buffer.
;;
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;;
;; Hooks Setups
;;
;;   Setup parinfer behavior on hooks, so parinfer will know chanegs of buffer.
;;
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;;
;; Commands
;;
;;   The commands that you found in M-x.
;;
;; -----------------------------------------------------------------------------

(defun parinfer-enable ()
  (interactive))

(defun parinfer-disable ()
  (interactive))

;; -----------------------------------------------------------------------------
;;
;; Keymaps
;;
;; -----------------------------------------------------------------------------

(defvar parinfer-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;; -----------------------------------------------------------------------------
;;
;; Modes
;;
;; -----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode parinfer-mode
  "Parinfer mode."
  nil (:eval (pi/lighter)) parinfer-mode-map
  (if parinfer-mode
      (parinfer-enable)
    (parinfer-disable)))

;;; parinfer.el ends here
