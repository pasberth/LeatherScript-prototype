(eval-when-compile (require 'cl))

(defvar lthstx-mode-map
  (make-sparse-keymap)
  "Keymap used in Leather Syntax mode.")

(defconst lthstx-font-lock-keywords
  (list
    (concat
       (regexp-opt
         '( "Token"
	    "Notation"
	    ":="
	    "level"
	    "no"
            "left"
            "right"
            "associativity")
         t))))
;;;###autoload
(defun lthstx-mode ()
  "lthstx mode"
  (interactive)
  (kill-all-local-variables)
  (use-local-map lthstx-mode-map)
  (set (make-local-variable 'font-lock-defaults)
          '((lthstx-font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-keywords)
        lthstx-font-lock-keywords)
  (setq mode-name "Leather Syntax"))
;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.lthstx\\'") 'lthstx-mode))

(provide 'lthstx-mode)

;;; lthstx-mode.el ends here
