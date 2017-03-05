;;; text-tools --- Special tools for me to manipulate text

;;; Commentary:

;;; Code:
(defvar
  break-object-trailing-commas
  t
  "Whether break-object should insert trailing commas.")

(defun tt/-break-object ()
  "Breaks an object when point is inside the block."
  (newline-and-indent)

  (while (re-search-forward "{\\|(\\|,\\|}\\|)" (line-end-position) t)
    (let ((string-before (char-to-string (char-before))))
      (cond ((s-contains? string-before "{(") (tt/-break-object))
            ((s-contains? string-before "})") (tt/-break-object-end))
            (t (newline-and-indent))))))

(defun tt/-break-object-end ()
  "Deals with breaking an object at the end of the object."
  (backward-char)

  ;; trailing comma
  (if (and break-object-trailing-commas
           (re-search-backward "[^[:space:]]" (line-beginning-position) t))
      (progn
        (forward-char)
        (insert-char ?,)
        (re-search-forward "}\\|)" (line-end-position))
        (backward-char)))

  (newline-and-indent)
  (forward-char)
  )

(defun tt/break-object ()
  "Breaks objects onto multiple lines."
  (interactive)

  ;; go to the beginning of this object
  (re-search-backward "{\\|(" (line-beginning-position))
  (forward-char)
  (tt/-break-object)
  )

(setq text-tools-map
      (let ((map (make-sparse-keymap)))
        (define-key map "b" 'tt/break-object)
        map))

(provide 'text-tools)
;;; text-tools.el ends here
