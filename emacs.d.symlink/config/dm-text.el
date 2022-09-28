;;; dm-text.el --- Special tools for me to manipulate text -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package s)

(defvar
  break-object-trailing-commas
  t
  "Whether break-object should insert trailing commas.")

(defun tt/-break-object ()
  "Breaks an object when point is inside the block."
  (newline-and-indent)

  (while (re-search-forward "\\[\\|{\\|(\\|,\\|\\]\\|}\\|)" (line-end-position) t)
    (let ((string-before (char-to-string (char-before))))
      (cond ((s-contains? string-before "[{(") (tt/-break-object))
            ((s-contains? string-before "]})") (tt/-break-object-end))
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
        (re-search-forward "\\]\\|}\\|)" (line-end-position))
        (backward-char)))

  (newline-and-indent)
  (forward-char)
  )

(defun tt/break-object ()
  "Breaks objects onto multiple lines."
  (interactive)

  ;; go to the beginning of this object
  (re-search-backward "\\[\\|{\\|(" (line-beginning-position))
  (forward-char)
  (tt/-break-object)
  )

(defun tt/expand-at-point ()
  "Insert a newline and put the cursor at the indented location above."
  (interactive)
  (newline-and-indent)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defvar
  dm-text-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'tt/break-object)
    map)
  "The map for my text tools.")

(use-package abbrev
  :delight "\uf475"
  :straight nil
  :config
  (define-abbrev
    global-abbrev-table "orgn" "organization")
  (define-abbrev
    global-abbrev-table "Scoto" "ScopedToOrganization")
  :custom
  (abbrev-mode t)
  (save-abbrevs nil)
  )

(provide 'dm-text)
;;; dm-text.el ends here
