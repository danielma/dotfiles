;;; chord-pro-mode --- A super basic mode for editing chord pro files
;
;;; Commentary:
;
; A super basic mode for editing chord pro files
;
;;; Code:

(defun cp/capitalize (s)
  "Convert the first word's first character to upper case and the rest to lower case in S."
  (concat (upcase (substring s 0 1)) (substring s 1)))

(defun insert-chord (chord)
  "Insert CHORD at the current insertion point."
  (interactive "sChord: ")
  (insert "[" (cp/capitalize chord) "]"))

(defvar chord-pro-mode-syntax-table
  (let ((st (make-syntax-table)))
    st)
  "Syntax table for `chord-pro-mode'.")

(defvar chord-pro-font-lock-keywords
  '(
    ("\\[[a-zA-Z0-9#/]+\\]" . font-lock-keyword-face)
    )
   "Keyword highlighting specification for `chord-pro-mode'.")

(defvar chord-pro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-'") 'insert-chord)
    map))

(define-derived-mode chord-pro-mode text-mode "Chord-Pro"
  "Major mode for PCO Chords."
  :syntax-table chord-pro-mode-syntax-table
  (setq-local font-lock-defaults
              '(chord-pro-font-lock-keywords))
  )

(provide 'chord-pro-mode)
;;; chord-pro.el ends here
