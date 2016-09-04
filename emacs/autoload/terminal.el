;;; terminal.el -- helpers for terminal emacs

;;; Commentary:

;;; Code:

(define-key global-map (kbd "C-c (")
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'save-buffer-always)
    map))

; nope
