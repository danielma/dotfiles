(defun switch-to-term ()
  (interactive)
  (if (string-equal (buffer-name) "vterm")
      (previous-buffer)
    (if (get-buffer "vterm")
	(switch-to-buffer "vterm")
      (call-interactively 'vterm))))

(define-key global-map (kbd "s-<return>") 'toggle-frame-fullscreen)
(define-key global-map (kbd "s-s") 'save-buffer-always)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; iterm command-s
(define-key global-map (kbd "<f5>") 'save-buffer-always)

(define-key global-map (kbd "C-h C-f") 'find-function)
(define-key global-map (kbd "C-'") 'company-yasnippet)
(define-key global-map (kbd "s-T") 'switch-to-term)
(define-key global-map (kbd "s-O") 'browse-url)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

(defun chunkwm/move (dirstring)
  "Move to DIRSTRING with chunkwm integration."
  (let ((dir (pcase dirstring
               ("north" 'above)
               ("east" 'right)
               ("south" 'below)
               ("west" 'left))))
    (if (window-in-direction dir)
        (and (windmove-do-window-select dir) "0")
      '1)))

(provide 'global-map)
