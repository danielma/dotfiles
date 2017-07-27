(define-key global-map (kbd "s-<return>") 'toggle-frame-fullscreen)
(define-key global-map (kbd "s-s") 'save-buffer-always)
(define-key global-map (kbd "RET") 'newline-and-indent)

(define-key global-map (kbd "C-h C-f") 'find-function)
(define-key global-map (kbd "C-'") 'company-yasnippet)

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

(provide 'global-map)
