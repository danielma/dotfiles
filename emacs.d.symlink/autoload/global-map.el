(define-key global-map (kbd "s-=") 'zoom-frm-in)
(define-key global-map (kbd "s--") 'zoom-frm-out)
(define-key global-map (kbd "s-0") 'zoom-frm-unzoom)
(define-key global-map (kbd "s-<return>") 'toggle-frame-fullscreen)
(define-key global-map (kbd "s-s") 'save-buffer-always)
(define-key global-map (kbd "RET") 'newline-and-indent)

(define-key global-map (kbd "C-h C-f") 'find-function)
(define-key global-map (kbd "C-.") 'helm-M-x)
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
