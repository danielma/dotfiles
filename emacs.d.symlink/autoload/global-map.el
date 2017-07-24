(define-key global-map (kbd "s-=") 'zoom-frm-in)
(define-key global-map (kbd "s--") 'zoom-frm-out)
(define-key global-map (kbd "s-0") 'zoom-frm-unzoom)
(define-key global-map (kbd "s-t") 'elscreen-create)
(define-key global-map (kbd "s-<return>") 'toggle-frame-fullscreen)
(define-key global-map (kbd "s-{") 'elscreen-previous)
(define-key global-map (kbd "s-}") 'elscreen-next)
(define-key global-map (kbd "s-]") 'evil-window-next)
(define-key global-map (kbd "s-[") 'evil-window-prev)
(define-key global-map (kbd "s-s") 'save-buffer-always)
(define-key global-map (kbd "s-w") 'elscreen-kill)
(define-key global-map (kbd "RET") 'newline-and-indent)

(define-key global-map (kbd "M-s-∆") 'evil-window-down)
(define-key global-map (kbd "M-s-˚") 'evil-window-up)
(define-key global-map (kbd "M-s-˙") 'evil-window-left)
(define-key global-map (kbd "M-s-¬") 'evil-window-right)

(define-key global-map (kbd "C-h C-f") 'find-function)
(define-key global-map (kbd "C-.") 'helm-M-x)
(define-key global-map (kbd "C-'") 'company-yasnippet)

(defun system-paste ()
  "Always paste from the system clipboard."
  (interactive)
  (evil-paste-before 1 ?+)
  (forward-char))

(defun system-yank ()
  "Always yank from the system clipboard."
  (interactive)
  (apply 'evil-yank (append (evil-operator-range t) '(?+)))
  (evil-normal-state))

(global-set-key (kbd "s-v") 'system-paste)
(global-set-key (kbd "s-c") 'system-yank)

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
