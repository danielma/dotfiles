(eval-when-compile
  (require 'use-package))

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

(use-package evil
  :init
  (setq evil-shift-width 2
	evil-shift-round t
	evil-echo-state nil)
  (evil-mode 1)
  :config
  (define-key base-leader-map "w" evil-window-map)
  :bind (
	 ("s-]" . evil-window-next)
	 ("s-[" . evil-window-prev)
	 ("M-s-∆" . evil-window-down)
	 ("M-s-˚" . evil-window-up)
	 ("M-s-˙" . evil-window-left)
	 ("M-s-¬" . evil-window-right)
	 ("s-v" . system-paste)
	 ("s-c" . system-yank)
	 :map evil-insert-state-map
	 ("C-n" . next-line)
	 ("C-p" . previous-line)
	 ("C-a" . beginning-of-line-text)
	 ("C-e" . end-of-line)
	 ("M-RET" . expand-at-point)
	 :map evil-normal-state-map
	 ("M-RET" . newline)
	 ("[b" . previous-buffer)
	 ("]b" . next-buffer)
	 ("C-." . helm-M-x)
	 :map evil-visual-state-map
	 ("C-w" . interactive-wrap-with-pair)
	 :map evil-window-map
	 ("]" . buf-move-right)
	 ("[" . buf-move-left)
	 ("}" . buf-move-down)
	 ("{" . buf-move-up)
	 ))

(use-package evil-multiedit
  :bind (:map evil-visual-state-map
	 ("R" . evil-multiedit-match-all)
	 ("M-d" . evil-multiedit-match-and-next)
	 ("M-D" . evil-multiedit-match-and-previous)
	 :map evil-normal-state-map
	 ("M-d" . evil-multiedit-match-and-next)
	 ("M-D" . evil-multiedit-match-and-previous)
	 ("C-M-D" . evil-multiedit-restore)
	 :map evil-multiedit-state-map
	 ("C-n" . evil-multiedit-next)
	 ("C-p" . evil-multiedit-prev)
	 :map evil-multiedit-insert-state-map
	 ("C-n" . evil-multiedit-next)
	 ("C-p" . evil-multiedit-prev))
	;; ("RET" . evil-multiedit-toggle-or-restrict-region)
	;; :map evil-motion-state-map
	;; ("RET" . evil-multiedit-toggle-or-restrict-region)
  )


(provide 'dm-evil)
