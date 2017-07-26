(eval-when-compile
  (require 'use-packge))

(use-package evil
  :init
  (setq evil-shift-width 2
	evil-shift-round t)
  :config
  (evil-mode 1)
  :bind (:map evil-insert-state-map
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

(provide 'dm-evil)
