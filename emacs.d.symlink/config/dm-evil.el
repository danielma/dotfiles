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

(defun my/window-x-sibling-p ()
  (let ((w (get-buffer-window)))
    (or (windmove-find-other-window 'right) (windmove-find-other-window 'left))))

(defun my/window-percent (percent)
  (interactive "N")
  (let ((p (* percent 0.1)))
    (if (my/window-x-sibling-p)
	(evil-resize-window (round (* (frame-width) p)) t)
      (evil-resize-window (round (* (frame-height) p))))))

(use-package ace-window)

(use-package evil
  :init
  (setq evil-shift-width 2
	evil-shift-round t
	evil-echo-state nil
	windmove-wrap-around t)
  (evil-mode 1)
  :config
  (define-key base-leader-map "w" evil-window-map)
  (defhydra hydra-evil-window ()
    "window"
    ("h" evil-window-left             "left")
    ("j" evil-window-down             "down")
    ("k" evil-window-up               "up")
    ("l" evil-window-right            "right")

    ("w" ace-window                   "ace-window")

    ;; movement
    ("H" evil-window-move-far-left    "far left")
    ("J" evil-window-move-very-bottom "far bottom")
    ("K" evil-window-move-very-top    "far top")
    ("L" evil-window-move-far-right   "far right")
    ("C-h" buf-move-left              "move left")
    ("C-j" buf-move-down              "move down")
    ("C-k" buf-move-up                "move up")
    ("C-l" buf-move-right             "move right")

    ;; size
    ("-" evil-window-decrease-height "height -")
    ("+" evil-window-increase-height "height +")
    ("<" evil-window-decrease-width  "width -")
    (">" evil-window-increase-width  "width +")
    ("p" my/window-percent "percent")

    ("_" evil-window-set-height      "height full")
    ("|" evil-window-set-width       "width full")
    ("=" balance-windows             "balance")
    )
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
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
         ("C-d" . delete-forward-char)
         ("M-RET" . expand-at-point)
         :map evil-normal-state-map
         ("M-RET" . newline)
         ("[b" . previous-buffer)
         ("]b" . next-buffer)
         ("C-." . helm-M-x)
         ("j" . evil-next-visual-line)
         ("k" . evil-previous-visual-line)
         ("'" . evil-repeat-find-char)
         :map evil-visual-state-map
         ("C-w" . interactive-wrap-with-pair)
         :map evil-window-map
         ("C-h" . buf-move-left)
         ("C-j" . buf-move-down)
         ("C-k" . buf-move-up)
         ("C-l" . buf-move-right)
         ("p" . my/window-percent)
         ("w" . ace-window)
         ("." . hydra-evil-window/body)
         :map base-leader-map
         ("," . evil-avy-goto-char)
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

(use-package evil-matchit
  :init
  (global-evil-matchit-mode))

(defun my/prog-mode-setup ()
  "Setup prog mode."
  (setq-local evil-shift-width 2))

(add-hook 'prog-mode-hook 'my/prog-mode-setup)

(provide 'dm-evil)
