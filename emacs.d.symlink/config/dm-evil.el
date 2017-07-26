(eval-when-compile
  (require 'use-package))

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

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "fs" 'save-buffer-always
    "fq" 'delete-window
    "fr" 'force-reload

    "bd" 'kill-this-buffer
    "bs" 'helm-buffers-list

    "cd" 'cd
    "cl" 'custom-comment-line
    "ct" 'my/base16-set-theme
    "cf" 'my/set-custom-face

    "dr" 'reveal-in-finder

    "ee" 'edit-emacs
    "es" 'edit-scratch
    "ey" 'edit-yasnippet-dir

    "gB" 'browse-at-remote

    "hr" 'helm-resume
    "hk" 'helm-show-kill-ring

    "ll" 'custom-flycheck-toggle-errors
    "ln" 'flycheck-next-error
    "lp" 'flycheck-previous-error

    ;; "mw" 'web-mode
    ;; "mj" 'js-mode

    "p" 'projectile-command-map

    "ss" 'evil-search-word-forward
    "sr" 'replace-symbol
    "sa" 'find-symbol-in-project

    "T" text-tools-map

    "w" evil-window-map

    "," 'ace-jump-char-mode

    "<SPC>" 'helm-M-x))

(provide 'dm-evil)
