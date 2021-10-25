;;; dm-ui.el --- UI
;;; Commentary:

;;; Code:

(use-package paren
  :init
  (show-paren-mode t))

(use-package whitespace
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face lines-tail))
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package nlinum
  :config
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (setq nlinum-format " %d"))
  
(use-package nlinum-relative
    :commands (nlinum-relative-on)
    :custom
    (nlinum-relative-current-symbol "")
    (nlinum-relative-redisplay-delay 0.1)
    :init
    (add-hook 'nlinum-mode-hook 'nlinum-relative-on)
    :config
    (nlinum-relative-setup-evil))

(use-package origami
  :config 
  (global-origami-mode))

(use-package buffer-move
  :commands (buf-move buf-move-right buf-move-left buf-move-up buf-move-down)
  )

(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  )

(use-package zoom-frm
  :config
  (setq frame-inhibit-implied-resize t)
  (face-spec-recalc 'default (selected-frame))
  :bind
  ("s-=" . zoom-all-frames-in)
  ("s--" . zoom-all-frames-out)
  ("s-0" . zoom-frm-unzoom))


(provide 'dm-ui)
;;; dm-ui.el ends here
