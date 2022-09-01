;;; dm-ui.el --- UI
;;; Commentary:

;;; Code:

(use-package paren
  :init
  (show-paren-mode t))

(use-package whitespace
  :custom
  (fill-column 100)
  (whitespace-line-column fill-column)
  (whitespace-style '(face lines-tail))
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :hook
  (prog-mode . display-line-numbers-mode)
  )

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

(use-package which-key
  :config
  (which-key-mode))

(provide 'dm-ui)
;;; dm-ui.el ends here
