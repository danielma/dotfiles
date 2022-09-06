;;; dm-ui.el --- UI
;;; Commentary:

;;; Code:

(use-package emacs
  :demand t
  :bind (:map global-map
	      ("s-=" . global-text-scale-adjust)))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :hook
  (prog-mode . display-line-numbers-mode)
  )

(use-package whitespace
  :custom
  (fill-column 100)
  (whitespace-line-column fill-column)
  (whitespace-style '(face lines-tail))
  :hook
  (prog-mode . whitespace-mode))


(use-package which-key
  :config
  (which-key-mode))

(use-package origami
  :disabled
  :config
  (global-origami-mode))

(use-package rainbow-delimiters
  :disabled
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  )

(provide 'dm-ui)
;;; dm-ui.el ends here
