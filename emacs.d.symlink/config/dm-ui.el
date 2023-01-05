;;; dm-ui.el --- UI -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun global-text-scale-adjust-by-two ()
  (interactive)
  (global-text-scale-adjust 2))

(use-package emacs
  :demand t
  :custom
  (tab-width 2)
  (display-buffer-alist '(
                          ((major-mode . magit-status-mode) . (display-buffer-same-window))
                          ((derived-mode . magit-mode) . nil)
                          (t . (display-buffer-same-window))))
  (indent-tabs-mode nil)
  :bind (:map global-map
              ("s-=" . global-text-scale-adjust-by-two)
              ("s--" . global-text-scale-adjust-by-two)
              :map minibuffer-mode-map
              ("C-k" . kill-whole-line)))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :hook
  (prog-mode . display-line-numbers-mode)
  )

(use-package whitespace
  :delight
  :custom
  (fill-column 100)
  (whitespace-line-column fill-column)
  (whitespace-style '(face lines-tail tabs tab-mark))
  :hook
  (prog-mode . whitespace-mode))

;;; Consult

(use-package consult
  :bind (:map global-map
	            ("M-g i" . consult-imenu)
	            ("C-x b" . consult-buffer))
  :config
  (defun consult-symbol-at-point ()
    "Search for the matching `symbol-at-point`."
    (interactive)
    (let ((sym (thing-at-point 'symbol)))
      (consult-line sym))))

(use-package consult-flycheck
  :after consult)

;;; End consult

(use-package xref
  :custom
  (xref-show-definitions-function 'consult-xref))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package which-key
  :delight
  :config
  (which-key-mode))

(use-package dired-sidebar
  :bind (("C-s-s" . dired-sidebar-toggle-sidebar)))

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
