;;; dm-ui.el --- UI -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun global-text-scale-adjust-by-two ()
  (interactive)
  (global-text-scale-adjust 2))

(defun dm-enable-xterm-mouse-mode (&optional frame)
  "Enable `xterm-mouse-mode' for terminal FRAMEs."
  (when-let ((frame (or frame (selected-frame))))
    (unless (display-graphic-p frame)
      (with-selected-frame frame
        (xterm-mouse-mode 1)))))

(defvar dm-default-interprogram-cut-function interprogram-cut-function
  "Clipboard backend to use when terminal clipboard integration is not needed.")

(defvar dm-default-interprogram-paste-function interprogram-paste-function
  "Paste backend to use when terminal clipboard integration is not needed.")

(defun dm-terminal-clipboard-supported-p (&optional frame)
  "Return non-nil when FRAME should use the macOS terminal clipboard backend."
  (and (eq system-type 'darwin)
       (not (display-graphic-p frame))
       (executable-find "pbcopy")
       (executable-find "pbpaste")))

(defun dm-interprogram-cut-function (text &optional push)
  "Copy TEXT to the appropriate clipboard backend.
PUSH is forwarded to the default backend when terminal integration is inactive."
  (if (dm-terminal-clipboard-supported-p)
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "pbcopy"))
    (when dm-default-interprogram-cut-function
      (funcall dm-default-interprogram-cut-function text push))))

(defun dm-interprogram-paste-function ()
  "Read text from the appropriate clipboard backend."
  (if (dm-terminal-clipboard-supported-p)
      (with-temp-buffer
        (call-process "pbpaste" nil t nil)
        (buffer-string))
    (when dm-default-interprogram-paste-function
      (funcall dm-default-interprogram-paste-function))))

(use-package emacs
  :demand t
  :custom
  (tab-width 2)
  (display-buffer-alist '(
                          ((major-mode . magit-status-mode) . (display-buffer-same-window))
                          ((derived-mode . magit-mode) . nil)
                          (t . (display-buffer-same-window))))
  (indent-tabs-mode nil)
  (compilation-scroll-output t)
  (interprogram-cut-function #'dm-interprogram-cut-function)
  (interprogram-paste-function #'dm-interprogram-paste-function)
  :bind (:map global-map
              ("s-=" . global-text-scale-adjust-by-two)
              ("s--" . global-text-scale-adjust-by-two)
              :map minibuffer-mode-map
              ("C-k" . kill-whole-line)))

(use-package display-line-numbers
  ;; :custom
  ;; (display-line-numbers-type 'relative)
  :hook text-mode prog-mode)

(use-package whitespace
  :delight
  :custom
  (fill-column 100)
  (whitespace-line-column fill-column)
  (whitespace-style '(face lines-tail tabs tab-mark))
  :hook
  (prog-mode . whitespace-mode))

(use-package xt-mouse
  :config
  (dm-enable-xterm-mouse-mode)
  (add-hook 'after-make-frame-functions #'dm-enable-xterm-mouse-mode))

;;; Consult

(use-package consult
  :bind (:map global-map
	            ("M-g i" . consult-imenu)
	            ("C-x C-b" . consult-buffer))
  :config
  (defun consult-symbol-at-point ()
    "Search for the matching `symbol-at-point`."
    (interactive)
    (let ((sym (thing-at-point 'symbol)))
      (consult-line sym))))

;;; End consult

(use-package xref
  :custom
  (xref-show-definitions-function 'consult-xref)
  (xref-show-xrefs-function 'consult-xref))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package which-key
  :delight
  :config
  (which-key-mode))

;; (use-package dired-sidebar
;;   :bind (("C-s-s" . dired-sidebar-toggle-sidebar)))

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
