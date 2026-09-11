;;; dm-ui.el --- UI -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Defer loading the symbol catalog until one of its commands is used.
(autoload 'sf-symbol-insert "sf" "Insert an SF Symbol." t)
(autoload 'sf-symbol-insert-name "sf" "Insert an SF Symbol name." t)

(defun global-text-scale-adjust-by-two ()
  (interactive)
  (global-text-scale-adjust 2))

(defun dm-enable-xterm-mouse-mode (&optional frame)
  "Enable `xterm-mouse-mode' for terminal FRAMEs."
  (when-let* ((frame (or frame (selected-frame))))
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
      (condition-case nil
          (funcall dm-default-interprogram-cut-function text push)
        (wrong-number-of-arguments
         (funcall dm-default-interprogram-cut-function text))))))

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
  (global-hl-line-sticky-flag 'window)
  (ring-bell-function #'ignore)
  (interprogram-cut-function #'dm-interprogram-cut-function)
  (interprogram-paste-function #'dm-interprogram-paste-function)
  :hook
  (text-mode . visual-line-mode)
  :config
  (global-hl-line-mode)
  :bind (:map global-map
              ("s-=" . global-text-scale-adjust-by-two)
              ("s--" . global-text-scale-adjust-by-two)
              :map minibuffer-mode-map
              ("C-k" . kill-whole-line)))

(use-package display-line-numbers
  :custom
  (display-line-numbers-width 3)
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

(use-package vertico
  :init
  (vertico-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  :bind (:map global-map
              ("M-R" . vertico-repeat)))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((embark-keybinding (styles basic))
     (file (styles basic partial-completion)))))

(use-package corfu
  :init
  (completion-preview-mode -1)
  (global-corfu-mode)
  :bind
  (:map global-map
        ("C-'" . completion-at-point)
        :map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Consult

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g o" . consult-outline)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :custom
  (consult-narrow-key "<")
  (consult-line-start-from-top t)
  :config
  (defun consult-symbol-at-point ()
    "Search for the matching `symbol-at-point`."
    (interactive)
    (let ((sym (thing-at-point 'symbol)))
      (consult-line sym))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; End consult

(use-package xref
  :custom
  (xref-show-definitions-function 'consult-xref)
  (xref-show-xrefs-function 'consult-xref))

(use-package marginalia
  :config
  (marginalia-mode 1))

;; (use-package dired-sidebar
;;   :bind (("C-s-s" . dired-sidebar-toggle-sidebar)))

(use-package origami
  :disabled
  :config
  (global-origami-mode))

(provide 'dm-ui)
;;; dm-ui.el ends here
