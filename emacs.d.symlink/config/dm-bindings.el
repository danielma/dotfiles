;;; dm-bindings.el --- Global bindings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun dm-yank-with-clipboard ()
  "Yank from the system clipboard."
  (interactive)
  (let ((select-enable-clipboard t))
    (yank)))

(defun tt/expand-at-point ()
  "Insert two indented newlines and leave point between them."
  (interactive)
  (newline-and-indent)
  (indent-according-to-mode)
  (newline-and-indent)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun dm-save-buffer-always (&rest _args)
  "Mark the current buffer modified so `save-buffer' always writes it."
  (set-buffer-modified-p t))

(advice-add 'save-buffer :before #'dm-save-buffer-always)

(use-package avy
  :bind (("C-c j" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-c j" . avy-isearch))
  :config
  (defun dm-avy-action-embark (point)
    "Run `embark-act' at the Avy candidate at POINT."
    (unwind-protect
        (save-excursion
          (goto-char point)
          (embark-act))
      (select-window (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) #'dm-avy-action-embark))

(use-package embark
  :demand t
  :bind ("C-c a" . embark-act)
  :custom
  (embark-auto-prefix-help-delay 1.0)
  :config
  (when (bound-and-true-p which-key-mode)
    (which-key-mode -1))
  (embark-auto-prefix-help-mode))

;; To restore Which Key as the prefix helper, uncomment this block.
;; (use-package which-key
;;   :config
;;   (embark-auto-prefix-help-mode -1)
;;   (which-key-mode))

(use-package emacs
  :bind (
	       ("C-x C-:" . comment-line)
	       :map global-map
         ("M-RET" . tt/expand-at-point)
	       ;; ("C-:" . execute-extended-command)
	       ("C-." . execute-extended-command)
         ("C-h C-f" . find-function)
         ("s-}" . tab-bar-switch-to-next-tab)
         ("s-{" . tab-bar-switch-to-prev-tab)
         ("s-[" . previous-buffer)
         ("s-]" . next-buffer)
         ("s-v" . dm-yank-with-clipboard)
         ("C-c o" . browse-url)
         :map window-prefix-map
         ("=" . balance-windows)
         ("j" . windmove-down)
         ("h" . windmove-left)
         ("k" . windmove-up)
         ("l" . windmove-right)
	       )
  :custom (select-enable-clipboard . nil))

(defun dm-with-select-clipboard (orig-fun &rest args)
  "Execute the ORIG-FUN with ARGS with `select-enable-clipboard' enabled."
  (let ((select-enable-clipboard t))
    (apply orig-fun args)))

(advice-add 'ns-copy-including-secondary :around #'dm-with-select-clipboard)

(provide 'dm-bindings)
;;; dm-bindings.el ends here
