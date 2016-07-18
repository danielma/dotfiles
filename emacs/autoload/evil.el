;;; evil.el --- setup for evil mode

;;; Commentary:

;;; Code:

(require 'evil)
(global-evil-leader-mode)
(evil-mode 1)

;; (fset 'evil-visual-update-x-selection 'ignore)
(setq evil-shift-width 2)
      ;; x-select-enable-clipboard nil)
(setq-default indent-tabs-mode nil)
(define-key evil-normal-state-map "[b" 'previous-buffer)
(define-key evil-normal-state-map "]b" 'next-buffer)

;; (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
;; (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
;; (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;; (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)

(defun save-buffer-always ()
  "Save this buffer even if it hasn't been modieifed."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun custom-comment-line ()
  "Comment lines the way I want to."
  (interactive)
  (if (evil-visual-state-p)
      (call-interactively 'comment-or-uncomment-region)
      (call-interactively 'toggle-comment-on-line)))

(defun replace-symbol ()
  "EVIL: search for instances of the symbol under the cursor."
  (interactive)
  (let (from to sym)
    (if (use-region-p)
        (progn
          (setq sym (buffer-substring-no-properties (mark) (point))))
        (progn
          (save-excursion
            (skip-syntax-backward "w_") (setq from (point)))
          (save-excursion
            (skip-syntax-forward "w_") (setq to (point)))
          (setq sym (buffer-substring-no-properties to from))))
    (evil-ex (concat "%s/" sym "/"))))

(defun find-symbol-in-project ()
  "Search for symbol in project using projectile-ag"
  (interactive)
  (let (from to sym)
    (if (use-region-p)
        (progn
          (setq sym (buffer-substring-no-properties (mark) (point))))
      (progn
        (save-excursion
          (skip-syntax-backward "w_") (setq from (point)))
        (save-excursion
          (skip-syntax-forward "w_") (setq to (point)))
        (setq sym (buffer-substring-no-properties to from))))
    (helm-ag-project-root sym)))

(defun custom-flycheck-toggle-errors ()
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (progn
        (delete-window (get-buffer-window (get-buffer "*Flycheck errors*")))
        (kill-buffer "*Flycheck errors*"))
    (flycheck-list-errors)))

(defun edit-emacs ()
  (interactive)
  (find-file "~/.dotfiles/emacs/emacs.symlink"))

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "fs" 'save-buffer-always
  "fq" 'delete-window
  "fl" 'sr-speedbar-toggle

  "bd" 'kill-this-buffer
  "bs" 'switch-to-buffer
  "bl" 'buf-move-right
  "bh" 'buf-move-left
  "bj" 'buf-move-down
  "bk" 'buf-move-up

  "cd" 'cd
  "cl" 'custom-comment-line

  "ee" 'edit-emacs

  "gs" 'magit-status
  "gc" 'magit-commit
  "gt" 'git-timemachine-toggle
  "gb" 'magit-blame

  "hd" 'helm-dash
  "hD" 'helm-dash-at-point
  "hr" 'helm-resume

  "ll" 'custom-flycheck-toggle-errors
  "ln" 'flycheck-next-error
  "lp" 'flycheck-previous-error

  "mw" 'web-mode
  "mx" 'js2-jsx-mode
  "mj" 'js-mode

  "p" 'projectile-command-map

  "ss" 'evil-search-word-forward
  "sr" 'replace-symbol

  "tn" 'elscreen-create
  "tl" 'elscreen-next
  "th" 'elscreen-previous
  "tq" 'elscreen-kill
  "tj" 'elscreen-select-and-goto

  "w" 'evil-window-map

  "," 'ace-jump-mode

  "<SPC>" 'helm-M-x)
