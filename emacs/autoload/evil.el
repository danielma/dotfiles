;;; evil.el --- setup for evil mode

;;; Commentary:

;;; Code:

(global-evil-leader-mode)

;; (fset 'evil-visual-update-x-selection 'ignore)
(setq evil-shift-width 2)
      ;; x-select-enable-clipboard nil)
(setq-default indent-tabs-mode nil)

(defun interactive-wrap-with-pair (pair)
  (interactive "c")
  (sp-wrap-with-pair (char-to-string pair)))

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
;; (key-chord-define evil-insert-state-map "fs" 'save-buffer-always)
(key-chord-define evil-insert-state-map ",," 'ace-jump-mode)
(key-chord-define evil-normal-state-map "''" 'helm-M-x)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-normal-state-map (kbd "M-RET") 'newline)
(define-key evil-normal-state-map "[b" 'previous-buffer)
(define-key evil-normal-state-map "]b" 'next-buffer)
(define-key evil-visual-state-map (kbd "C-w") 'interactive-wrap-with-pair)

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
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun custom-comment-line ()
  "Comment lines the way I want to."
  (interactive)
  (if (evil-visual-state-p)
      (call-interactively 'comment-or-uncomment-region)
      (call-interactively 'toggle-comment-on-line)))

(defun current-symbol-or-region ()
  "Return the symbol under the cursor or the selected region."
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
    sym))

(defun replace-symbol ()
  "EVIL: search for instances of the symbol under the cursor."
  (interactive)
  (evil-ex (concat "%s/" (current-symbol-or-region) "/")))

;; TODO this would be nice
(defun find-symbol-in-project ()
  "Search for symbol in project using projectile-ag."
  (interactive)
  (helm-ag-project-root (current-symbol-or-region)))

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

  "cd" 'cd
  "cl" 'custom-comment-line

  "ee" 'edit-emacs

  "gs" 'magit-status
  "gc" 'magit-commit
  "gb" 'magit-blame
  "gB" 'browse-at-remote

  "hr" 'helm-resume

  "ll" 'custom-flycheck-toggle-errors
  "ln" 'flycheck-next-error
  "lp" 'flycheck-previous-error

  "mw" 'web-mode
  "mj" 'js-mode

  "p" 'projectile-command-map

  "ss" 'evil-search-word-forward
  "sr" 'replace-symbol
  "sa" 'find-symbol-in-project

  "tn" 'elscreen-create
  "tl" 'elscreen-next
  "th" 'elscreen-previous
  "tq" 'elscreen-kill
  "tj" 'elscreen-select-and-goto

  "w" 'evil-window-map

  "," 'ace-jump-char-mode

  "<SPC>" 'helm-M-x)
