;;; evil.el --- setup for evil mode

;;; Commentary:

;;; Code:

(fset 'evil-visual-update-x-selection 'ignore)

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
  (find-file "~/.emacs.d/init.el"))

(defun edit-scratch ()
  (interactive)
  (find-file "~/SCRATCH.md"))

(defun my/edit-reload ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun reveal-in-finder ()
  (interactive)
  (shell-command (concat "open -R " (buffer-file-name))))

(setq my/evil-window-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map evil-window-map)

        (define-key map "]" 'buf-move-right)
        (define-key map "[" 'buf-move-left)
        (define-key map "{" 'buf-move-up)
        (define-key map "}" 'buf-move-down)
        map))

(evil-leader/set-key
  "fs" 'save-buffer-always
  "fq" 'delete-window
  "fl" 'sr-speedbar-toggle
  "fr" 'my/edit-reload

  "bd" 'kill-this-buffer
  "bs" 'switch-to-buffer

  "cd" 'cd
  "cl" 'custom-comment-line
  "ct" 'my/base16-set-theme
  "cf" 'my/set-custom-face

  "dr" 'reveal-in-finder

  "ee" 'edit-emacs
  "es" 'edit-scratch

  "gs" 'magit-status
  "gc" 'magit-commit
  "gd" 'magit-diff-buffer-file
  "gl" 'magit-log-buffer-file
  "gb" 'magit-blame
  "gB" 'browse-at-remote

  "hr" 'helm-resume
  "hk" 'helm-show-kill-ring

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
  "tt" 'elscreen-toggle-display-tab

  "T" text-tools-map

  "w" my/evil-window-map

  "," 'ace-jump-char-mode

  "<SPC>" 'helm-M-x)
