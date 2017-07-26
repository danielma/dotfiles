(defun interactive-wrap-with-pair (pair)
  (interactive "c")
  (sp-wrap-with-pair (char-to-string pair)))

(defun expand-at-point ()
  "Insert a newline and put the cursor at the indented location above."
  (interactive)
  (newline-and-indent)
  (evil-open-above 1))

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
;; (defun find-symbol-in-project ()
;;   "Search for symbol in project using projectile-ag."
;;   (interactive)
;;   (helm-ag-project-root (current-symbol-or-region)))

;; TODO: this should open 20% below
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

(defun edit-yasnippet-dir ()
  (interactive)
  (dired "~/.dotfiles/emacs.d.symlink/yasnippet-snippets"))

(defun force-reload ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun reveal-in-finder ()
  (interactive)
  (shell-command (concat "open -R " (buffer-file-name))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(provide 'general-funcs)
;; general-funcs.el ends here
