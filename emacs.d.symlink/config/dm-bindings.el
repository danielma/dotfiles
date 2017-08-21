(defun edit-emacs ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun edit-scratch ()
  (interactive)
  (find-file "~/SCRATCH.md"))

(defun edit-yasnippet-dir ()
  (interactive)
  (dired "~/.dotfiles/emacs.d.symlink/yasnippet-snippets"))

(defun edit-dotfiles ()
  (interactive)
  (dired "~/.dotfiles"))

(defun force-reload ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

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

(defun reveal-in-finder ()
  (interactive)
  (shell-command (concat "open -R " (buffer-file-name))))

(defun find-definition-in-file ()
  (interactive)
  (let ((keyword (pcase major-mode
		   ('emacs-lisp-mode "defun")
		   ('ruby-mode "def")
		   ('web-mode "function")
		   (_ (error (concat "no definition keyword for " (symbol-name major-mode)))))))
    (evil-search (concat keyword " " (current-symbol-or-region)) t)))

(defvar base-leader-map (make-sparse-keymap) "The main LEADER map.")

(use-package bind-map
  :config
  (bind-map base-leader-map
    :override-minor-modes t
    :keys ("M-m")
    :evil-keys ("SPC"))

  (bind-map-set-keys base-leader-map
    "fs" 'save-buffer-always
    "fq" 'delete-window
    "fr" 'force-reload

    "bd" 'kill-this-buffer
    "bs" 'helm-buffers-list

    "cd" 'cd
    "cl" 'custom-comment-line
    "ct" 'my/base16-set-theme
    "cf" 'my/set-custom-face

    "dr" 'reveal-in-finder

    "ee" 'edit-emacs
    "ed" 'edit-dotfiles
    "es" 'edit-scratch
    "ey" 'edit-yasnippet-dir

    "sr" 'replace-symbol
    "sd" 'find-definition-in-file
    "sa" 'find-symbol-in-project

    "T" text-tools-map

    "," 'ace-jump-char-mode)
  )

(use-package hydra
  :config
  (defhydra hydra-buffers ()
    "buffers"				;
    ("p" previous-buffer "previous")
    ("n" next-buffer "next"))
  :bind (:map base-leader-map
	      ("b." . hydra-buffers/body)))

(provide 'dm-bindings)
