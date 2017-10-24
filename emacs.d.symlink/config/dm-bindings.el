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

; https://stackoverflow.com/a/42862075/4499924
(defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

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

    "T" text-tools-map)
  )

(use-package ace-jump-mode
  :config
  (key-chord-define evil-insert-state-map ",," 'ace-jump-mode)
  :bind (:map base-leader-map
 	("," . ace-jump-char-mode)))

(use-package hydra
  :config
  (defhydra hydra-buffers (base-leader-map "b")
    "buffers"
    ("d" kill-this-buffer "kill")
    ("s" helm-buffers-list "list" :exit t)
    ("p" previous-buffer "previous")
    ("n" next-buffer "next")))

(use-package winner-mode
  :ensure nil
  :init
  (winner-mode 1)
  (defun toggle-single-window ()
    "Make WINDOW fill its frame. Execute `winner-undo` if it's already full."
    (interactive)
    (if (eq (count-windows) 1)
	(winner-undo)
      (delete-other-windows)))
  :bind (:map global-map
	 ("C-x 1" . toggle-single-window)))

(provide 'dm-bindings)
