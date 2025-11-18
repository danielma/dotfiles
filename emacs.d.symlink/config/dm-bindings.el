;;; dm-bindings.el --- Global bindings
;;; Commentary:

;;; Code:

(defun yank-with-clipboard ()
  "Yank from the system clipboard."
  (interactive)
  (let ((select-enable-clipboard t))
    (yank)))

(defadvice save-buffer (before save-buffer-always activate)
  "always save buffer"
  (set-buffer-modified-p t))

;;; 
;;; C-x t C-f				find-file-other-tab
;;; C-x t RET				tab-switch
;;; C-x t C-r				find-file-read-only-other-tab
;;; C-x t 0					tab-close
;;; C-x t 1					tab-close-other
;;; C-x t 2					tab-new
;;; C-x t G					tab-group
;;; C-x t M					tab-move-to
;;; C-x t N					tab-new-to
;;; C-x t O					tab-previous
;;; C-x t b					switch-to-buffer-other-tab
;;; C-x t d					dired-other-tab
;;; C-x t f					find-file-other-tab
;;; C-x t m					tab-move
;;; C-x t n					tab-duplicate
;;; C-x t o					tab-next
;;; C-x t p					project-other-tab-command
;;; C-x t r					tab-rename
;;; C-x t t					other-tab-prefix
;;; C-x t u					tab-undo
;;; 
;;; C-x t ^ f				tab-detach

(transient-define-prefix tab-bar-transient-menu ()
  [("RET" "Switch" tab-switch)
   ("t" "New" tab-new)
   ("n" "Next" tab-next :transient t)
   ("p" "Previous" tab-previous :transient t)
   ("q" "Quit" transient-quit-one)
   ("x" "Close" tab-close)
   ])

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
         ("s-v" . yank-with-clipboard)
         ("M-n" . windmove-down)
         ("M-e" . windmove-up)
         ("M-m" . windmove-left)
         ("M-i" . windmove-right)
         ("C-x C-t" . tab-bar-transient-menu)
         :map window-prefix-map
         ("=" . balance-windows)
         ("n" . windmove-down)
         ("m" . windmove-left)
         ("e" . windmove-up)
         ("i" . windmove-right)
	       )
  :custom (select-enable-clipboard . nil))

(defun with-select-clipboard (orig-fun &rest args)
  "Execute the ORIG-FUN with ARGS with `select-enable-clipboard' enabled."
  (let ((select-enable-clipboard t))
    (apply orig-fun args)))

(advice-add 'ns-copy-including-secondary :around #'with-select-clipboard)

;; (if (boundp 'mac-command-modifier)
;;     (progn
;;       (setq select-enable-clipboard nil
;;             mac-option-modifier 'meta
;;             mac-command-modifier 'super
;;             )))

;; (defun edit-emacs ()
;;   "Edit init.el."
;;   (interactive)
;;   (find-file "~/.emacs.d/init.el"))

;; (defun edit-scratch ()
;;   "Edit main scratch file."
;;   (interactive)
;;   (find-file "~/SCRATCH.md"))

;; (defun edit-yasnippet-dir ()
;;   "Edit yasnippets."
;;   (interactive)
;;   (dired "~/.dotfiles/emacs.d.symlink/snippets"))

;; (defun edit-dotfiles ()
;;   "Open dotfiles."
;;   (interactive)
;;   (dired "~/.dotfiles"))

;; (defun force-reload ()
;;   "Revert buffer without confirmation."
;;   (interactive)
;;   (revert-buffer :ignore-auto :noconfirm))

;; (defun save-buffer-always ()
;;   "Save this buffer even if it hasn't been modieifed."
;;   (interactive)
;;   (set-buffer-modified-p t)
;;   (save-buffer))

;; (defun toggle-comment-on-line ()
;;   "Comment or uncomment current line."
;;   (interactive)
;;   (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; (defun custom-comment-line ()
;;   "Comment lines the way I want to."
;;   (interactive)
;;   (if (region-active-p)
;;       (call-interactively 'comment-or-uncomment-region)
;;       (call-interactively 'toggle-comment-on-line)))

;; ; https://stackoverflow.com/a/42862075/4499924
;; (defun kill-other-buffers ()
;;       "Kill all other buffers."
;;       (interactive)
;;       (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; (defun current-symbol-or-region ()
;;   "Return the symbol under the cursor or the selected region."
;;   (let (from to sym)
;;     (if (use-region-p)
;;         (progn
;;           (setq sym (buffer-substring-no-properties (mark) (point))))
;;         (progn
;;           (save-excursion
;;             (skip-syntax-backward "w_") (setq from (point)))
;;           (save-excursion
;;             (skip-syntax-forward "w_") (setq to (point)))
;;           (setq sym (buffer-substring-no-properties to from))))
;;     sym))

;; (defun replace-symbol ()
;;   "EVIL: search for instances of the symbol under the cursor."
;;   (interactive)
;;   (evil-ex (concat "%s/" (current-symbol-or-region) "/")))

;; (defun reveal-in-finder ()
;;   "Open dir."
;;   (interactive)
;;   (shell-command (concat "open -R " (buffer-file-name))))

;; (use-package bind-map)

;; (use-package emacs
;;   :after evil
;;   :bind
;;   (:map global-map
;;         ("s-<return>" . toggle-frame-fullscreen)
;;         ("s-s" . save-buffer-always)
;;         ("C-h C-f" . find-function)
;;         ("C-'" . company-yasnippet)
;;         ("C-." . my/m-x)
;;         ("s-O" . browse-url)
;;         )
;;   (:map base-leader-map
;;         ("<SPC>" . my/m-x)
;;         ("fs" . save-buffer-always)
;;         ("fq" . delete-window)
;;         ("fr" . force-reload)

;;         ("cd" . cd)
;;         ("cl" . custom-comment-line)
;;         ("ct" . my/base16-set-theme)
;;         ("cf" . menu-set-font)

;;         ("dr" . reveal-in-finder)

;;         ("ee" . edit-emacs)
;;         ("ed" . edit-dotfiles)
;;         ("es" . edit-scratch)
;;         ("ey" . edit-yasnippet-dir)

;;         ("sr" . replace-symbol))
;;   :bind-keymap
;;   ("M-m" . base-leader-map)
;;   :config
;;   (bind-map base-leader-map
;;     :keys ("M-m")
;;     :evil-keys ("SPC")
;;     :evil-states (normal visual))
;;   (bind-key "T" dm-text-map base-leader-map)
;;   )

;; (use-package hydra
;;   :config
;;   (defhydra hydra-buffers (base-leader-map "b")
;;     "buffers"
;;     ("d" kill-this-buffer "kill")
;;     ("s" ivy-switch-buffer "list" :exit t)
;;     ("p" previous-buffer "previous")
;;     ("n" next-buffer "next")))

;; (use-package winner-mode
;;   :straight nil
;;   :init
;;   (winner-mode 1)
;;   (defun toggle-single-window ()
;;     "Make WINDOW fill its frame. Execute `winner-undo` if it's already full."
;;     (interactive)
;;     (if (eq (count-windows) 1)
;; 	(winner-undo)
;;       (delete-other-windows)))
;;   :bind (:map global-map
;; 	 ("C-x 1" . toggle-single-window)))

(provide 'dm-bindings)
;;; dm-bindings.el ends here
