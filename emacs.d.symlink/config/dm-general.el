;;; dm-general.el --- Just general stuff

;;; Commentary:

;;; Code:

;;; Windmove

(defun chunkwm/move (dirstring)
  "Move to DIRSTRING with chunkwm integration."
  (let* ((dir (pcase dirstring
		("north" 'above)
		("east" 'right)
		("south" 'below)
		("west" 'left)))
	 (other-window (window-in-direction dir)))
    (if other-window (select-window other-window) '1)))

;;; End windmove

(use-package try
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  )

(use-package undo-tree
  :init
  (global-undo-tree-mode))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(if t
    t
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name &optional buffer)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (read-string "New name: " (file-name-base (buffer-file-name)))))
  (let* ((original-buffer (or buffer (current-buffer))))
    (if (not (buffer-file-name original-buffer))
	(message "Buffer '%s' is not visiting a file!" (buffer-name original-buffer))
      (let* ((new-basename (file-name-base new-name))
	     (filename (buffer-file-name original-buffer))
	     (extension (file-name-extension (if (s-contains? "." new-name) new-name filename)))
	     (dir (file-name-directory filename))
	     (new-file (concat dir new-basename "." extension)))
	(rename-file filename new-file 1)
	(kill-buffer original-buffer)
	(find-file new-file)))))

(defun rename-file-and-counterpart (new-name)
  "Renames both current buffer and counterpart to NEW-NAME."
  (interactive (list (read-string "New name: " (file-name-base (buffer-file-name)))))
  (let* ((projectile-create-missing-test-files t)
	 (original-buffer (current-buffer))
	 (counterpart (projectile-find-implementation-or-test (buffer-file-name)))
	 (counterpart-buffer (find-buffer-visiting counterpart)))
    (if (not counterpart-buffer)
	(message "No counterpart buffer found!")
      (let* ((counterpart-basename (file-name-base counterpart))
	     (basename (file-name-base (buffer-file-name)))
	     (counterpart-new-name (s-replace basename new-name counterpart-basename)))
	(rename-file-and-buffer counterpart-new-name counterpart-buffer)
	(rename-file-and-buffer new-name original-buffer)))))
  

(defun delete-this-file ()
  "Deletes the active buffer file."
  (interactive)
  (delete-file buffer-file-name)
  (kill-this-buffer))

; https://stackoverflow.com/a/1242366/4499924
(defun what-face (pos)
  "Describes the current face at POS simply."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (initial-scratch-message nil))

(defun my/custom-dumb-jump-go (arg)
  "Custom dumb jump command."
  (interactive "P")
  (if (eq major-mode 'typescript-mode)
      (lsp-find-definition)
    (let ((xref-auto-jump-to-first-definition (and arg t)))
      (call-interactively 'evil-goto-definition))))

;; (use-package dumb-jump
;;   :custom
;;   (dumb-jump-selector 'ivy)
;;   :init
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;   :bind (:map base-leader-map
;; 	 ("sa" . my/custom-dumb-jump-go)
;; 	 ("sA" . dumb-jump-go-other-window)
;; 	 ("sd" . dumb-jump-go)
;; 	 ("sp" . dumb-jump-go-prompt)
;; 	 ("sl" . dumb-jump-quick-look)))

;; (use-package ivy-xref
;;   :custom
;;   (xref-show-definitions-function #'ivy-xref-show-defs)
;;   (xref-show-xrefs-function #'ivy-xref-show-xrefs))
)

(provide 'dm-general)
;;; dm-general.el ends here
