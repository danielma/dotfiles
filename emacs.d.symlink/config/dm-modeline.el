;;; dm-modeline.el --- Mode-line presentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Compose the mode line and connect status supplied by other packages.

;;; Code:

(require 'project)

(defface doom-modeline-project-dir
  '((t (:inherit mode-line-emphasis)))
  "Project directory face supplied by Doom themes.")

(defface doom-modeline-buffer-path
  '((t (:inherit mode-line-emphasis)))
  "Buffer path face supplied by Doom themes.")

(defface doom-modeline-buffer-file
  '((t (:inherit mode-line-emphasis)))
  "Buffer file face supplied by Doom themes.")

(defface doom-modeline-buffer-modified
  '((t (:inherit warning)))
  "Modified buffer face supplied by Doom themes.")

(defface doom-modeline-error
  '((t (:inherit error)))
  "Error face supplied by Doom themes.")

(defface doom-modeline-buffer-major-mode
  '((t (:inherit mode-line-emphasis)))
  "Major mode face supplied by Doom themes.")

(defface doom-modeline-info
  '((t (:inherit success)))
  "Informational face supplied by Doom themes.")

(defface doom-modeline-bar
  '((t (:inherit mode-line)))
  "Accent bar face supplied by Doom themes.")

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Highlighted panel face supplied by Doom themes.")

(setopt line-number-mode t
        column-number-mode t
        mode-line-collapse-minor-modes '(not flymake-mode)
        mode-line-collapse-minor-modes-to " +"
        mode-line-compact 'long
        mode-line-percent-position '(-3 "%p")
        project-mode-line 'non-remote
        project-mode-line-face 'doom-modeline-project-dir)

(defun dm-modeline-apply-faces ()
  "Apply Doom modeline faces to native mode-line components."
  (set-face-attribute 'mode-line-buffer-id nil
                      :inherit 'doom-modeline-buffer-file
                      :foreground 'unspecified
                      :weight 'unspecified))

(defvar-local dm-modeline-git-branch nil
  "Current Git branch displayed in this buffer's mode line.")

(defun dm-modeline-update-git-branch ()
  "Update the current buffer's cached Git branch."
  (setq dm-modeline-git-branch
        (unless (file-remote-p default-directory)
          (magit-get-current-branch))))

(defun dm-modeline-refresh-git-branches ()
  "Refresh cached branches for buffers in the current repository."
  (when-let* ((root (magit-toplevel)))
    (let ((branch (magit-get-current-branch)))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and buffer-file-name
                     (file-in-directory-p buffer-file-name root))
            (setq dm-modeline-git-branch branch))))))
  (force-mode-line-update t))

(defun dm-modeline-git-branch ()
  "Return the current Git branch as a mode-line construct."
  (when dm-modeline-git-branch
    (propertize (concat " " dm-modeline-git-branch)
                'face 'doom-modeline-info
                'help-echo "Current Git branch")))

(defun dm-modeline-configure-format ()
  "Arrange the default mode-line components."
  (setq-default mode-line-format
                (mapcan
                 (lambda (construct)
                   (cond
                    ((equal construct
                            '(project-mode-line project-mode-line-format))
                     nil)
                    ((eq construct 'mode-line-buffer-identification)
                     (list '(project-mode-line project-mode-line-format)
                           " "
                           construct))
                    ((equal construct '(vc-mode vc-mode))
                     (list '(:eval (dm-modeline-git-branch))))
                    (t
                     (list construct))))
                 mode-line-format)))

(dm-modeline-configure-format)

(add-hook 'find-file-hook #'dm-modeline-update-git-branch)

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'dm-modeline-refresh-git-branches))

(with-eval-after-load 'meow
  (setopt meow-replace-state-name-list '((normal . "N")
                                          (motion . "M")
                                          (keypad . "K")
                                          (insert . "I")
                                          (beacon . "B"))
          meow-indicator-face-alist '((normal . doom-modeline-info)
                                      (motion . doom-modeline-project-dir)
                                      (keypad . doom-modeline-panel)
                                      (insert . doom-modeline-buffer-major-mode)
                                      (beacon . doom-modeline-buffer-modified)))
  (meow-setup-indicator))

(provide 'dm-modeline)
;;; dm-modeline.el ends here
