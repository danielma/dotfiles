(defun interactive-wrap-with-pair (pair)
  "Interactively wraps."
  (interactive "c")
  (sp-wrap-with-pair (char-to-string pair)))

(defun expand-at-point ()
  "Insert a newline and put the cursor at the indented location above."
  (interactive)
  (newline-and-indent)
  (evil-open-above 1))

;; TODO this would be nice
;; (defun find-symbol-in-project ()
;;   "Search for symbol in project using projectile-ag."
;;   (interactive)
;;   (helm-ag-project-root (current-symbol-or-region)))

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

(defun my/delete-this-file ()
  (interactive)
  (delete-file buffer-file-name)
  (kill-this-buffer)
  )

; https://stackoverflow.com/a/1242366/4499924
(defun what-face (pos)
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
            (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(provide 'general-funcs)
;; general-funcs.el ends here
