;;; dm-general.el --- Just general stuff

;;; Commentary:

;;; Code:

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

(defun chunkwm/move (dirstring)
  "Move to DIRSTRING with chunkwm integration."
  (let ((dir (pcase dirstring
               ("north" 'above)
               ("east" 'right)
               ("south" 'below)
               ("west" 'left))))
    (if (window-in-direction dir)
        (and (windmove-do-window-select dir) "0")
      '1)))

(provide 'dm-general)
;;; dm-general.el ends here
