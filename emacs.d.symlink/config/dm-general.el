;;; dm-general.el --- General utilities -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc #'kill-buffer
        (delq (current-buffer)
              (buffer-list))))

(use-package try
  :commands try)

(defun delete-this-file ()
  "Delete the file visited by the current buffer."
  (interactive)
  (delete-file buffer-file-name)
  (kill-this-buffer))

(provide 'dm-general)
;;; dm-general.el ends here
