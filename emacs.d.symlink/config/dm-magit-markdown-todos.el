;;; magit-markdown-todos.el --- Add local todo items to the magit status buffer

;; Copyright (C) 2018 Daniel Ma <github.com/danielma>
;; Author: Daniel Ma
;; URL: http://github.com/danielma/magit-markdown-todos
;; Created: 2018
;; Version: 0.1.3
;; Keywords: org-mode magit tools
;; Package-Version: 0.1.3
;; Package-Requires: ((magit "2.0.0") (emacs "24"))

;;; Commentary:
;;
;; adds all TODO items from a todo.md file in the magit project's root
;; to the magit status buffer

;;; Code:
(require 'magit)

;;; Customizations:
(defgroup magit-markdown-todos nil
  "Add local todo items to the magit status buffer"
  :group 'tools)

(defcustom magit-markdown-todos-filename "todo.md"
  "The org file that holds todo items."
  :group 'magit-markdown-todos
  :type 'string)

;;; Implementation:
(defun magit-markdown-todos--todo-file-path ()
  "Path of the todo file."
  (let* ((toplevel (magit-toplevel))
         (todo (concat toplevel magit-markdown-todos-filename)))
    todo))

(defun magit-markdown-todos--visit-todo ()
  "Visits the org todo file."
  (interactive)
  (find-file (magit-markdown-todos--todo-file-path)))

(defvar magit-markdown-todos-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap magit-visit-thing] 'magit-markdown-todos--visit-todo)
    m))

;;;###autoload
(defun magit-markdown-todos-insert-markdown-todos ()
  "Insert org todos from the local todo.md."
  (when (file-readable-p (magit-markdown-todos--todo-file-path))
    (let ((todos '()))
      (with-temp-buffer
        (insert-file-contents (magit-markdown-todos--todo-file-path))
        (markdown-mode)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((bounds (markdown-cur-list-item-bounds)))
            (when (and bounds (s-matches? "\\[ \\]" (nth 5 bounds)))
              ;; (0     1   2      3              4      5        6    )
              ;; (begin end indent nonlist-indent marker checkbox match)
              (let* ((checkbox (nth 5 bounds))
                     (title (buffer-substring-no-properties (+ (nth 0 bounds) (nth 3 bounds) (length checkbox)) (line-end-position)))
                     (level (/ (nth 2 bounds) 2)))
                (add-to-list 'todos (list level checkbox title) t))))
          (forward-line 1)))
      (when (> (length todos) 0)
        (magit-insert-section (markdown-todos-wrapper)
          (magit-insert-heading "Todos:")
          (dolist (todo todos)
            (let ((checkbox (nth 1 todo))
                  (title (nth 2 todo))
                  (level (nth 0 todo)))
              (magit-insert-section (org-todos title)
                (dotimes (i level) (insert "  "))
                (insert (concat
                         "- "
                         (propertize checkbox 'face 'markdown-gfm-checkbox-face)
                         title))))
            (insert ?\n))
          (insert ?\n))))))

;;;###autoload
(defun magit-markdown-todos-autoinsert ()
  "Automatically insert todo section into magit status buffer."
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-markdown-todos-insert-markdown-todos
   'magit-insert-staged-changes
   t))

(provide 'dm-magit-markdown-todos)

;;; magit-markdown-todos.el ends here
