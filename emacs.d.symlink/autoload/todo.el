;; todo

(defun my/new-todo (body)
  (save-window-excursion
    (find-file "~/org/auto.org")
    (goto-char (point-max))
    (org-insert-todo-heading "TODO")
    (insert body)
    (save-buffer)))

(defun my/todo-done (body)
  (save-window-excursion
    (find-file "~/org/auto.org")
    (goto-char (point-min))
    (search-forward body)
    (kill-whole-line)
    (save-buffer)))

(defun my/todos-print ()
  (save-window-excursion
    (find-file "~/org/auto.org")
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))
