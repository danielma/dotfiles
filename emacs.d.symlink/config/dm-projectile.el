(use-package projectile
  :init
  (projectile-mode)

  :config
  (define-key base-leader-map "p" projectile-command-map)

  (use-package helm-projectile
    :init
    (helm-projectile-on)
    :bind (:map projectile-command-map
		("a" . helm-projectile-ag)))
  (use-package helm-ag
    :init
    (setq helm-ag-fuzzy-match t)
    (setq helm-ag-insert-at-point (quote symbol))
    )

  (setq projectile-generic-command "ag -g \"\""
        projectile-switch-project-action 'magit-status)

  :bind (:map projectile-command-map
	("T" . projectile-find-implementation-or-test-other-window)))

(defun my/projectile-choices (dirs)
  "Find files in directories by (dir re) pair.

The DIRS is list of lists consisting of a directory path and regexp to filter files from that directory.
Returns a hash table with keys being short names and values being relative paths to the files."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (dir re) in dirs do
          (loop for file in (projectile-dir-files (concat (projectile-project-root) dir)) do
                (when (string-match re file)
                  (puthash (match-string 1 file) (concat dir file) hash))))
    hash))

(defun my/projectile-goto-file (filepath)
  (find-file (concat (projectile-project-root) filepath)))

(defmacro my/projectile-find-resource (prompt dirs &optional newfile-template)
  "Presents files from DIRS to the user using `projectile-completing-read'.

If users chooses a non existant file and NEWFILE-TEMPLATE is not nil
it will use that variable to interpolate the name for the new file.
NEWFILE-TEMPLATE will be the argument for `s-lex-format'.
The bound variable is \"filename\"."
  `(let* ((choices (my/projectile-choices ,dirs))
          (filename (or
                     (projectile-completing-read ,prompt (hash-table-keys choices))
                     (user-error "The completion system you're using does not allow inputting arbitrary value.")))
          (filepath (gethash filename choices)))
     (if filepath
         (my/projectile-goto-file filepath)
       (when ,newfile-template
         (my/projectile-goto-file (s-lex-format ,newfile-template))))))

(provide 'dm-projectile)
