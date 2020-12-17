(defun projectile-rubygem-project-p ()
  (projectile-verify-file-wildcard "*.gemspec"))

(defun my/projectile-switch-command (&optional directory)
  "If the project being switched to is a git repository, invoke
magit-status on the project root directory. Use dired otherwise."
  (interactive)
  (let ((project-root (or directory (projectile-project-root))))
    (if (and (executable-find "git")
             (file-exists-p (concat project-root "/.git/index")))
        (magit-status project-root)
      (dired project-root))))

(defun my/projectile-ignore-project (arg)
  (s-contains? "/.rbenv/" arg))

(use-package projectile
  :init
  (projectile-mode)
  :config
  (define-key base-leader-map "p" projectile-command-map)
  (projectile-register-project-type 'rubygem 'projectile-rubygem-project-p
                                    :compile "bundle"
                                    :test "bundle exec rspec"
                                    :test-suffix "_spec.rb")
  ;; (projectile-register-project-type 'generic '("README.md"))
  (add-to-list 'projectile-project-root-files-bottom-up "Gemfile")
  :custom
  (projectile-generic-command "rg --files")
  (projectile-git-submodule-command "git submodule --quiet foreach 'echo $displaypath' | tr '\\n' '\\0'")
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action 'my/projectile-switch-command) ;; you're looking in the wrong place. set counsel
  (projectile-create-missing-test-files t)
  (projectile-project-search-path '("~/Code/"))
  (projectile-ignored-project-function 'my/projectile-ignore-project)
  :bind (:map projectile-command-map
        ("a" . my/projectile-search)
        ("A" . my/projectile-search-in-dir)
        ("F" . my/projectile-find-file-in-other-project)
	("T" . projectile-find-implementation-or-test-other-window)))

(defalias 'my/projectile-search 'counsel-projectile-rg)

(defun my/projectile-search-in-dir ()
  (interactive)
  (let ((dir (counsel-read-directory-name (concat
                                          (car (split-string counsel-ag-command))
                                          " in directory: "))))
    (counsel-rg "" dir)))

(defun my/projectile-find-file-in-other-project ()
  "Find a file in another project."
  (interactive)
  (let ((projectile-switch-project-action 'projectile-find-file))
    (projectile-switch-project)))

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
