(defun my/projectile-rails-find-service ()
  (interactive)
  (projectile-rails-find-resource
   "service: "
   '(("app/services/" "\\(.+\\)\\.rb$"))
   "app/services/${filename}.rb"))

(defun my/projectile-rails-find-presenter ()
  (interactive)
  (projectile-rails-find-resource
   "presenter: "
   '(("app/presenters/" "\\(.+?\\)\\(_presenter\\)?\\.rb$"))
   "app/presenters/${filename}_presenter.rb"))

(defun my/projectile-rails-find-rake-tasks ()
  (interactive)
  (projectile-rails-find-resource
   "tasks: "
   '(("lib/tasks/" "\\(.+\\.rake\\)$"))
   "lib/tasks/${filename}.rake"))

(defun my/projectile-rails-find-js-test ()
  (interactive)
  (projectile-rails-find-resource
   "test: "
   '(("test/assets/javascripts/" "\\(.+\\)_test\\.js?$"))
   "test/assets/javascripts/${filename}_test.js"))

(defun my/projectile-rails-find-vertex ()
  (interactive)
  (projectile-rails-find-resource
   "vertex: "
   '(("app/graphs/" "\\(.+\\)_graph\\(.*\\)/vertices\\(/.+\\)_vertex.rb$"))))

(defun my/projectile-rails-goto-package-json ()
  (interactive)
  (projectile-rails-goto-file "package.json"))

(defun my/projectile-rails-goto-gemfile-lock ()
  (interactive)
  (projectile-rails-goto-file "Gemfile.lock"))

(defun my/projectile-rails-find-test-or-spec ()
  (interactive)
  (if (eq (projectile-project-type) 'rails-rspec)
      (projectile-rails-find-spec)
    (projectile-rails-find-test)))

(defun my/projectile-rails-find-spec-or-policy ()
  (interactive)
  (if (eq (projectile-project-type) 'rails-rspec)
      (projectile-rails-find-spec)
    (projectile-rails-find-resource
     "policy: "
     '(("app/policies/" "\\(.+?\\)\\(_policy\\)?\\.rb$"))
     "app/policies/${filename}_policy.rb")))

(defun my/projectile-rails-fixture-dirs ()
  (--map (list it "fixtures/\\(.+?\\)\\.yml$")
         '("test/fixtures" "spec/fixtures")))

(defun my/projectile-rails-select-fixture ()
  (interactive)
  (let* ((choices (project-choices (my/projectile-rails-fixture-dirs)))
	 (type (my/projectile-rails-select-fixture-type))
         (root (project-root (project-current)))
	 (filepath (file-name-concat root (gethash type choices)))
	 (fixture (my/projectile-rails-select-fixture-in-file filepath))
	 (cleaned-type (s-replace "/" "_" type)))
    ;; (string-match "[^a-zA-Z_]" "hello@my_guy")
    (if (string-match "[^A-Za-z_]" fixture)
        (concat cleaned-type "(\"" fixture "\")")
      (concat cleaned-type "(:" fixture ")"))))

(defun my/projectile-rails-select-fixture-type ()
  "Select a fixture type"
  (interactive)
  (let* ((choices (project-choices (my/projectile-rails-fixture-dirs)))
         (type (completing-read "type: " (hash-table-keys choices))))
    type))

(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (setq matches (append matches (list (match-string 0 string))))
        (setq pos (match-end 0)))
      matches)))

;; (let ((yaml-source "---
;; andy_bernard_andy@dunder.mifflin:
;;   hello: values

;; "))
;;   (re-seq "^[a-z][^: ]+" yaml-source))

(defun my/projectile-rails-select-fixture-in-file (filename)
  "Select a fixture in FILENAME."
  (let* ((yaml-source (with-temp-buffer (insert-file-contents filename) (buffer-substring-no-properties (point-min) (point-max))))
         ;; (parsed-yaml (yaml-parse-string yaml-source :object-type 'alist))
         ;; (yaml-keys (mapcar 'car parsed-yaml)))
         (yaml-keys (re-seq "^[a-z][^: ]+" yaml-source)))
    (completing-read "fixture: " yaml-keys))
  )

(defcustom projectile-rails-component-dirs
  '("app/javascript/")
  "The directory to look for javascript component files in."
  :group 'projectile-rails
  :type 'string)

(defun my/projectile-rails-find-component ()
  "Find a react component."
  (interactive)
  (projectile-rails-find-resource
   "components: "
   (--map (list it "\\(.*/?\\)components/\\(.+\\)\\.[^.]+$") projectile-rails-component-dirs)))

(if t
    t

  ;; (evil-leader/set-key
  ;;   "jc" 'projectile-rails-find-component
  ;;   "jt" 'my/projectile-rails-find-js-test)

  (use-package inf-ruby)
  (use-package inflections)
  (use-package rake)

  (use-package projectile-rails
    :after bind-map
    :straight (:type git :host github :repo "danielma/projectile-rails" :branch "dma/use-all-matches-for-finding-resource"
                     :upstream (:host github :repo "asok/projectile-rails"))
    :init
    (projectile-rails-global-mode)
    (setq projectile-rails-component-dir "app/javascript/"
	  projectile-rails-javascript-dirs (add-to-list 'projectile-rails-javascript-dirs "app/javascript/"))
    :config
    (bind-map-for-mode-inherit my/projectile-rails-command-map base-leader-map
                               :minor-modes (projectile-rails-mode)
                               :bindings ("r" projectile-rails-command-map))
    :bind (
	   :map projectile-rails-command-map
           ("f" . my/projectile-rails-find-presenter)
           ("s" . my/projectile-rails-find-service)
           ("t" . my/projectile-rails-find-test-or-spec)
           ("a" . projectile-rails-find-stylesheet)
           ("A" . projectile-rails-find-current-stylesheet)
           ("R" . my/projectile-rails-find-rake-tasks)
           ("p" . my/projectile-rails-find-spec-or-policy)
           ("w" . my/projectile-rails-find-component)
           ("V" . my/projectile-rails-find-vertex)
	   :map projectile-rails-mode-goto-map
           ("G" . my/projectile-rails-goto-gemfile-lock)
	   ("p" . my/projectile-rails-goto-package-json)))
  )

(provide 'dm-projectile-rails)
