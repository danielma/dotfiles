(defun my/projectile-rails-find-service ()
  (interactive)
  (projectile-rails-find-resource
   "service: "
   '(("app/services/" "/services/\\(.+\\)\\.rb$"))
  "app/services/${filename}.rb"))

(defun my/projectile-rails-find-presenter ()
  (interactive)
  (projectile-rails-find-resource
   "presenter: "
   '(("app/presenters/" "/presenters/\\(.+?\\)\\(_presenter\\)?\\.rb$"))
   "app/presenters/${filename}_presenter.rb"))

;; (defun projectile-rails-find-component ()
;;   (interactive)
;;   (projectile-rails-find-resource
;;    "component: "
;;    '(("app/assets/javascripts/components/" "/components/\\(.+\\.[jt]sx?\\)$"))
;;    "app/assets/javascripts/components/${filename}.js"))

(defun my/projectile-rails-find-rake-tasks ()
  (interactive)
  (projectile-rails-find-resource
   "tasks: "
   '(("lib/tasks/" "/tasks/\\(.+\\.rake\\)$"))
   "lib/tasks/${filename}.rake"))

(defun my/projectile-rails-find-js-test ()
  (interactive)
  (projectile-rails-find-resource
   "test: "
   '(("test/assets/javascripts/" "/javascripts/\\(.+\\)_test\\.js?$"))
   "test/assets/javascripts/${filename}_test.js"))

(defun my/projectile-rails-goto-package-json ()
  (interactive)
  (projectile-rails-goto-file "package.json"))

(defun my/projectile-rails-find-spec-or-policy ()
  (interactive)
  (if (file-exists-p (expand-file-name "spec" (projectile-project-root)))
      (projectile-rails-find-spec)
    (projectile-rails-find-resource
     "policy: "
     '(("app/policies/" "/policies/\\(.+?\\)\\(_policy\\)?\\.rb$"))
     "app/policies/${filename}_policy.rb")))

;; (evil-leader/set-key
;;   "jc" 'projectile-rails-find-component
;;   "jt" 'my/projectile-rails-find-js-test)

(use-package projectile-rails
  :init
  (projectile-rails-global-mode)
  :config
  (bind-map-for-mode-inherit my/projectile-rails-command-map base-leader-map
    :minor-modes (projectile-rails-mode)
    :bindings ("r" projectile-rails-command-map))
  :bind (
	 :map projectile-rails-command-map
         ("f" . my/projectile-rails-find-presenter)
         ("s" . my/projectile-rails-find-service)
         ("a" . projectile-rails-find-stylesheet)
         ("A" . projectile-rails-find-current-stylesheet)
         ("R" . my/projectile-rails-find-rake-tasks)
         ("p" . my/projectile-rails-find-spec-or-policy)
	 :map projectile-rails-mode-goto-map
	 ("p" . my/projectile-rails-goto-package-json)))

(provide 'dm-projectile-rails)
