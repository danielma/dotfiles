;;; projectile-rails.el --- setup

;;; Commentary:

;;; Code:

(defun projectile-rails-find-service ()
  (interactive)
  (projectile-rails-find-resource
   "service: "
   '(("app/services/" "/services/\\(.+\\)\\.rb$"))
  "app/services/${filename}.rb"))

(defun projectile-rails-find-presenter ()
  (interactive)
  (projectile-rails-find-resource
   "presenter: "
   '(("app/presenters/" "/presenters/\\(.+?\\)\\(_presenter\\)?\\.rb$"))
   "app/presenters/${filename}_presenter.rb"))

(defun my/projectile-rails-find-javascript ()
  (interactive)
  (projectile-rails-find-resource
   "javascript: "
   '(("app/assets/javascripts/" "/javascripts/\\(.+\\.[jt]sx?\\)$"))
   "app/assets/javascripts/${filename}.js"))

(defun projectile-rails-find-component ()
  (interactive)
  (projectile-rails-find-resource
   "component: "
   '(("app/assets/javascripts/components/" "/components/\\(.+\\.[jt]sx?\\)$"))
   "app/assets/javascripts/components/${filename}.js"))

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

(evil-leader/set-key
  "jc" 'projectile-rails-find-component
  "jt" 'my/projectile-rails-find-js-test)

(setq my/projectile-rails-goto-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map projectile-rails-mode-goto-map)

        (define-key map "p" 'my/projectile-rails-goto-package-json)
        map))

(setq my/projectile-rails-command-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map projectile-rails-command-map)

        (define-key map "f" 'projectile-rails-find-presenter)
        (define-key map "s" 'projectile-rails-find-service)
        (define-key map "a" 'projectile-rails-find-stylesheet)
        (define-key map "A" 'projectile-rails-find-current-stylesheet)
        (define-key map "z" 'projectile-rails-find-serializer)
        (define-key map "Z" 'projectile-rails-find-current-serializer)
        (define-key map "R" 'my/projectile-rails-find-rake-tasks)
        (define-key map "j" 'my/projectile-rails-find-javascript)
        (define-key map "p" 'my/projectile-rails-find-spec-or-policy)
        (define-key map "g" my/projectile-rails-goto-map)
        map))

(evil-define-minor-mode-key
  'normal
  'projectile-rails-mode
  (kbd (concat evil-leader/leader "r"))
  my/projectile-rails-command-map)

(projectile-rails-global-mode)
