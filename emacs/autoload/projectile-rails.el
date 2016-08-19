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

(defun projectile-rails-find-component ()
  (interactive)
  (projectile-rails-find-resource
   "component: "
   '(("app/assets/javascripts/components/" "/components/\\(.+\\.[jt]sx?\\)$"))
   "app/assets/javascripts/components/${filename}.js"))

(defun projectile-rails-setup ()
  "Customizations for projectile rails mode."
  (evil-leader/set-key-for-mode 'projectile-rails-mode
    "r" 'projectile-rails-command-map
    "jc" 'projectile-rails-find-component

    "rz" 'projectile-rails-find-serializer
    "rZ" 'projectile-rails-find-current-serializer)
  (define-key projectile-rails-command-map "p" 'projectile-rails-find-presenter)
  (define-key projectile-rails-command-map "s" 'projectile-rails-find-service)
  (define-key projectile-rails-command-map "a" 'projectile-rails-find-stylesheet)
  (define-key projectile-rails-command-map "A" 'projectile-rails-find-current-stylesheet)
  )
                                   
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'projectile-rails-mode-hook 'projectile-rails-setup)
