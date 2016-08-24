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

(evil-leader/set-key-for-mode 'projectile-rails-mode
  "r" 'projectile-rails-command-map
  "jc" 'projectile-rails-find-component)

(setq my/projectile-rails-command-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map projectile-rails-command-map)

        (define-key map "f" 'projectile-rails-find-presenter)
        (define-key map "s" 'projectile-rails-find-service)
        (define-key map "a" 'projectile-rails-find-stylesheet)
        (define-key map "A" 'projectile-rails-find-current-stylesheet)
        (define-key map "z" 'projectile-rails-find-serializer)
        (define-key map "Z" 'projectile-rails-find-current-serializer)
        map))
        
(evil-define-minor-mode-key
  'normal
  'alchemist-phoenix-mode
  (kbd (concat evil-leader/leader "r"))
  my/projectile-rails-command-map)
