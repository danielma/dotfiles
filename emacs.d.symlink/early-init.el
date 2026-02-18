(setq package-enable-at-startup nil)
(add-to-list 'default-frame-alist '(undecorated . t))

;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 100000000)))

