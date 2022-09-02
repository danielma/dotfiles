;; (dir-locals-set-class-variables
;;  'login
;;  '(
;;    (ruby-mode . ((prettier-js-command . "prettier_d")
;;                  (prettier-js-args . ("--plugin" "/Users/danielma/.config/yarn/global/node_modules/@prettier/plugin-ruby"))
;;                  (eval . (prettier-js-mode))))
;;    ))
   

;; (dir-locals-set-directory-class "~/Code/login" 'login)

;; (evil-set-initial-state 'rg-mode 'emacs)

(use-package rg
  :init
  (rg-enable-default-bindings)
  :custom
  (rg-custom-type-aliases '(("yuh" . "*")))
  :config
  (rg-define-search rg-project-simple-literal "Simple Literal"
    :format literal
    :files "everything"
    :dir project
    :menu ("Search" "l" "Simple literal"))
  )

(provide 'dm-projects)
