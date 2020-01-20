(dir-locals-set-class-variables
 'login
 '(
   (ruby-mode . ((prettier-js-command . "bundle")
                 (prettier-js-args . ("exec" "rbprettier"))
                 (eval . (push 'ruby-rubocop flycheck-disabled-checkers))
                 (eval . (prettier-js-mode))))
   ))

(dir-locals-set-directory-class "~/Code/login" 'login)

(provide 'dm-projects)
