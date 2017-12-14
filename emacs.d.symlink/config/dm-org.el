(use-package org-alert
  :config
  (setq alert-default-style 'notifier
        alert-fade-time 0))

(use-package org
  :config
  (setq org-directory "~/Dropbox/org"
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-agenda-files (quote ("~/Dropbox/org/agenda.org"))
        org-todo-keywords (quote ((sequence "TODO(t)" "DONE(d)")))
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t
        org-agenda-timegrid-use-ampm t
        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/org/gtd.org" "Tasks")
              "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/Dropbox/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  :bind (:map base-leader-map
              ("oa" . org-agenda)
  ))

(provide 'dm-org)
