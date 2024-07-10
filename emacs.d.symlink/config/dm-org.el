;;; dm-org --- org setup

;;; Commentary:

;;; Code:

(use-package inf-ruby)
(use-package org)
(use-package org-babel
  :after org
  :custom
  (org-confirm-babel-evaluate nil)
  )

(provide 'dm-org)

;;; dm-org.el ends here
