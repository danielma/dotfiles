(add-hook 'javascript-mode-hook 'my-javascript-mode-setup)

(defun my-javascript-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (back-to-indentation)
      (current-column)))
  )

(defun my-javascript-mode-setup ()
  (hs-minor-mode 1)
  (modify-syntax-entry ?` "\"" js-mode-syntax-table)
  ;; (setq-local outline-regexp " *\\(function\\|class\\|describe\\|it(\\)")
  ;; (setq-local outline-level 'my-javascript-outline-level)

  ;; (outline-minor-mode 1)
  )
