(defun web-mode-add-yas-extra-modes ()
  (if (string= "erb" web-mode-engine)
      (add-to-list 'yas--extra-modes 'html-erb-mode)
    ))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun complete-or-yas-expand ()
  (interactive)
  (if (or (not yas-minor-mode)
	  (null (do-yas-expand)))
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "->") t nil)))))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(defun my/no-yas ()
  (or
   (equal major-mode 'help-mode)
   (equal major-mode 'org-agenda-mode)
   (derived-mode-p 'compilation-mode)))

(use-package company
  :commands company-complete-common
  :init
  (global-company-mode)
  (setq company-dabbrev-downcase nil
	company-idle-delay 0.2)
  :config
  (setq company-global-modes '(not help-mode compilation-mode org-agenda-mode))
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
	      ("<tab>" . tab-indent-or-complete)
	(:map company-mode-map
	      ("<tab>" . tab-indent-or-complete))))

(use-package yasnippet
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (add-hook 'web-mode-hook 'web-mode-add-yas-extra-modes)
  (setq yas-dont-activate-functions (add-to-list 'yas-dont-activate-functions #'my/no-yas))
  :bind (:map yas-minor-mode-map
	 ("<tab>" . tab-indent-or-complete))
  )

(add-hook 'after-init-hook 'yas-global-mode)

(provide 'dm-yasnippet)
