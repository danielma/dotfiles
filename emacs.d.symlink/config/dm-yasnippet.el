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

(use-package yasnippet
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (add-hook 'web-mode-hook 'web-mode-add-yas-extra-modes)
  :bind (:map company-mode-map
	 ("<tab>" . tab-indent-or-complete)
	 :map company-active-map
	 ("<tab>" . complete-or-yas-expand))
  )

(global-set-key [tab] 'tab-indent-or-complete)

(add-hook 'after-init-hook 'yas-global-mode)

(provide 'dm-yasnippet)
