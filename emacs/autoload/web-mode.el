(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(add-to-list 'auto-mode-alist '("\\.html\\(\+modal\\)?\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(defun my-web-mode-setup ()
  (flycheck-mode (cond ((equal web-mode-content-type "jsx") t)
                       ((equal web-mode-content-type "javascript") t)
                       (t nil)))
  (if (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx")))
(evil-define-key 'normal web-mode-map "zc" 'web-mode-fold-or-unfold)
(evil-define-key 'normal web-mode-map "zo" 'web-mode-fold-or-unfold)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'my-web-mode-setup)
