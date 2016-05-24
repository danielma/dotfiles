(defun comint-goto-end-and-insert ()
  (interactive)
  (if (not (comint-after-pmark-p))
      (progn (comint-goto-process-mark)
             (evil-append-line nil))
    (evil-insert 1)))

(defun my-alchemist-iex-mode-config ()
  "For use in `alchemist-iex-mode`."
  (evil-define-key 'normal comint-mode-map "i" 'comint-goto-end-and-insert)

  (evil-define-key 'insert comint-mode-map
    (kbd "<up>") 'comint-previous-input
    (kbd "<down>") 'comint-next-input)
  )


;; add to hook
(add-hook 'alchemist-iex-mode-hook 'my-alchemist-iex-mode-config)
