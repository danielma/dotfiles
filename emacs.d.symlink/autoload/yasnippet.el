(add-hook
 'web-mode-hook
 #'(lambda ()
     (if (string= "erb" web-mode-engine)
         (add-to-list 'yas--extra-modes 'html-erb-mode)
       )
     ))

(setq yas-snippet-dirs
      '(yas-installed-snippets-dir
        "~/.emacs.d/yasnippet-snippets/"))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [tab] 'tab-indent-or-complete)

(add-hook 'after-init-hook 'yas-global-mode)
