(setq package-enable-at-startup nil)

;; Treat Ghostty as xterm-compatible before terminal clients initialize.
(add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm"))
(add-to-list 'term-file-aliases '("ghostty" . "xterm"))

(add-to-list 'default-frame-alist '(undecorated . t))

;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 100000000)))
