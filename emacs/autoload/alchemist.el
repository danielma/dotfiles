(require 'elixir-mode)

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

(defun my-alchemist-mode-config ()
  "tryna alchemize"
  (evil-leader/set-key
    "a" 'alchemist-key-command-prefix)
  (defun alchemist-file-find-files (root directory)
    "open DIRECTORY inside ROOT and prompt for a file."
    (let* ((files (alchemist-file-read-dir root directory))
           (root-name (car (cdr (reverse (split-string root "/")))))
           (file (projectile-completing-read (format "%s: " directory) files)))
      (find-file (expand-file-name file root))))
  )

(defun alchemist-phoenix-find-lib ()
  (interactive)
  (alchemist-phoenix-find-dir "lib"))

(defun alchemist-phoenix-find-template ()
  (interactive)
  (alchemist-phoenix-find-dir "web/templates"))

(defun my-alchemist-phoenix-mode-config ()
  "use dat space"
  (define-key alchemist-phoenix-command-map (kbd "n b") #'alchemist-phoenix-find-lib)
  (define-key alchemist-phoenix-command-map (kbd "n p") #'alchemist-phoenix-find-template)
  )


(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

;; add to hook
(add-hook 'alchemist-iex-mode-hook 'my-alchemist-iex-mode-config)
(add-hook 'alchemist-mode-hook 'my-alchemist-mode-config)
(add-hook 'alchemist-phoenix-mode-hook 'my-alchemist-phoenix-mode-config)