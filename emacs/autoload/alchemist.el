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
  (evil-leader/set-key-for-mode 'alchemist-phoenix-mode
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

(setq my/projectile-phoenix-command-map
      (let ((map (make-sparse-keymap)))
        (define-key map "b" #'alchemist-phoenix-find-lib)
        (define-key map "p" #'alchemist-phoenix-find-template)
        (define-key map "w" #'alchemist-phoenix-find-web)
        (define-key map "w" #'alchemist-phoenix-find-web)
        (define-key map "v" #'alchemist-phoenix-find-views)
        (define-key map "c" #'alchemist-phoenix-find-controllers)
        (define-key map "l" #'alchemist-phoenix-find-channels)
        (define-key map "t" #'alchemist-phoenix-find-templates)
        (define-key map "m" #'alchemist-phoenix-find-models)
        (define-key map "s" #'alchemist-phoenix-find-static)
        (define-key map "r" #'alchemist-phoenix-router)
        (define-key map "R" #'alchemist-phoenix-routes)
        map))

(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

(flycheck-define-checker elixir-credo
  "Defines a checker for elixir with credo"
  :command ("mix" "credo" "--format" "flycheck" source-original)
  :standard-input t
  :working-directory (lambda (checker)
                       (locate-dominating-file default-directory "mix.exs"))
  :error-patterns
  (
   (info line-start (file-name) ":" line ":" column ": " (or "F" "R" "C")  ": " (message) line-end)
   (info line-start (file-name) ":" line ": " (or "F" "D" "R" "C" "W")  ": " (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": " (or "D" "W")  ": " (message) line-end)
   (warning line-start (file-name) ":" line ": " (or "D" "W")  ": " (message) line-end)
   )
  :modes (elixir-mode)
  :next-checkers ((warning . elixir-dogma))
)

(defun flycheck-elixir-credo-setup ()
  "Setup Flycheck for Elixir Credo."
  (add-to-list 'flycheck-checkers 'elixir-credo))

;; add to hook
(add-hook 'alchemist-iex-mode-hook 'my-alchemist-iex-mode-config)
(add-hook 'alchemist-mode-hook 'my-alchemist-mode-config)
(add-hook 'alchemist-mode-hook 'flycheck-elixir-credo-setup)
(evil-define-minor-mode-key
  'normal
  'alchemist-phoenix-mode
  (kbd (concat evil-leader/leader "r"))
  my/projectile-phoenix-command-map)
