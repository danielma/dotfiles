(require 'elixir-mode)
(require 'flycheck)

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

(defun my/alchemist-phoenix-find-lib ()
  (interactive)
  (my/projectile-find-resource
   "lib: "
   '(("lib" "lib/\\(.+\\)\.exs?$"))
   "lib/${filename}.ex"))

(defun my/alchemist-phoenix-find-view ()
  (interactive)
  (my/projectile-find-resource
   "view: "
   '(("web/views" "/views/\\(.+\\)\.exs?$"))
   "web/views/${filename}.ex"))

(defun my/alchemist-phoenix-find-controller ()
  (interactive)
  (my/projectile-find-resource
   "controller: "
   '(("web/controllers" "/controllers/\\(.+\\)\.exs?$"))
   "web/controllers/${filename}.ex"))

(defun my/alchemist-phoenix-find-model ()
  (interactive)
  (my/projectile-find-resource
   "model: "
   '(("web/models" "/models/\\(.+\\)\.exs?$"))
   "web/models/${filename}.ex"))

(defun my/alchemist-phoenix-find-template ()
  (interactive)
  (my/projectile-find-resource
   "template: "
   '(("web/templates" "/templates/\\(.+\\)$"))
   "web/templates/${filename}"))

(defun my/alchemist-phoenix-find-web ()
  (interactive)
  (my/projectile-find-resource
   "web: "
   '(("web" "web/\\(.+\\)$"))
   "web/${filename}"))

(evil-define-minor-mode-key
  'normal
  'alchemist-phoenix-mode
  (kbd (concat evil-leader/leader "r"))
  (let ((map (make-sparse-keymap)))
    (define-key map "l" #'my/alchemist-phoenix-find-lib)
    (define-key map "w" #'my/alchemist-phoenix-find-web)
    (define-key map "v" #'my/alchemist-phoenix-find-view)
    (define-key map "c" #'my/alchemist-phoenix-find-controller)
    (define-key map "h" #'alchemist-phoenix-find-channels)
    (define-key map "t" #'my/alchemist-phoenix-find-template)
    (define-key map "m" #'my/alchemist-phoenix-find-model)
    (define-key map "s" #'alchemist-phoenix-find-static)
    (define-key map "r" #'alchemist-phoenix-router)
    (define-key map "R" #'alchemist-phoenix-routes)
    map)
  )

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
