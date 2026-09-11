;;; dm-javascript.el --- JavaScript and TypeScript support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package js
  :ensure nil
  :config
  (makunbound 'js-indent-level)
  (defvaralias 'js-indent-level 'tab-width))

(use-package flymake-jsts
  :vc (:url "https://github.com/orzechowskid/flymake-jsts.git"
       :branch "main")
  :defer t)

(defun dm-javascript-use-oxlint-root-markers ()
  "Use Oxlint-specific project markers in the current buffer."
  (setq-local flymake-jsts-project-markers-alist
              (copy-tree flymake-jsts-project-markers-alist))
  (setf (alist-get 'oxlint flymake-jsts-project-markers-alist)
        '(".oxlintrc.json" ".oxlintrc.jsonc" "oxlint.json" "oxlint.jsonc"
          "node_modules/.bin/oxlint")))

(defun dm-javascript-use-local-oxlint ()
  "Use the current project's Oxlint executable when available."
  (let* ((root (locate-dominating-file default-directory "node_modules"))
         (oxlint (and root (expand-file-name "node_modules/.bin/oxlint" root))))
    (dm-javascript-use-oxlint-root-markers)
    (setq-local flymake-jsts-executable-name-alist
                `((eslint . "eslint_d")
                  ,@(when (and oxlint (file-executable-p oxlint))
                      `((oxlint . ,oxlint)))
                  (biome . "biome")))))

(defun dm-javascript-oxlint-command (file-name source-buffer)
  "Build an Oxlint command for FILE-NAME relative to SOURCE-BUFFER's lint root."
  (let* ((cwd (flymake-jsts/get-process-cwd 'oxlint source-buffer))
         (lint-path (if cwd (file-relative-name file-name cwd) file-name)))
    (list (cdr (assoc 'oxlint flymake-jsts-executable-name-alist))
          "-f" "json" lint-path)))

(with-eval-after-load 'flymake-jsts-oxlint
  (advice-add 'flymake-jsts/oxlint-get-command
              :override #'dm-javascript-oxlint-command))

(defun dm-javascript-enable-linters ()
  "Enable project-local JavaScript linters in Eglot-managed buffers."
  (when (derived-mode-p 'js-base-mode 'typescript-ts-base-mode)
    (require 'flymake-jsts)
    (dm-javascript-use-local-oxlint)
    (when (assoc 'oxlint flymake-jsts-executable-name-alist)
      (flymake-jsts-oxlint-enable))
    (flymake-jsts-eslint-enable)))

(add-hook 'eglot-managed-mode-hook #'dm-javascript-enable-linters)

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.mts\\'" . typescript-ts-mode)
         ("\\.cts\\'" . typescript-ts-mode))
  :hook
  (typescript-ts-base-mode . eglot-ensure)
  (typescript-ts-base-mode . dm-guard-mode))

(provide 'dm-javascript)
;;; dm-javascript.el ends here
