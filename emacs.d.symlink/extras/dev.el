;;; -*- lexical-binding: t -*-
;;;
;;; Emacs Bedrock
;;;
;;; Extra config: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el config if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs
;;;  - Templating

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Code folding config
  ;(setopt hs-show-indicators t)         ; Show collapse indicators in margin
  ;(setopt hs-display-lines-hidden t)    ; Show number of collapsed lines


  ;; Treesitter config

  ;; Enable tree-sitter in all available modes
  (setopt treesit-enabled-modes t)

  ;; Amount to highlight: integer between 1-4; 4 is max highlighting
  (setopt treesit-font-lock-level 3)

  ;; What to do if language grammar not installed: default is `ask';
  ;; other options are `always', and `ask-dir'.
  (setopt treesit-auto-install-grammar 'ask)

  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

(use-package project
  :custom
  (when (>= emacs-major-version 30)
    (project-mode-line t)))         ; show project name in modeline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode)

(use-package json-mode)

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot-ensure))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  ;; Avoid changing line heights if your font is wonky. See
  ;; https://github.com/joaotavora/eglot/discussions/1492
  (setopt eglot-code-action-indicator "h")

  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))

  ;; You can set various options for each language server. For
  ;; example, you can raise the number of completions surfaced by a
  ;; given langauge server to Emacs:
  (setopt eglot-workspace-configuration
	  '((haskell (maxCompletions . 100))
	    (elixir  (maxCompletions . 100))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Templating
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tempel
  ;; By default, tempel looks at the file "templates" in
  ;; user-emacs-directory, but you can customize that with the
  ;; tempel-path variable:
  ;; :custom
  ;; (tempel-path (concat user-emacs-directory "custom_template_file"))
  :bind (("M-*" . tempel-insert)
         ("M-+" . tempel-complete)
         :map tempel-map
         ("C-c RET" . tempel-done)
         ("C-<down>" . tempel-next)
         ("C-<up>" . tempel-previous)
         ("M-<down>" . tempel-next)
         ("M-<up>" . tempel-previous))
  :init
  ;; Make a function that adds the tempel expansion function to the
  ;; list of completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  ;; Put tempel-expand on the list whenever you start programming or
  ;; writing prose.
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))
