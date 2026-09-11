;;; -*- lexical-binding: t -*-
;;;  ________                                                _______                 __                            __
;;; /        |                                              /       \               /  |                          /  |
;;; $$$$$$$$/ _____  ____   ______   _______  _______       $$$$$$$  | ______   ____$$ | ______   ______   _______$$ |   __
;;; $$ |__   /     \/    \ /      \ /       |/       |      $$ |__$$ |/      \ /    $$ |/      \ /      \ /       $$ |  /  |
;;; $$    |  $$$$$$ $$$$  |$$$$$$  /$$$$$$$//$$$$$$$/       $$    $$</$$$$$$  /$$$$$$$ /$$$$$$  /$$$$$$  /$$$$$$$/$$ |_/$$/
;;; $$$$$/   $$ | $$ | $$ |/    $$ $$ |     $$      \       $$$$$$$  $$    $$ $$ |  $$ $$ |  $$/$$ |  $$ $$ |     $$   $$<
;;; $$ |_____$$ | $$ | $$ /$$$$$$$ $$ \_____ $$$$$$  |      $$ |__$$ $$$$$$$$/$$ \__$$ $$ |     $$ \__$$ $$ \_____$$$$$$  \
;;; $$       $$ | $$ | $$ $$    $$ $$       /     $$/       $$    $$/$$       $$    $$ $$ |     $$    $$/$$       $$ | $$  |
;;; $$$$$$$$/$$/  $$/  $$/ $$$$$$$/ $$$$$$$/$$$$$$$/        $$$$$$$/  $$$$$$$/ $$$$$$$/$$/       $$$$$$/  $$$$$$$/$$/   $$/

;;; Minimal init.el

;;; Contents:
;;;
;;;  - Basic settings
;;;  - Discovery aids
;;;  - Minibuffer/completion/searching settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Optional extras
;;;  - Built-in customization framework

;;; Guardrail

(when (< emacs-major-version 31)
  (error "Emacs Bedrock only works with Emacs 31 and newer; you have version %s" emacs-major-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package initialization
;;
;; Emacs ships with a bunch of Emacs Lisp package archives ("ELPAs")
;; pre-configured. The MELPA archive is the biggest package archive
;; out there. Most of the packages Bedrock uses in the extras/ folder
;; come from the built-in ELPAs, but a few (notably Citar in
;; extras/researcher.el) are on MELPA.
;;
;; These lines add MELPA to the list of ELPAs that Emacs will read.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; If you want to turn off the welcome screen, uncomment this
                                        ;(setopt inhibit-splash-screen t)

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes by
;; using the OS file change notification interface rather than
;; repeatedly polling to see if there are changes.
;;
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
;; Set this to `nil' if Emacs is having trouble picking up changes.
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer: future invocations will have
;; recently-used selections sorted first
(savehist-mode)

;; Save existing clipboard content to the kill ring---useful if you've
;; copied something from an external program and then kill some text
;; in Emacs shortly after. Also, deduplicate kill ring contents.
(setopt save-interprogram-paste-before-kill t)
(setopt kill-do-not-save-duplicates t)

;; Don't ping url-looking things when running find-file
(setopt ffap-machine-p-known 'reject)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Rebalance windows automatically when splitting
(setopt window-combination-resize t)

;; On macOS, make the first click raise the window but don't
;; reposition the cursor to where the click happened.
(setopt ns-click-through nil)

;; Prefer horizontal split on landscape monitors: `longest' is
;; default; can be `vertical' or `horizontal'.
;; See also the variable `split-width-threshold'.
(setopt split-window-preferred-direction 'longest)

;; Fix archaic defaults; justification: https://practicaltypography.com/one-space-between-sentences.html
(setopt sentence-end-double-space nil)

;; Make all confirmation prompts use `y' or `n'. Default is for some
;; prompts to ask for a full `yes' or `no' when the operation is
;; potentially dangerous. Commented out to keep the safer behavior.
                                        ; (setopt use-short-answers t)

;; Make right-click do something sensible and shift-drag behave better
(when (display-graphic-p)
  (mouse-shift-adjust-mode)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;; The above creates nested directories in the backup folder. If
;; instead you would like all backup files in a flat structure, albeit
;; with their full paths concatenated into a filename, then you can
;; use the following configuration:
;; (Run `'M-x describe-variable RET backup-directory-alist RET' for more help)
;;
;; (let ((backup-dir (expand-file-name "emacs-backup/" user-emacs-directory)))
;;   (setopt backup-directory-alist `(("." . ,backup-dir))))

;; Basic speedups
;;
;; Emacs works really hard to be incredibly compatible out-of-the-box
;; with a wide variety of languages. That comes at the cost of a
;; little performance. These tell Emacs to assume left-to-right text
;; in all buffers.
;; Remove/comment if you read right-to-left languages (Arabic, Hebrew, etc.)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the help buffer after startup---makes it a little bit like nano
(add-hook 'after-init-hook 'help-quick)
(setopt view-lossage-auto-refresh t)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion/searching settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt minibuffer-visible-completions t)              ; Use ↑↓ to select candidates
(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is an arbitrary value
(setopt completions-format 'one-column)                ; Makes it easier to scroll
(setopt completions-group t)

;; Eager completion setup: show *Completions* buffer immediately
(setopt completion-auto-select 'second-tab)            ; Much more eager
(setopt completion-eager-display t)                    ; Show the completions buffer immediately
(setopt completion-eager-update t)                     ; Update display as-you-type

;; Uncomment to get automatic inline completion previews
(completion-preview-mode)

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

                                        ;(icomplete-vertical-mode)
                                        ;(fido-vertical-mode)
                                        ;(setopt icomplete-delay-completions-threshold 4000)


;; isearch is Emacs's built-in searching system
(use-package isearch
  :ensure nil                           ; already installed
  :bind
  (:map isearch-mode-map
        ("C-." . isearch-forward-thing-at-point)) ; Search for thing under cursor
  :custom
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-lazy-count t)                 ; show match count
  (isearch-allow-motion t)
  (isearch-allow-scroll t)               ; lets you scroll without breaking search
  (isearch-repeat-on-direction-change t) ; C-r immediately goes to previous match
  (isearch-wrap-pause 'no-ding)          ; Automatically wrap search to top
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well
(setopt mode-line-collapse-minor-modes t)          ; Hide minor modes behind a compact indicator

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; Update the cursor shape inside a terminal; e.g. when in insert mode
;; when using Evil (Vim emulation) change the cursor to a bar.
(setopt xterm-update-cursor t)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling
;; If you use a mouse and scrolling seems a little jittery, you might
;; want to set this to `nil':
                                        ;(setopt pixel-scroll-precision-interpolate-mice nil)

;; Use common keystrokes by default
(cua-mode)

;; Makes it easier to repeat commands; `C-x o C-x o' becomes `C-x o o'
;; See https://karthinks.com/software/it-bears-repeating/
(repeat-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

(setopt global-hl-line-sticky-flag 'window) ; Every window gets own hl-line instance
(global-hl-line-mode)

;; Use this to enable the line highlight in only certain modes:
                                        ;(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
                                        ;  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Show matching delimiters
(setopt show-paren-delay 0)
(setopt show-paren-mode t)
(setopt show-paren-style 'expression)   ; default is 'parenthesis and just does delimiters
(setopt show-paren-context-when-offscreen 'overlay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;; A transient menu to make working with the tab-bar easier
;; The `transient' library is built-in and makes defining little menus
;; easy to work with. Activate this menu with `C-c C-t'.
(use-package transient
  :ensure nil                           ; built-in
  :config
  ;; You can define as many of these as you like
  (transient-define-prefix tab-bar-transient ()
    "Tab-bar menu"
    [["Creation"
      ("t" "new tab" tab-bar-new-tab)
      ("n" "next command in new tab" other-tab-prefix)]
     ["Movement"
      ("j" "jump to tab" tab-switch)
      ("h" "move left" tab-bar-move-tab-backward :transient t)
      ("l" "move right" tab-bar-move-tab :transient t)]]
    [["Management"
      ("r" "rename tab" tab-rename)]]
    [[""
      ("RET" "Done" transient-quit-one)]])
  :bind (:map global-map
              ("C-c C-t" . tab-bar-transient)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (load-theme 'modus-vivendi))          ; for light theme, use modus-operandi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
                                        ;(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
                                        ;(load-file (expand-file-name "extras/dev.el" user-emacs-directory))

;; Vim-bindings in Emacs (evil-mode configuration)
                                        ;(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
;; WARNING: need to customize things inside the elisp file before use! See
;; the file extras/org-intro.txt for help.
                                        ;(load-file (expand-file-name "extras/org.el" user-emacs-directory))

;; Email configuration in Emacs
;; WARNING: needs the `mu' program installed; see the elisp file for more
;; details.
                                        ;(load-file (expand-file-name "extras/email.el" user-emacs-directory))

;; Tools for academic researchers
                                        ;(load-file (expand-file-name "extras/researcher.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(citar-typst magit meow which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold (or bedrock--initial-gc-threshold 800000))

(use-package magit)

(add-to-list 'load-path "~/.emacs.d/config")

(require 'dm-meow)
