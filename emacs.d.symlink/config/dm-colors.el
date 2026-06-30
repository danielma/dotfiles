;;; dm-colors.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar my/light-theme 'doom-opera-light)
(defvar my/dark-theme 'doom-gruvbox)

(defun my/apply-theme (appearance)
  "Apply theme based on APPEARANCE ('light or 'dark)."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme my/light-theme t))
    ('dark (load-theme my/dark-theme t))))

(defun my/system-appearance-changed (appearance)
  "Hook for ns-system-appearance-change-functions."
  (my/apply-theme appearance))

(defvar my/terminal-background-appearance nil
  "Cached terminal background appearance detected via OSC 11.")

(defun my/osc-11-response-appearance (response)
  "Return light/dark appearance parsed from an OSC 11 RESPONSE string."
  (when (string-match "]11;rgb:\\([[:xdigit:]]+\\)/\\([[:xdigit:]]+\\)/\\([[:xdigit:]]+\\)" response)
    (let* ((r-hex (match-string 1 response))
           (g-hex (match-string 2 response))
           (b-hex (match-string 3 response))
           (r (/ (string-to-number r-hex 16) (float (1- (expt 16 (length r-hex))))))
           (g (/ (string-to-number g-hex 16) (float (1- (expt 16 (length g-hex))))))
           (b (/ (string-to-number b-hex 16) (float (1- (expt 16 (length b-hex))))))
           (luminance (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))
      (if (> luminance 0.5) 'light 'dark))))

(defun my/read-terminal-osc-response (&optional timeout)
  "Read a terminal OSC response, waiting up to TIMEOUT seconds."
  (let ((end (+ (float-time) (or timeout 0.2)))
        (response "")
        event)
    (catch 'done
      (while (< (float-time) end)
        (setq event (read-event nil nil (max 0 (- end (float-time)))))
        (cond
         ((null event)
          (throw 'done response))
         ((characterp event)
          (setq response (concat response (string event))))
         ((stringp event)
          (setq response (concat response event)))
         (t
          (push event unread-command-events)
          (throw 'done response)))
        (when (or (string-match-p "\\\\" response)
                  (string-match-p "" response))
          (throw 'done response)))
      response)))

(defun my/terminal-osc-11-appearance ()
  "Query the terminal background color with OSC 11 and infer light/dark."
  (or my/terminal-background-appearance
      (when (and (not noninteractive) (not (display-graphic-p)))
        (send-string-to-terminal "\e]11;?\e\\")
        (setq my/terminal-background-appearance
              (my/osc-11-response-appearance
               (my/read-terminal-osc-response 0.2))))))

(defun my/current-appearance ()
  "Return current light/dark appearance."
  (or (and (memq (bound-and-true-p ns-system-appearance) '(light dark))
           ns-system-appearance)
      (my/terminal-osc-11-appearance)
      'light))

(use-package doom-themes
  :config
  (my/apply-theme (my/current-appearance))
  (when (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions #'my/system-appearance-changed)))

(use-package emacs
  :custom
  (line-spacing 0)
  :custom-face
  (default ((t (:height 140 :width regular :weight regular :family "Iosevka Nerd Font Mono"))))
  :config
  (when (display-graphic-p)
    ;; (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
    (set-fontset-font t nil "SF Pro Display" nil 'append))
  )

(use-package indent-bars
  :if (display-graphic-p)
  :hook ((prog-mode yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t))


;; (load-theme 'modus-operandi)
;; (use-package ef-themes)

(if t
    t

  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'custom-theme-load-path "~/Code/test/base16-builder/templates/emacs/build")

  (use-package bespoke-themes
    :straight (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
    :custom
    (bespoke-set-variable-pitch t)
    )

  (defun my/base16-set-theme (theme)
    "Set a base16 THEME by unloading all others."
    (interactive
     (list
      (completing-read "Load custom theme: "
                       (--map (s-chop-prefix "base16-" it)
                              (--filter (string-prefix-p "base16-" it)
                                        (mapcar
                                         'symbol-name
                                         (custom-available-themes)))))
      )
     )

    (--each
        (--filter (string-prefix-p "base16-" it) (mapcar 'symbol-name custom-enabled-themes))
      (disable-theme (intern it)))
    (custom-push-theme 'theme-face 'default 'user 'reset)
    (load-theme (intern (concat "base16-" theme)) t)
    )

  (defun get-string-from-file (filePath)
    "Return FILEPATH's file content."
    (if (file-exists-p filePath)
        (with-temp-buffer
	        (insert-file-contents filePath)
	        (buffer-string))))

  ;; (let* ((themename (s-chomp (get-string-from-file "~/.base16_theme-name")))
  ;;        (theme (intern (concat "base16-" themename))))
  ;;   (load-theme (if (member theme (custom-available-themes))
  ;; 		  theme
  ;; 		'base16-default-dark) t))

  (use-package ewal
    :init (setq ewal-use-built-in-always-p nil
                ewal-use-built-in-on-failure-p t
                ewal-built-in-palette "sexy-material"))
  (use-package ewal-spacemacs-themes
    :init (progn
            (setq spacemacs-theme-underline-parens t)
            ;; my:rice:font (font-spec
            ;;               :family "Source Code Pro"
            ;;               :weight 'semi-bold
            ;;               :size 11.0))
            ;; (show-paren-mode +1)
            ;; (global-hl-line-mode)
            ;; (set-frame-font my:rice:font nil t)
            ;; (add-to-list  'default-frame-alist
            ;;               `(font . ,(font-xlfd-name my:rice:font))))
            )
    :config (progn
              (load-theme 'ewal-spacemacs-modern t)
              (enable-theme 'ewal-spacemacs-modern)))
  ;; (use-package ewal-evil-cursors
  ;;   :after (ewal-spacemacs-themes)
  ;;   :config (ewal-evil-cursors-get-colors
  ;;            :apply t :spaceline t))
  ;; (use-package spaceline
  ;;   :after (ewal-evil-cursors winum)
  ;;   :init (setq powerline-default-separator nil)
  ;;   :config (spaceline-spacemacs-theme))

  ;; (custom-set-faces
  ;;  '(default ((t (:weight regular :height 140 :width regular :family "Rec Mono Duotone")))))

  (set-frame-parameter (selected-frame) 'alpha 95)

  (use-package emojify
    :custom
    (emojify-display-style 'unicode)
    (emojify-emoji-styles '(unicode))
    )

  (global-prettify-symbols-mode 1)

  )

(provide 'dm-colors)
;;; dm-colors.el ends here
