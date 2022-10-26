;;; dm-colors.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my/doom-theme-setup ()
  (load-theme 'doom-gruvbox))

(use-package doom-themes
  :hook (after-init . my/doom-theme-setup))

(use-package emacs
  :custom
  (line-spacing 0)
  :custom-face
  (default ((t (:height 140 :width regular :weight regular :family "JetBrains Mono"))))
  (tab-bar ((t :inherit default :box (:line-width (0 . 8)))))
  )

;; (load-theme 'modus-operandi)

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

  (if (display-graphic-p)
      (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend))

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
