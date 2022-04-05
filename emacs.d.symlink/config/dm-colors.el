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

(let* ((themename (s-chomp (get-string-from-file "~/.base16_theme-name")))
       (theme (intern (concat "base16-" themename))))
  (load-theme (if (member theme (custom-available-themes))
		  theme
		'base16-default-dark) t))

(custom-set-faces
 '(default ((t (:weight regular :height 140 :width regular :family "JetBrains Mono")))))

(if (display-graphic-p)
    (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend))

(set-frame-parameter (selected-frame) 'alpha 100)

(use-package emojify
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode))
  )

(global-prettify-symbols-mode 1)

(use-package emacs
  :custom
  (line-spacing 0))

(provide 'dm-colors)
;;; dm-colors.el ends here
