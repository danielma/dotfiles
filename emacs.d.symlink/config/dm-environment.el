;;; dm-environment.el --- Process environment setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (add-to-list 'exec-path-from-shell-variables "DEVBOX_USE_VERSION")
  (add-to-list 'exec-path-from-shell-variables "TERMINFO")
  :config
  (exec-path-from-shell-initialize))

(defun dm-add-ghostty-terminfo ()
  "Teach Emacs where Ghostty terminfo lives."
  (let* ((ghostty-terminfo "/Applications/Ghostty.app/Contents/Resources/terminfo")
         (current (getenv "TERMINFO_DIRS"))
         (default-paths '("/usr/share/terminfo" "/lib/terminfo" "/usr/lib/terminfo"))
         (paths (append (if current (parse-colon-path current) nil)
                        default-paths)))
    (when (and (file-directory-p ghostty-terminfo)
               (not (member ghostty-terminfo paths)))
      (setenv "TERMINFO_DIRS"
              (mapconcat #'identity
                         (cons ghostty-terminfo paths)
                         path-separator)))))

(dm-add-ghostty-terminfo)

(provide 'dm-environment)
;;; dm-environment.el ends here
