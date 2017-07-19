;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (with-temp-buffer
;;               (when (getenv "NVM_DIR")
;;                 (let* ((nvm-dir (getenv "NVM_DIR"))
;;                        (default (concat nvm-dir "/alias/default")))
;;                   (insert-file-contents default)
;;                   (let* ((version (s-trim (buffer-string)))
;;                          (bindir (concat nvm-dir "/versions/node/" version "/bin")))
;;                     (exec-path-from-shell-setenv "PATH" (concat (getenv "PATH") ":" bindir)))))))) ;
