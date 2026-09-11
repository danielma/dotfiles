;;; dm-flymake.el --- On-the-fly diagnostics -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package transient
  :ensure nil
  :config
  (transient-define-prefix dm-flymake-menu ()
    "Flymake diagnostics."
    [["Diagnostics"
      ("l" "list" consult-flymake)
      ("n" "next" flymake-goto-next-error :transient t)
      ("p" "previous" flymake-goto-prev-error :transient t)]])
  :bind ("C-c ." . dm-flymake-menu))

(defun dm-ruby-flymake-rubocop-use-bundler-p (orig-fun dir)
  "Check Gemfile.lock for Rubocop before calling ORIG-FUN with DIR."
  (let ((lock (expand-file-name "Gemfile.lock" dir)))
    (or (and (file-exists-p lock)
             (with-temp-buffer
               (insert-file-contents lock)
               (re-search-forward "^ *rubocop ([[:digit:]]" nil t)))
        (funcall orig-fun dir))))

(with-eval-after-load 'ruby-mode
  (advice-add 'ruby-flymake-rubocop--use-bundler-p
              :around #'dm-ruby-flymake-rubocop-use-bundler-p))

(use-package flymake
  :ensure nil
  :delight
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-show-diagnostics-at-end-of-line nil))

(use-package flymake-popon
  :hook (flymake-mode . flymake-popon-mode))

(provide 'dm-flymake)
;;; dm-flymake.el ends here
