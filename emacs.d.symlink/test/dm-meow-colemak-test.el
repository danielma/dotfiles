;;; dm-meow-colemak-test.el --- Tests for positional Colemak bindings -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              "../config"
              (file-name-directory (or load-file-name buffer-file-name))))
(require 'dm-meow-colemak)

(ert-deftest dm-meow-colemak-keeps-normal-commands-under-the-same-fingers ()
  (should (eq (lookup-key meow-normal-state-keymap (kbd "n")) #'meow-next))
  (should (eq (lookup-key meow-normal-state-keymap (kbd "e")) #'meow-prev))
  (should (eq (lookup-key meow-normal-state-keymap (kbd "m")) #'meow-left))
  (should (eq (lookup-key meow-normal-state-keymap (kbd "i")) #'meow-right))
  (should (eq (lookup-key meow-normal-state-keymap (kbd "u")) #'meow-insert))
  (should (eq (lookup-key meow-normal-state-keymap (kbd "j")) #'meow-save))
  (should (eq (lookup-key meow-normal-state-keymap (kbd ":")) #'meow-yank))
  (should (eq (lookup-key meow-normal-state-keymap (kbd "O"))
              #'execute-extended-command))
  (should (eq (lookup-key meow-normal-state-keymap (kbd "_"))
              #'negative-argument)))

(ert-deftest dm-meow-colemak-keeps-motion-commands-under-the-same-fingers ()
  (should (eq (lookup-key meow-motion-state-keymap (kbd "n")) #'meow-next))
  (should (eq (lookup-key meow-motion-state-keymap (kbd "e")) #'meow-prev)))

(ert-deftest dm-meow-colemak-moves-owned-leader-bindings-positionally ()
  (should (keymapp (lookup-key mode-specific-map (kbd "v"))))
  (should (commandp (lookup-key mode-specific-map (kbd ":"))))
  (should (eq (lookup-key dm-buffer-command-map (kbd "e"))
              #'kill-current-buffer))
  (should (eq (lookup-key dm-snippet-command-map (kbd "u"))
              #'yas-insert-snippet))
  (should (eq (lookup-key dm-snippet-command-map (kbd "k"))
              #'yas-new-snippet))
  (should (eq (lookup-key dm-snippet-command-map (kbd "d"))
              #'yas-visit-snippet-file)))

(ert-deftest dm-meow-colemak-cheatsheet-matches-the-customized-qmk-layer ()
  (should (equal (assq '<AD10> meow-cheatsheet-layout)
                 '(<AD10> ":" ";")))
  (should (equal (assq '<AC02> meow-cheatsheet-layout)
                 '(<AC02> "s" "S")))
  (should (equal (assq '<AC03> meow-cheatsheet-layout)
                 '(<AC03> "r" "R"))))

;;; dm-meow-colemak-test.el ends here
