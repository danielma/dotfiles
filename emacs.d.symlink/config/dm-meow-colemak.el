;;; dm-meow-colemak.el --- Positional Meow bindings for Colemak -*- lexical-binding: t -*-

;;; Commentary:

;; Keep the commands from `dm-meow' under the same fingers when the customized
;; Colemak-DH layer in qmk/ergodox/keymap.c is active.  To use this layout,
;; require `dm-meow-colemak' instead of `dm-meow' from init.el.

;;; Code:

(require 'dm-meow)

(defconst dm-meow-colemak-key-translation
  '(("e" . "f") ("E" . "F")
    ("r" . "p") ("R" . "P")
    ("t" . "b") ("T" . "B")
    ("y" . "j") ("Y" . "J")
    ("u" . "l") ("U" . "L")
    ("i" . "u") ("I" . "U")
    ("o" . "y") ("O" . "Y")
    ("p" . ":") ("P" . ";")
    ("d" . "r") ("D" . "R")
    ("f" . "t") ("F" . "T")
    ("h" . "m") ("H" . "M")
    ("j" . "n") ("J" . "N")
    ("k" . "e") ("K" . "E")
    ("l" . "i") ("L" . "I")
    (";" . "o") (":" . "O")
    ("v" . "d") ("V" . "D")
    ("b" . "v") ("B" . "V")
    ("n" . "k") ("N" . "K")
    ("m" . "h") ("M" . "H")
    ("-" . "_"))
  "Map QWERTY keys to the characters emitted from the same physical keys.")

(defun dm-meow-colemak--translate-key (key)
  "Return the customized Colemak character at QWERTY position KEY."
  (or (alist-get key dm-meow-colemak-key-translation nil nil #'string=)
      key))

(defun dm-meow-colemak--move-keys (keymap keys)
  "Move bindings in KEYMAP at QWERTY KEYS to their Colemak positions."
  (let ((bindings
         (mapcar (lambda (key)
                   (cons (dm-meow-colemak--translate-key key)
                         (lookup-key keymap (kbd key))))
                 keys)))
    (dolist (key keys)
      (unless (equal key (dm-meow-colemak--translate-key key))
        (define-key keymap (kbd key) nil)))
    (dolist (binding bindings)
      (unless (or (memq (cdr binding) '(nil undefined))
                  (numberp (cdr binding)))
        (define-key keymap (kbd (car binding)) (cdr binding))))))

(defun dm-meow-colemak--cheatsheet-layout ()
  "Return a cheatsheet layout matching the customized QMK Colemak layer."
  (mapcar
   (lambda (key)
     (list (nth 0 key)
           (dm-meow-colemak--translate-key (nth 1 key))
           (dm-meow-colemak--translate-key (nth 2 key))))
   meow-cheatsheet-layout-qwerty))

(defun dm-meow-colemak-setup ()
  "Move the current Meow bindings to matching physical Colemak positions."
  (setq meow-cheatsheet-layout (dm-meow-colemak--cheatsheet-layout))

  (dm-meow-colemak--move-keys
   meow-normal-state-keymap
   (mapcar #'car dm-meow-colemak-key-translation))
  (dm-meow-colemak--move-keys meow-motion-state-keymap '("j" "k"))

  ;; Only move the leader bindings owned by dm-meow.  The leader keymap is
  ;; shared with C-c, so translating the whole map would move package bindings.
  (dm-meow-colemak--move-keys mode-specific-map '("b" "s" "p" "w"))
  (dm-meow-colemak--move-keys dm-buffer-command-map '("k" "s"))
  (dm-meow-colemak--move-keys dm-snippet-command-map '("i" "n" "v")))

(dm-meow-colemak-setup)

(provide 'dm-meow-colemak)

;;; dm-meow-colemak.el ends here
