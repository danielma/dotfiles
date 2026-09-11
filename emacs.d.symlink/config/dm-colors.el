;;; dm-colors.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar dm-light-theme 'doom-opera-light)
(defvar dm-dark-theme 'doom-gruvbox)

(defun dm-apply-theme (appearance)
  "Apply theme based on APPEARANCE ('light or 'dark)."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme dm-light-theme t))
    ('dark (load-theme dm-dark-theme t))))

(defun dm-system-appearance-changed (appearance)
  "Hook for ns-system-appearance-change-functions."
  (dm-apply-theme appearance))

(defvar dm-terminal-background-appearance nil
  "Cached terminal background appearance detected via OSC 11.")

(defun dm-osc-11-response-appearance (response)
  "Return light/dark appearance parsed from an OSC 11 RESPONSE string."
  (when (string-match "\e]11;rgb:\\([[:xdigit:]]+\\)/\\([[:xdigit:]]+\\)/\\([[:xdigit:]]+\\)" response)
    (let* ((r-hex (match-string 1 response))
           (g-hex (match-string 2 response))
           (b-hex (match-string 3 response))
           (r (/ (string-to-number r-hex 16) (float (1- (expt 16 (length r-hex))))))
           (g (/ (string-to-number g-hex 16) (float (1- (expt 16 (length g-hex))))))
           (b (/ (string-to-number b-hex 16) (float (1- (expt 16 (length b-hex))))))
           (luminance (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))
      (if (> luminance 0.5) 'light 'dark))))

(defun dm-read-terminal-osc-response (&optional timeout)
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
        (when (or (string-match-p "\e\\\\" response)
                  (string-match-p "\a" response))
          (throw 'done response)))
      response)))

(defun dm-terminal-osc-11-appearance ()
  "Query the terminal background color with OSC 11 and infer light/dark."
  (or dm-terminal-background-appearance
      (when (and (not noninteractive) (not (display-graphic-p)))
        (send-string-to-terminal "\e]11;?\e\\")
        (setq dm-terminal-background-appearance
              (dm-osc-11-response-appearance
               (dm-read-terminal-osc-response 0.2))))))

(defun dm-current-appearance ()
  "Return current light/dark appearance."
  (or (and (memq (bound-and-true-p ns-system-appearance) '(light dark))
           ns-system-appearance)
      (dm-terminal-osc-11-appearance)
      'light))

(use-package doom-themes
  :config
  (dm-apply-theme (dm-current-appearance))
  (when (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions #'dm-system-appearance-changed)))

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

(provide 'dm-colors)
;;; dm-colors.el ends here
