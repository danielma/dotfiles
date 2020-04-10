;;; dm-rspec --- Provides rspec mode

;;; Commentary:

;;; Code:

(defvar-local rspec-project-supports-spec-data 'undefined)

(defvar rspec-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defvar rspec-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'rspec-next-failing-spec)
    (define-key map (kbd "p") #'rspec-prev-failing-spec)
    map))

(defun rspec--root ()
  "Get the root of the project."
  (if (projectile-project-p)
      (projectile-project-root)
    default-directory))

(defun rspec--examples-file ()
  "The examples file."
  ; if this was implemented correctly it was consider rspec.config.example_status_persistence_file_path
  (concat (rspec--root) "spec/examples.txt"))

(defun rspec--project-supports-spec-data-p ()
  "Does the project support spec data?"

  (if (eq rspec-project-supports-spec-data 'undefined)
      (setq rspec-project-supports-spec-data
            (file-readable-p (rspec--examples-file))))

  rspec-project-supports-spec-data)

(defun rspec--indexed-map ()
  (save-excursion
    (let ((alist '()))
      (goto-char (point-min))
      ;; (re-search-forward "describe")
      (setq alist (nconc (rspec--index-map-of-block (point)) alist))
      alist))
  )

(defun rspec--index-map-of-block (beg)
  (goto-char beg)
  (let* ((alist '())
         (current-item nil)
         (indent-level (current-indentation))
         (end-point (re-search-forward (concat "^ \\{" (number-to-string indent-level) "\\}end$"))))
    (goto-char beg)
    (while (setq current-item (rspec--index-map-next-item (point) end-point))
      (add-to-list 'alist current-item))
    (reverse alist)))

(defun rspec--index-map-next-item (beg end-point)
  (goto-char beg)

  (if (re-search-forward "^\\( *\\)\\(RSpec\.\\)?\\(its?\\|describe\\|context\\) \\(.+\\)" end-point t)
      (let ((indent (match-string-no-properties 1))
            (spec-kind (intern (match-string-no-properties 3)))
            (description (match-string-no-properties 4))
            (location (- (point) 1)))
        (if (eq 'it spec-kind)
            (progn
              (if (s-ends-with? "do" description)
                  (re-search-forward (concat "^" indent "end$")))
              (list spec-kind location description))
          (list spec-kind location description (rspec--index-map-of-block (point)))))))

(defun rspec--failing-specs ()
  "Get a list of failing specs for the current file."
  (if (rspec--project-supports-spec-data-p)
      (let* ((examples (with-temp-buffer (insert-file-contents (rspec--examples-file)) (buffer-substring-no-properties (point-min) (point-max))))
             (failing-examples (--filter (s-contains? "| failed" it) (s-lines examples)))
             (filepath (file-relative-name (buffer-file-name) (rspec--root)))
             (in-this-file (--filter (s-starts-with? (concat "./" filepath) it) failing-examples)))
        (--map (let* ((matches (s-match "\\[\\([0-9:]+\\)" it))
                      (coordinate-section (nth 1 matches)))
                 (--map (string-to-number it) (s-split ":" coordinate-section)))
               in-this-file))))

(defun rspec--spec-buffer-coordinate (map coordinates)
  (--each coordinates
    (let ((kind (car map)))
      (setq map
            (cond ((or (eq kind 'describe) (eq kind 'context))
                   (nth (- it 1) (car (last map))))
                  ((eq kind 'it) map)
                  (t (nth (- it 1) map))))))
  (nth 1 map))

(defun rspec-next-failing-spec ()
  "Visit the next failing spec."
  (interactive)
  (let* ((spec-coordinates (rspec--failing-specs))
         (map (rspec--indexed-map))
         (buffer-coordinates (--map (rspec--spec-buffer-coordinate map it) spec-coordinates))
         (next (--first (> it (point)) buffer-coordinates)))
    (if next (goto-char next))))

(defun rspec-prev-failing-spec ()
  "Visit the previous failing spec."
  (interactive)
  (let* ((spec-coordinates (rspec--failing-specs))
         (map (rspec--indexed-map))
         (buffer-coordinates (--map (rspec--spec-buffer-coordinate map it) spec-coordinates))
         (prev (--first (< it (point)) (reverse buffer-coordinates))))
    (if prev (goto-char prev))))

(define-derived-mode rspec-mode ruby-mode "RSpec"
  "Major mode for specs."
  (flycheck-add-mode 'ruby-rubocop 'rspec-mode))

(add-to-list 'auto-mode-alist
             (cons (purecopy "_spec\\.rb\\'") 'rspec-mode))

(provide 'dm-rspec)
