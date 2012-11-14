(defun run-rspec (use-current-line use-new-shell)
  "Run the current rspec using the current line if use-current-line is true"
  (let* ((n (1+ (count-lines 1 (point))))
         (file-name (buffer-file-name))
         (spec-path (if use-current-line (format "%s:%s" file-name n) file-name))
         (testable (and file-name (or (is-rails-spec file-name) (is-rails-test file-name)))))
    (if testable
        (run-rspec-path spec-path use-new-shell)
      (message "This is not a Rails rspec or test!"))))

(defun run-rspec-path (spec-path use-new-shell)
  "Run rspec with the given path"
  (if (or use-new-shell (not (get-buffer rspec-shell-name)))
      (new-shell rspec-shell-name)
    (load-rspec-buffer))
  (goto-char (point-max))
  (insert (format rspec-command spec-path))
  (comint-send-input))

(defun load-rspec-buffer ()
  "Load the rspec buffer, or switch to it if it is visible"
  (let* ((window (get-buffer-window rspec-shell-name)))
    (if window
        (select-window window)
      (switch-to-buffer rspec-shell-name))))

(defun run-rspec-at-current-line ()
  "Run rspec tests for the current file at the current line"
  (interactive)
  (run-rspec t nil))

(defun run-rspec-at-current-line-in-new-shell ()
  "Run rspec tests for the current file at the current line in a new rspec shell"
  (interactive)
  (run-rspec t t))

(defun run-rspec-at-current-file ()
  "Run rspec tests for the current file at the current line"
  (interactive)
  (run-rspec nil nil))

(defun run-rspec-at-current-file-in-new-shell ()
  "Run rspec tests for the current file at the current line in a new rspec shell"
  (interactive)
  (run-rspec nil t))

(setq rspec-shell-name "*rspec*")
(setq rspec-command (concat "cd " (get-rails-root) " && rspec %s"))

(global-unset-key "\C-crl")
(global-set-key "\C-crl" 'run-rspec-at-current-line)

(global-unset-key "\C-crL")
(global-set-key "\C-crL" 'run-rspec-at-current-line-in-new-shell)

(global-unset-key "\C-crf")
(global-set-key "\C-crf" 'run-rspec-at-current-file)

(global-unset-key "\C-crF")
(global-set-key "\C-crF" 'run-rspec-at-current-file-in-new-shell)
