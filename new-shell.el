(defun new-shell (shell-name)
  "Generate a new shell with the given name"
  (let* ((new-buffer (generate-new-buffer shell-name)))
    (set-window-buffer (selected-window) new-buffer)
    (shell new-buffer)))

(defun sh ()
  "Interactively generate a new shell with the given name"
  (interactive)
  (let* ((shell-name (read-string "New shell name: ")))
    (new-shell shell-name)))

(defmacro shell-arrow-key (fn-if-at-end fn-if-normal arg)
  "Extracted common logic from shell-up and shell-down"
  `(if (and (eobp) (is-shell-mode))
       (,fn-if-at-end ,arg)
     (,fn-if-normal ,arg)))

(defun is-shell-mode ()
  "Determine if the current buffer is a shell mode buffer"
  (eq (with-current-buffer (current-buffer) major-mode) 'shell-mode))

(defun shell-up (&optional arg)
  "Act like bash up arrow when at the end of a shell mode buffer"
  (interactive "p")
  (shell-arrow-key comint-previous-input previous-line arg))

(defun shell-down (&optional arg)
  "Act like bash down arrow when at the end of a shell mode buffer"
  (interactive "p")
  (shell-arrow-key comint-next-input next-line arg))

(setq shell-up-down-keymap (make-keymap))

(define-key shell-up-down-keymap (kbd "<up>") 'shell-up)
(define-key shell-up-down-keymap (kbd "<down>") 'shell-down)

(define-minor-mode shell-up-down-minor-mode
  "A minor mode where up and down will act like bash when in shell mode"
  :lighter " ^"
  :keymap shell-up-down-keymap)
