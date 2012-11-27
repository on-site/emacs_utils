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

(defun my-shell-up (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-previous-input arg)
    (previous-line arg)))

(defun my-shell-down (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-next-input arg)
    (next-line arg)))

(add-hook 'shell-mode-hook
	  (lambda ()
	    (define-key shell-mode-map (kbd "<up>") 'my-shell-up)
	    (define-key shell-mode-map (kbd "<down>") 'my-shell-down)))
