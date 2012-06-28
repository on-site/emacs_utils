(defun sh ()
  "Interactively generate a new shell with the given name"
  (interactive)
  (let ((shell-name (read-string "New shell name: ")))
    (let ((new-buffer (generate-new-buffer shell-name)))
      (set-window-buffer (selected-window) new-buffer)
      (shell new-buffer))))
