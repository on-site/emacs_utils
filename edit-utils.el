(defun change-inside (&optional type-or-ask)
  "Change inside a given type of character (supported types: parens, braces, brakets or any arbitrary string, with special parsing for forward slash and double quotes)"
  (interactive)
  (let ((type (or type-or-ask (read-string "Type to change: "))))
    (cond ((or (equal type "(") (equal type ")")) (delete-between "(" ")"))
          ((or (equal type "{") (equal type "}")) (delete-between "{" "}"))
          ((or (equal type "[") (equal type "]")) (delete-between "[" "]"))
          ((equal type "\\") (delete-between type type)) ; TODO: handle \\ properly
          ((equal type "\"") (delete-between type type)) ; TODO: handle \" properly
          (t (delete-between type type)))))

(defun delete-between (start-string end-string)
  "Delete everything between the given start and end value from the current cursor"
  (save-excursion
    (let ((start (+ (search-backward start-string) 1))
          (end (- (search-forward end-string) 1)))
      (if (ensure-wants-to-delete start end)
          (kill-region start end)))))

(defun ensure-wants-to-delete (start end)
  "Verify the user wants to delete what they said if start and end is large (based on max-change-inside-without-prompt constant)"
  (if (>= (- end start) max-change-inside-without-prompt)
      (let ((answer (read-char (format "You are about to delete %d characters, are you sure (y/n)? " (- end start)))))
        (or (equal ?y answer) (equal ?Y answer)))
    t))

(setq max-change-inside-without-prompt 200)
(set-rails-key "\C-xci" 'change-inside)
