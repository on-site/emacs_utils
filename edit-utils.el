(defmacro def-inside (type fn-to-call help)
  "Define an inside function"
  (let ((fn (intern fn-to-call)))
    `(defun ,(intern (concat type "-inside")) (&optional type-or-ask)
       ,help
       (interactive)
       (let ((type (or type-or-ask (read-string "Inside: "))))
         (cond ((or (equal type "(") (equal type ")")) (,fn "(" ")"))
               ((or (equal type "{") (equal type "}")) (,fn "{" "}"))
               ((or (equal type "[") (equal type "]")) (,fn "[" "]"))
               ((equal type "\\") (,fn type type)) ; TODO: handle \\ properly
               ((equal type "\"") (,fn type type)) ; TODO: handle \" properly
               (t (,fn type type)))))))

(def-inside "change" "delete-between"
  "Change inside a given type of character (supported types: parens, braces, brakets or any arbitrary string, with special parsing for forward slash and double quotes)")

(def-inside "grab" "grab-between"
  "Grab inside a given type of character (supported types: parens, braces, brakets or any arbitrary string, with special parsing for forward slash and double quotes)")

(defun delete-between (start-string end-string)
  "Delete everything between the given start and end value from the current cursor"
  (let ((start (+ (save-excursion (search-backward start-string)) 1))
        (end (- (save-excursion (search-forward end-string)) 1)))
    (if (ensure-wants-to-delete start end)
        (kill-region start end))))

(defun grab-between (start-string end-string)
  "Grab everything between the given start and end value from the current cursor"
  (let ((start (+ (save-excursion (search-backward start-string)) 1))
        (end (- (save-excursion (search-forward end-string)) 1)))
    (kill-ring-save start end)))

(defun ensure-wants-to-delete (start end)
  "Verify the user wants to delete what they said if start and end is large (based on max-change-inside-without-prompt constant)"
  (if (>= (- end start) max-change-inside-without-prompt)
      (let ((answer (read-char (format "You are about to delete %d characters, are you sure (y/n)? " (- end start)))))
        (or (equal ?y answer) (equal ?Y answer)))
    t))

(defun set-change-key (binding fn)
  "Set the change key binding as given"
  (global-unset-key binding)
  (global-set-key binding fn))

(setq max-change-inside-without-prompt 200)
(set-change-key "\C-xci" 'change-inside)
(set-change-key "\C-xgi" 'grab-inside)
