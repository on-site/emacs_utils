(defmacro def-inside (type fn-to-call help)
  "Define an inside function"
  (let ((fn (intern fn-to-call)))
    `(defun ,(intern (concat type "-inside")) (&optional type-or-ask)
       ,help
       (interactive)
       (let* ((default-type (get-default-change-type))
              (retrieved-type (or type-or-ask (read-string (concat "Inside (default " default-type "): "))))
              (type (if (equal retrieved-type "") default-type retrieved-type)))
         (cond ((or (equal type "(") (equal type ")")) (,fn "(" ")"))
               ((or (equal type "{") (equal type "}")) (,fn "{" "}"))
               ((or (equal type "[") (equal type "]")) (,fn "[" "]"))
               ((or (equal type "<%") (equal type "%>")) (,fn "<%" "%>"))
               ((equal type "\\") (,fn type type)) ; TODO: handle \\ properly
               ((equal type "\"") (,fn type type)) ; TODO: handle \" properly
               (t (,fn type type)))))))

(def-inside "change" "delete-between"
  "Change inside a given type of character (supported types: parens, braces, brakets or any arbitrary string, with special parsing for forward slash and double quotes)")

(def-inside "grab" "grab-between"
  "Grab inside a given type of character (supported types: parens, braces, brakets or any arbitrary string, with special parsing for forward slash and double quotes)")

(defun get-default-change-type ()
  "Retrieve the closest surrounding changeable type"
  (let* ((types '(("(" ")")
                  ("{" "}")
                  ("[" "]")
                  ("<%" "%>")
                  ("\"" "\"")
                  ("'" "'")))
         (type-with-inside-lengths (mapcar (lambda (type-pair) (cons (get-inside-length type-pair) type-pair)) types))
         (closest-type "")
         (closest-type-size nil))
    (dolist (type-details type-with-inside-lengths closest-type)
      (let ((type (cadr type-details))
            (inside-length (car type-details)))
        (if (and inside-length (or (not closest-type-size) (< inside-length closest-type-size)))
            (progn
              (setq closest-type-size inside-length)
              (setq closest-type type)))))))

(defun get-inside-length (type-pair)
  "Get the length from the current buffer"
  (let* ((open-type (car type-pair))
         (close-type (cadr type-pair))
         (heterogeneous (not (equal open-type close-type)))
         (open-backward-position (save-excursion (search-backward open-type nil t)))
         (close-backward-position (if heterogeneous (save-excursion (search-backward close-type nil t))))
         (open-forward-position (if heterogeneous (save-excursion (search-forward open-type nil t))))
         (close-forward-position (save-excursion (search-forward close-type nil t))))
    (if (or (not close-backward-position)
            (> open-backward-position close-backward-position))
        (if (or (not open-forward-position)
                (< close-forward-position open-forward-position))
            (- close-forward-position open-backward-position)))))

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
