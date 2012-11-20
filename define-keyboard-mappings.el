(unless (boundp 'keyboard-mappings-hash)
  (setq keyboard-mappings-hash (make-hash-table :test 'equal))
  (setq keyboard-mappings-help-hash (make-hash-table :test 'equal))
  (setq keyboard-on-load-hash (make-hash-table :test 'equal))
  (setq keyboard-on-unload-hash (make-hash-table :test 'equal))
  (puthash "default" (make-hash-table :test 'equal) keyboard-mappings-hash)
  (setq current-keyboard-mapping "default")
  (setq keyboard-mappings-keymap (make-keymap))
  (define-minor-mode custom-keyboard-minor-mode
    "A minor mode with custom defined keyboard mappings."
    t
    :lighter (:eval (format " kbd-%s" current-keyboard-mapping))
    :keymap keyboard-mappings-keymap)
  (custom-keyboard-minor-mode 1))

(defun define-keyboard-mappings (name mapping-groups &optional on-load on-unload)
  "Redefine a set of keyboard mapping-groups with the given name and mapping-groups and optional on-load and on-unload"
  (puthash name (make-hash-table :test 'equal) keyboard-mappings-hash)
  (puthash name () keyboard-mappings-help-hash)
  (add-keyboard-mappings name mapping-groups)
  (puthash name on-load keyboard-on-load-hash)
  (puthash name on-unload keyboard-on-unload-hash))

(defun add-keyboard-mappings (name mapping-groups)
  "Add additional keyboard mapping groups for a given name"
  (while mapping-groups
    (add-keyboard-mapping-group name (car mapping-groups))
    (setq mapping-groups (cdr mapping-groups))))

(defun add-keyboard-mapping-group (name mapping-group)
  "Add a mapping group for the given name"
  (let* ((group-name (car mapping-group))
         (mappings (cdr mapping-group)))
    (puthash name (cons mapping-group (gethash name keyboard-mappings-help-hash))
             keyboard-mappings-help-hash)
    (while mappings
      (let* ((mapping (car mappings))
             (code (car mapping))
             (fn (cadr mapping))
             (kbdcode (read-kbd-macro code)))
        (puthash kbdcode fn (gethash name keyboard-mappings-hash)))
      (setq mappings (cdr mappings)))))

(defun change-keyboard-mapping (name)
  "Change which keyboard mapping is in use"
  (reset-keyboard-mapping)
  (unless (equal "default" name) (bind-keyboard-mapping name)))

(defun reset-keyboard-mapping ()
  "Reset the keyboard mapping in use to the defaults"
  (bind-keyboard-mapping "default"))

(defun bind-keyboard-mapping (name)
  "Bind all the mappings in the given name"
  (let* ((old-mappings (gethash current-keyboard-mapping keyboard-mappings-hash))
         (mappings (gethash name keyboard-mappings-hash))
         (on-load-fn (gethash name keyboard-on-load-hash))
         (on-unload-fn (gethash name keyboard-on-unload-hash)))
    (if on-load-fn (funcall on-load-fn))
    ;; Clear the old mapping
    (maphash (lambda (code fn)
               (define-key keyboard-mappings-keymap code nil))
             old-mappings)
    ;; Setup the new mapping
    (maphash (lambda (code fn)
               (define-key keyboard-mappings-keymap code fn))
             mappings)
    ;; Set variables, message the user, etc
    (setq current-keyboard-mapping name)
    (force-mode-line-update t)
    (message (concat "Keyboard mapping set to '" name "'"))
    (if on-unload-fn (funcall on-unload-fn))))

(defun get-keyboard-mapping-names ()
  "Retrieve a list of all keyboard mapping names"
  (let* ((result ()))
    (maphash (lambda (name _)
               (setq result (cons name result))) keyboard-mappings-hash)
    result))

(defun set-keyboard-mapping (name)
  "Set the keyboard mapping to the given name"
  (if (gethash name keyboard-mappings-hash)
      (change-keyboard-mapping name)
    (message (concat "'" name "' is not a valid keyboard mappings name... define it with define-keyboard-mappings"))))

(defun toggle-keyboard-mapping ()
  "Change the keyboard mapping to the user entered mapping, defaulting to toggling between default and the user's primary mapping"
  (interactive)
  (let* ((primary-name (if (boundp 'primary-keyboard-mapping)
                           primary-keyboard-mapping
                         "default"))
         (default-name (if (equal "default" current-keyboard-mapping)
                           primary-name
                         "default"))
         (given-name (completing-read
                      (format "Use keyboard mapping (default %s): " default-name)
                      (get-keyboard-mapping-names)))
         (is-empty (= 0 (length given-name)))
         (name (if is-empty default-name given-name)))
    (set-keyboard-mapping name)))

(defun set-default-keyboard-mapping (name)
  "Set the default keyboard mapping to the given name and sets the mapping to that"
  (setq primary-keyboard-mapping name)
  (set-keyboard-mapping name))

(defun describe-keyboard-mappings ()
  "Describe custom-keyboard-minor-mode"
  (interactive)
  (let* ((new-buffer-name (format "*kbd-%s help*" current-keyboard-mapping))
         (new-buffer (generate-new-buffer new-buffer-name)))
    (set-window-buffer (selected-window) new-buffer)
    (set-buffer new-buffer)
    (insert (concat "Key Mappings for '" current-keyboard-mapping "'\n"))
    (insert "==================================================\n")
    (insert-mapping-help (gethash current-keyboard-mapping keyboard-mappings-help-hash))
    (beginning-of-buffer)))

(defun insert-mapping-help (help)
  "Insert the given help recursively to the current buffer"
  (if help
      (let* ((current (car help))
             (remaining (cdr help))
             (title (car current))
             (mappings (cdr current))
             (max-length (max-code-length mappings)))
        (insert-mapping-help remaining)
        (insert (concat "\n" title "\n"))
        (insert "--------------------------------------------------\n")
        (while mappings
          (let* ((mapping (car mappings))
                 (code (car mapping))
                 (fn (cadr mapping))
                 (space-filler (make-string (- max-length (length code)) ?\ )))
            (insert (format "%s%s  =>  %s\n" code space-filler fn)))
          (setq mappings (cdr mappings))))))

(defun max-code-length (mappings)
  "Get the max code length for the given mapping group"
  (if mappings
      (let* ((current (length (caar mappings)))
             (remaining (max-code-length (cdr mappings))))
        (max current remaining))
    0))

(global-unset-key [f12])
(global-set-key [f12] 'toggle-keyboard-mapping)

(global-unset-key [f1 f12])
(global-unset-key (kbd "C-h <f12>"))
(global-set-key [f1 f12] 'describe-keyboard-mappings)
(global-set-key (kbd "C-h <f12>") 'describe-keyboard-mappings)
