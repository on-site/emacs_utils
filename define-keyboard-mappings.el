(unless (boundp 'keyboard-mappings-hash)
  (setq keyboard-mappings-hash (make-hash-table :test 'equal))
  (setq keyboard-on-load-hash (make-hash-table :test 'equal))
  (setq keyboard-on-unload-hash (make-hash-table :test 'equal))
  (puthash "default" (make-hash-table :test 'equal) keyboard-mappings-hash)
  (setq current-keyboard-mapping "default"))

(defun define-keyboard-mappings (name mappings &optional on-load on-unload)
  "Redefine a set of keyboard mappings with the given name and mappings and optional on-load and on-unload"
  (let* ((defaults (gethash "default" keyboard-mappings-hash))
         (new-mapping (make-hash-table :test 'equal)))
    (puthash name new-mapping keyboard-mappings-hash)
    (while (< 0 (length mappings))
      (let* ((mapping (car mappings))
             (code (car mapping))
             (fn (cadr mapping))
             (kbdcode (read-kbd-macro code)))
        (unless (gethash kbdcode defaults)
          (puthash kbdcode (global-key-binding kbdcode) defaults))
        (puthash kbdcode fn new-mapping))
      (setq mappings (cdr mappings)))
    (puthash name on-load keyboard-on-load-hash)
    (puthash name on-unload keyboard-on-unload-hash)))

(defun change-keyboard-mapping (name)
  "Change which keyboard mapping is in use"
  (reset-keyboard-mapping)
  (unless (equal "default" name) (bind-keyboard-mapping name)))

(defun reset-keyboard-mapping ()
  "Reset the keyboard mapping in use to the defaults"
  (bind-keyboard-mapping "default"))

(defun bind-keyboard-mapping (name)
  "Bind all the mappings in the given name"
  (let* ((mappings (gethash name keyboard-mappings-hash))
         (on-load-fn (gethash name keyboard-on-load-hash))
         (on-unload-fn (gethash name keyboard-on-unload-hash)))
    (if on-load-fn (funcall on-load-fn))
    (maphash (lambda (code fn)
               (global-unset-key code)
               (if fn (global-set-key code fn)))
             mappings)
    (setq current-keyboard-mapping name)
    (message (concat "Keyboard mapping set to '" name "'"))
    (if on-unload-fn (funcall on-unload-fn))))

(defun get-keyboard-mapping-names ()
  "Retrieve a list of all keyboard mapping names"
  (let* ((result ()))
    (maphash (lambda (name _)
               (setq result (cons name result))) keyboard-mappings-hash)
    result))

(defun set-keyboard-mapping (&optional name)
  "Set the keyboard mapping to the given name, or prompt for the name"
  (interactive)
  (let* ((mapping-name (if name
                           name
                         (completing-read "Use keyboard mapping: " (get-keyboard-mapping-names)))))
    (if (gethash mapping-name keyboard-mappings-hash)
        (change-keyboard-mapping mapping-name)
      (message (concat "'" mapping-name "' is not a valid keyboard mappings name... define it with define-keyboard-mappings")))))

(defun toggle-keyboard-mapping ()
  "Toggle the keyboard mapping between the default and the user defined primary mapping"
  (interactive)
  (if (boundp 'primary-keyboard-mapping)
      (if (equal "default" current-keyboard-mapping)
          (set-keyboard-mapping primary-keyboard-mapping)
        (set-keyboard-mapping "default"))
    (message "Please define your primary mapping with '(set-default-keyboard-mapping \"my_mapping\")'")))

(defun set-default-keyboard-mapping (name)
  "Set the default keyboard mapping to the given name and sets the mapping to that"
  (setq primary-keyboard-mapping name)
  (set-keyboard-mapping name))

(global-unset-key [f11])
(global-set-key [f11] 'set-keyboard-mapping)

(global-unset-key [f12])
(global-set-key [f12] 'toggle-keyboard-mapping)
