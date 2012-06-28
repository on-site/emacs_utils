(defmacro def-jump-to-rails (type)
  "Simplify defining the jump-to rails functions"
  `(defun ,(intern (concat "jump-to-rails-" type)) ()
     ,(concat "Get the correct rails " type " for the current file")
     (interactive)
     (if (get-rails-root)
         (jump-to-rails-item ,type)
       (message "Cannot find rails root!"))))

(def-jump-to-rails "controller")
(def-jump-to-rails "helper")
(def-jump-to-rails "model")
(def-jump-to-rails "view")

(defun jump-to-rails-item (type)
  "Jump to the rails item for the given type based on the current file"
  (let ((default-item (get-rails-item)))
    (let ((item (read-string (concat "Rails item to load (default " default-item "): "))))
      (find-file (get-rails-item-path type (if (equal item "") default-item item))))))

(defun get-rails-item (&optional file-name-or-current)
  "Get the rails item from the given file name or current buffer file name"
  (let ((file-name (or file-name-or-current (buffer-file-name) (file-truename "."))))
    (cond ((is-rails-controller file-name) (string-inside file-name (get-rails-path "app/controllers/") "_controller.rb"))
          ((is-rails-helper file-name) (string-inside file-name (get-rails-path "app/helpers/") "_helper.rb"))
          ((is-rails-model file-name) (string-inside file-name (get-rails-path "app/models/") ".rb"))
          ((is-rails-view file-name) (concat (string-inside file-name (get-rails-path "app/views/") (concat "/" (file-name-nondirectory file-name)))
                                             "#" (nth 0 (split-string (file-name-nondirectory file-name) "\\."))))
          (t "application"))))

(defun is-rails-controller (file-name)
  "Determine if the given file-name is a rails controller"
  (has-parent-directory file-name (get-rails-path "app/controllers")))

(defun is-rails-helper (file-name)
  "Determine if the given file-name is a rails helper"
  (has-parent-directory file-name (get-rails-path "app/helpers")))

(defun is-rails-model (file-name)
  "Determine if the given file-name is a rails model"
  (has-parent-directory file-name (get-rails-path "app/models")))

(defun is-rails-view (file-name)
  "Determine if the given file-name is a rails view"
  (has-parent-directory file-name (get-rails-path "app/views")))

(defun get-rails-item-path (type full-item)
  "Get an item path relative to the current rails root"
  (let ((item (nth 0 (split-rails-item full-item)))
        (maybe-action (nth 1 (split-rails-item full-item))))
    (let ((action (or maybe-action "index")))
      (cond ((equal type "controller") (get-rails-path (concat "app/controllers/" item "_controller.rb")))
            ((equal type "helper") (get-rails-path (concat "app/helpers/" item "_helper.rb")))
            ((equal type "model") (get-rails-path (concat "app/models/" item ".rb")))
            ((equal type "view") (get-rails-path (concat "app/views/" item "/" action ".html.erb")))))))

(defun split-rails-item (full-item)
  "Split a rails item like 'account#index' into '('acount' 'index')"
  (split-string full-item "#"))

(defun get-rails-path (path)
  "Get a path relative to the current rails root"
  (concat (get-rails-root) path))

(defun get-rails-root (&optional path-or-current)
  "Find rails root from the current or given directory, or nil if it is not detected, and fall back to default-rails-root global variable as possible Rails location"
  (let ((path (or path-or-current ".")))
    (let ((from-path (locate-rails-root path)))
      (if from-path
          from-path
        (if (boundp 'default-rails-root) (locate-rails-root default-rails-root))))))

(defun locate-rails-root (path)
  "Find rails root relative to the given directory, or nil if it is not detected"
  (if path
      (let ((path-dir (file-name-as-directory path)))
        (cond ((is-rails-root path-dir) (file-truename path-dir))
              ((equal (file-truename path-dir) "/") nil)
              (t (get-rails-root (concat path-dir "..")))))))

(defun is-rails-root (path)
  "Determine if a given path is a rails root"
  (let ((path-dir (file-name-as-directory path)))
    (file-exists-p (concat path-dir "script/rails"))))

(defun has-parent-directory (testing against)
  "Determine if the first argument has the parent directory being the against argument"
  (let ((testing-dir (file-name-as-directory (file-truename testing)))
        (against-dir (file-name-as-directory (file-truename against))))
    (cond ((equal testing-dir against-dir) t)
          ((equal testing-dir "/") nil)
          (t (has-parent-directory (concat testing-dir "..") against)))))

(defun string-inside (string before after)
  "Retrieve the string iside the before and after strings"
  (let ((latter (chomp-starts-with string before)))
    (if latter (chomp-ends-with latter after))))

(defun chomp-starts-with (string value)
  "Determine if the string starts with the value and chomp it if so, else nil"
  (cond ((and (>= (length string) (length value))
              (string-equal (substring string 0 (length value)) value))
         (substring string (length value)))
        (t nil)))

(defun chomp-ends-with (string value)
  "Determine if the string ends with the value and chomp it if so, else nil"
  (let ((endlength (- (length string) (length value))))
    (cond ((and (>= (length string) (length value))
                (equal value (substring string endlength)))
           (substring string 0 endlength))
          (t nil))))

(defun set-rails-key (binding fn)
  "Set the rails key binding as given"
  (global-unset-key binding)
  (global-set-key binding fn))

(set-rails-key "\C-xjc" 'jump-to-rails-controller)
(set-rails-key "\C-xjh" 'jump-to-rails-helper)
(set-rails-key "\C-xjm" 'jump-to-rails-model)
(set-rails-key "\C-xjv" 'jump-to-rails-view)
