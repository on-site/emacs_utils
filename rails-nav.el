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
    (let ((item (read-rails-item type default-item)))
      (find-file (get-rails-full-item-path type (if (equal item "") default-item item))))))

(defun read-rails-item (type default-item)
  "Read a rails item of the given type with tab completion"
  (completing-read (concat "Rails " type " to load (default " default-item "): ") (retrieve-rails-items type) nil 'confirm))

(defun retrieve-rails-items (type)
  "Retrieve the rails items for the given type"
  (cond ((equal type "controller") (mapcar
                                    (lambda (x) (chomp-ends-with x "_controller.rb"))
                                    (rails-directory-files "app/controllers" ".*_controller\\.rb$")))
        ((equal type "helper") (mapcar
                                (lambda (x) (chomp-ends-with x "_helper.rb"))
                                (rails-directory-files "app/helpers" ".*_helper\\.rb$")))
        ((equal type "model") (mapcar
                               (lambda (x) (chomp-ends-with x ".rb"))
                               (rails-directory-files "app/models" ".*\\.rb$")))
        ((equal type "view") (mapcar
                              (lambda (x)
                                (replace-regexp-in-string "^\\(.+\\)/\\(.+?\\)\\.html\\.erb$" "\\1#\\2" x))
                              (rails-directory-files "app/views" ".+/.+?\\.html\\.erb$")))))

(defun rails-directory-files (path regex)
  "Like a recursive version of directory-files, but for rails directories"
  (compact (mapcar
            (lambda (x)
              (if (and x (string-match regex x)) x))
            (flatten (rails-directory-expand-files "" (get-rails-path path))))))

(defun rails-directory-expand-files (prefix path)
  "Recursive helper function for rails-directory-files"
  (let ((path-dir (file-name-as-directory path)))
    (mapcar (lambda (x)
              (if (or (equal x ".") (equal x ".."))
                  nil
                (let ((path-x (concat path-dir x)))
                  (if (file-directory-p path-x)
                      (rails-directory-expand-files (file-name-as-directory (concat prefix x)) path-x)
                    (concat prefix x)))))
              (directory-files path-dir nil nil t))))

;; From http://stackoverflow.com/questions/969067/name-of-this-function-in-built-in-emacs-lisp-library
(defun flatten(x)
  "Flatten a list, so all sub-list items become simple items in the array, so ((1 2) 3 (4 5)) becomes (1 2 3 4 5)"
  (cond ((null x) nil)
        ((listp x) (append (flatten (car x)) (flatten (cdr x))))
        (t (list x))))

;; From http://stackoverflow.com/questions/3967320/lisp-function-to-remove-nils
(defun compact (x)
  "Compact a list, so all nils are removed, so (nil 1 2 3 nil 4) becomes (1 2 3 4)"
  (if (listp x)
      (mapcar #'compact
              (remove nil x))
    x))

(defun get-rails-item (&optional file-name-or-current)
  "Get the rails item from the given file name or current buffer file name"
  (let ((file-name (or file-name-or-current (buffer-file-name) (file-truename ".")))
        (using-current-buffer (not file-name-or-current)))
    (cond ((is-rails-controller file-name) (concat (string-inside file-name (get-rails-path "app/controllers/") "_controller.rb") (get-rails-controller-action using-current-buffer)))
          ((is-rails-helper file-name) (string-inside file-name (get-rails-path "app/helpers/") "_helper.rb"))
          ((is-rails-model file-name) (string-inside file-name (get-rails-path "app/models/") ".rb"))
          ((is-rails-view file-name) (concat (string-inside file-name (get-rails-path "app/views/") (concat "/" (file-name-nondirectory file-name)))
                                             (get-rails-view-action file-name)))
          (t "application"))))

(defun get-rails-controller-action (detect)
  "Try to detect an action from the current buffer, return with leading #, but only if detect is true"
  (if detect
      (let ((result (save-excursion (if (re-search-backward "def\\s-+\\([a-zA-Z_]+\\)" nil t) (match-string 1)))))
        (if result (concat "#" result)))))

(defun get-rails-view-action (file-name)
  "Get the rails action from the given view file (including a leading #)"
  (concat "#" (nth 0 (split-string (file-name-nondirectory file-name) "\\."))))

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

(defun get-rails-full-item-path (type full-item)
  "Get an item path relative to the current rails root"
  (let ((item (nth 0 (split-rails-item full-item)))
        (maybe-action (nth 1 (split-rails-item full-item))))
    (let ((action (or maybe-action "index")))
      (let ((non-toggled-path (get-rails-item-path type item action))
            (toggled-path (get-rails-item-path type (toggle-plural item) action)))
        (if (and (not (file-exists-p non-toggled-path)) (file-exists-p toggled-path))
            toggled-path
          non-toggled-path)))))

(defun get-rails-item-path (type item action)
  "Get a rails item path from the item and action"
  (cond ((equal type "controller") (get-rails-path (concat "app/controllers/" item "_controller.rb")))
        ((equal type "helper") (get-rails-path (concat "app/helpers/" item "_helper.rb")))
        ((equal type "model") (get-rails-path (concat "app/models/" item ".rb")))
        ((equal type "view") (get-rails-path (concat "app/views/" item "/" action ".html.erb")))))

(defun toggle-plural (item)
  "Toggle the plural state of the given item"
  (let ((chomped (chomp-ends-with item "s")))
    (if chomped
        chomped
      (concat item "s"))))

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
              (t (locate-rails-root (concat path-dir "..")))))))

(defun is-rails-root (path)
  "Determine if a given path is a rails root"
  (let ((path-dir (file-name-as-directory path)))
    (or (file-exists-p (concat path-dir "script/rails"))
        (and (file-exists-p (concat path-dir "script/server"))
             (file-directory-p (concat path-dir "app"))))))

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
