(defmacro def-jump-to-file (binding fn-name help root-dir current-item retrieve-files-message retrieve-files load-file)
  "Define a function to jump to a particular file based on a root directory function, a way to retrieve current context, a way to load known files, and how to load that specific file"
  `(progn
     (defun ,(intern fn-name) ()
       ,help
       (interactive)
       (if ,root-dir
           (let* ((default-item ,current-item)
                  (item (completing-read (concat ,retrieve-files-message " (default " default-item "): ") ,retrieve-files nil 'confirm)))
             (find-file (funcall ,load-file (if (equal item "") default-item item))))
         (message "Cannot find rails root!")))
     (global-unset-key ,binding)
     (global-set-key ,binding (intern ,fn-name))))

(defmacro def-jump-to-rails (binding type retrieve-files)
  "Define a jump-to-rails- function to jump to a particular rails file"
  `(def-jump-to-file
     ,binding
     ,(concat "jump-to-rails-" type)
     (concat "Get the correct rails " ,type " for the current file")
     (get-rails-root)
     (get-rails-default-item ,type (get-rails-type) (get-rails-item) (get-rails-action))
     (concat "Rails " ,type " to load")
     ,retrieve-files
     (lambda (x) (get-rails-full-item-path ,type x))))

(def-jump-to-rails "\C-xjc" "controller"
  (mapcar
   (lambda (x) (chomp-ends-with x "_controller.rb"))
   (rails-directory-files "app/controllers" ".*_controller\\.rb$")))
(def-jump-to-rails "\C-xjh" "helper"
  (mapcar
   (lambda (x) (chomp-ends-with x "_helper.rb"))
   (rails-directory-files "app/helpers" ".*_helper\\.rb$")))
(def-jump-to-rails "\C-xjm" "model"
  (mapcar
   (lambda (x) (chomp-ends-with x ".rb"))
   (rails-directory-files "app/models" ".*\\.rb$")))
(def-jump-to-rails "\C-xjs" "spec"
  (mapcar
   (lambda (x) (chomp-ends-with x "_spec.rb"))
   (rails-directory-files "spec" ".*_spec\\.rb$")))
(def-jump-to-rails "\C-xjt" "test"
  (mapcar
   (lambda (x) (chomp-ends-with x "_test.rb"))
   (rails-directory-files "test" ".*_test\\.rb$")))
(def-jump-to-rails "\C-xjv" "view"
  (mapcar
   (lambda (x) (replace-regexp-in-string "^\\(.+\\)/\\(.+?\\)\\.html\\.erb$" "\\1#\\2" x))
   (rails-directory-files "app/views" ".+/.+?\\.html\\.erb$")))

(defun rails-directory-files (path regex)
  "Like a recursive version of directory-files, but for rails directories"
  (recursive-directory-files 'get-rails-path path regex))

(defun recursive-directory-files (root-fn path regex &optional max-depth)
  "Like a recursive version of directory-files, but use a function to determine the base path"
  (compact (mapcar
            (lambda (x)
              (if (and x (string-match regex x)) x))
            (flatten (recursive-directory-expand-files "" (funcall root-fn path) max-depth)))))

(defun recursive-directory-expand-files (prefix path max-depth)
  "Recursive helper function for recursive-directory-files"
  (let ((path-dir (file-name-as-directory path)))
    (mapcar (lambda (x)
              (if (or (equal x ".") (equal x ".."))
                  nil
                (let ((path-x (concat path-dir x)))
                  (if (file-directory-p path-x)
                      (if (or (not max-depth) (> max-depth 0))
                          (recursive-directory-expand-files (file-name-as-directory (concat prefix x)) path-x (if max-depth (- max-depth 1))))
                    (concat prefix x)))))
            (if (file-exists-p path-dir)
                (directory-files path-dir nil nil t)))))

;; Adapted from http://stackoverflow.com/questions/969067/name-of-this-function-in-built-in-emacs-lisp-library
(defun flatten (x)
  "Flatten a list, so all sub-list items become simple items in the array, so ((1 2) 3 (4 5)) becomes (1 2 3 4 5)"
  (cond ((null x) nil)
        ((listp x) (let (value)
                     (dolist (elt x value)
                       (setq value (append value (flatten elt))))))
        (t (list x))))

;; From http://stackoverflow.com/questions/3967320/lisp-function-to-remove-nils
(defun compact (x)
  "Compact a list, so all nils are removed, so (nil 1 2 3 nil 4) becomes (1 2 3 4)"
  (if (listp x)
      (mapcar #'compact
              (remove nil x))
    x))

(defun get-rails-default-item (for-type maybe-type item action)
  "Get the default item for the given type, given the current item/type/action"
  (let* ((type (if maybe-type maybe-type "model"))
         (non-test-type (cond ((equal type "test") "model")
                              ((equal type "spec") "model")
                              (t type)))
         (test-suffix (cond ((equal type "controller") "_controller")
                            ((equal type "helper") "_helper"))))
    (cond ((equal for-type "controller") item)
          ((equal for-type "helper") item)
          ((equal for-type "model") item)
          ((equal for-type "spec") (concat (toggle-plural non-test-type) "/" item test-suffix))
          ((equal for-type "test") (concat (toggle-plural non-test-type) "/" item test-suffix))
          ((equal for-type "view") (concat item action)))))

(defun get-rails-type (&optional file-name-or-current)
  "Get the rails item type from the given file name or current buffer file name"
  (let ((file-name (file-truename (or file-name-or-current (buffer-file-name) "."))))
    (cond ((is-rails-controller file-name) "controller")
          ((is-rails-helper file-name) "helper")
          ((is-rails-model file-name) "model")
          ((is-rails-spec file-name) "spec")
          ((is-rails-test file-name) "test")
          ((is-rails-view file-name) "view")
          (t nil))))

(defun get-rails-item (&optional file-name-or-current)
  "Get the rails item from the given file name or current buffer file name"
  (let ((file-name (file-truename (or file-name-or-current (buffer-file-name) ".")))
        (using-current-buffer (not file-name-or-current))
        (file-type (get-rails-type file-name-or-current)))
    (cond ((equal file-type "controller") (string-inside file-name (get-rails-path "app/controllers/") "_controller.rb"))
          ((equal file-type "helper") (string-inside file-name (get-rails-path "app/helpers/") "_helper.rb"))
          ((equal file-type "model") (string-inside file-name (get-rails-path "app/models/") ".rb"))
          ((equal file-type "spec") (get-rails-test-item file-name (get-rails-path "spec/") "_spec.rb"))
          ((equal file-type "test") (get-rails-test-item file-name (get-rails-path "test/") "_test.rb"))
          ((equal file-type "view") (string-inside file-name (get-rails-path "app/views/") (concat "/" (file-name-nondirectory file-name))))
          (t "application"))))

(defun get-rails-test-item (file-name before after)
  "Get the item for a test type by checking for controller/helper/model/view tests"
  (or (string-inside file-name (concat before "controllers/") (concat "_controller" after))
      (string-inside file-name (concat before "helpers/") (concat "_helper" after))
      (string-inside file-name (concat before "models/") after)
      (string-inside file-name (concat before "views/") after)
      "application"))

(defun get-rails-action (&optional file-name-or-current)
  "Get the rails action from the given file name or current buffer file name, return with leading #"
  (let ((file-name (file-truename (or file-name-or-current (buffer-file-name) ".")))
        (using-current-buffer (not file-name-or-current))
        (file-type (get-rails-type file-name-or-current)))
    (cond ((equal file-type "controller") (or (get-rails-controller-action using-current-buffer) "#index"))
          ((equal file-type "helper") "#index")
          ((equal file-type "model") "#index")
          ((equal file-type "spec") "#index")
          ((equal file-type "test") "#index")
          ((equal file-type "view") (get-rails-view-action file-name))
          (t "#index"))))

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

(defun is-rails-spec (file-name)
  "Determine if the given file-name is a rails spec"
  (has-parent-directory file-name (get-rails-path "spec")))

(defun is-rails-test (file-name)
  "Determine if the given file-name is a rails test"
  (has-parent-directory file-name (get-rails-path "test")))

(defun is-rails-view (file-name)
  "Determine if the given file-name is a rails view"
  (has-parent-directory file-name (get-rails-path "app/views")))

(defun get-rails-full-item-path (type full-item)
  "Get an item path relative to the current rails root"
  (let* ((item (nth 0 (split-rails-item full-item)))
         (maybe-action (nth 1 (split-rails-item full-item)))
         (action (or maybe-action "index"))
         (non-toggled-path (get-rails-item-path type item action))
         (toggled-path (get-rails-item-path type (toggle-plural item) action)))
    (if (and (not (file-exists-p non-toggled-path)) (file-exists-p toggled-path))
        toggled-path
      non-toggled-path)))

(defun get-rails-item-path (type item action)
  "Get a rails item path from the item and action"
  (cond ((equal type "controller") (get-rails-path (concat "app/controllers/" item "_controller.rb")))
        ((equal type "helper") (get-rails-path (concat "app/helpers/" item "_helper.rb")))
        ((equal type "model") (get-rails-path (concat "app/models/" item ".rb")))
        ((equal type "spec") (get-rails-path (concat "spec/" item "_spec.rb")))
        ((equal type "test") (get-rails-path (concat "test/" item "_test.rb")))
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
  (let* ((path (or path-or-current "."))
         (from-path (locate-rails-root path)))
    (if from-path
        from-path
      (if (boundp 'default-rails-root) (locate-rails-root default-rails-root)))))

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
             (file-directory-p (concat path-dir "app")))
        (and (file-exists-p (concat path-dir "config/application.rb"))
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
