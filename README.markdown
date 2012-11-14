# Summary

This is a collection of various emacs lisp files that you can include
to make various things easier.

## edit-utils.el

This adds some utility functions to make editing easier.

The function change-inside (bound to **C-x c i**) will allow you to
change things inside a particular string or character.  It will handle
parenthesis, braces and brackets as expected (you can type either a
starting or ending one), and it handles any arbitrary character or
string.  The results are saved in your kill ring.

If you are going to delete more than 200 characters, you will be
prompted if you are sure you want to do it (just press y or n to
confirm or cancel).  This maximum can be changed by setting the
max-change-inside-without-prompt like:

    (setq max-change-inside-without-prompt 500)

Similar to change-inside is grab-inside (bound to **C-x g i**), which
will grab what is inside the given string, but it will not be deleted.

## load-all.el

Loads everything in this repo, so you can just add the following line
in your .emacs file to load everything:

    (load "~/path/to/emacs_utils_clone/load-all.el")

## new-shell.el

Adds a simple function sh, which can be used via "M-x sh" to launch a
new shell.  It differs from "M-x shell" by prompting you for which
buffer you want to launch the shell in, and then creates the new
buffer to launch the shell in (rather than opening an existing shell
if one exists).  There is also a non-interactive `new-shell` function
which takes an argument for the name of the shell to create.

## rails-nav.el

Adds a bunch of rails navigation functions inspired by similar
utilities in some VIM plugins.

From anywhere in a Rails project directory, you can jump to a file
based on your current file (or specify a new one).  The name will be
something like "account" which could open account_controller.rb,
account_helper.rb, the account.rb model, or index.html.erb in the
account views directory.  You can specify an action like
"account#show" to either jump directly to that action in the
controller, or open that specific view.  The current position in the
controller will jump you to that particular view, if you appear to be
in an action corresponding to a view.  Similarly, you will be placed
in the appropriate action of a controller if you are in a specific
view.

If the file trying to be found doesn't exist, the name will be toggled
from plural vs not plural and tried again.  Odd inflections like
person/people is not currently supported.

Use the following bindings to jump to the corresponding type:

* **C-x j c**: Controllers
* **C-x j h**: Helpers
* **C-x j m**: Models
* **C-x j v**: Views

If you want to specify a default rails root directory, do so with the
`default-rails-root` global variable in your .emacs file:

    (setq default-rails-root "~/path/to/my/rails/root")

Note that you can tab complete for all possible values of the various
types.

## run-rspec.el

Add the ability to run rspec tests directly from the file in a new
shell buffer.  This relies on new-shell.el and rails-nav.el, so make
sure those are loaded, otherwise this will break.

There are 2 options in invoking this that can be combined for 4 ways
of running rspec tests.  You can run either the entire file, or the
current line you are on.  You can also run in the `*rspec*` buffer,
creating it if it doesn't exist, using it if it does, or you can
always create a new `*rspec*` buffer.

When using the existing `*rspec*` buffer, the buffer will be created
for the first time and set as the current buffer.  All other times,
the buffer will merely be switched to.  If the buffer is visible, that
window will be selected.  If the buffer is not visible, the buffer
will be switched to from the current window.

Use the following key bindings to run your specs:

* **C-c r f**: Run this file in the existing `*rspec*` buffer
* **C-c r F**: Run this file in a new `*rspec*` buffer
* **C-c r l**: Run this line in the existing `*rspec*` buffer
* **C-c r L**: Run this line in a new `*rspec*` buffer

If you want to change the name of the shell buffer used, use the
`rspec-shell-name` global variable.  If you want to change the command
used to run rspec, use the `rspec-command` global variable (which
defaults to `"cd [rails dir] && rspec %s"`, where %s is filled in with
the rspec file to run).  You can set these in your .emacs file:

    (setq rspec-shell-name "*my-specs*")
    (setq rspec-command (concat "cd " (get-rails-root) " && /path/to/rspec %s"))

# License

These utility scripts are licensed under the [MIT license](http://github.com/on-site/emacs_utils/blob/master/MIT-LICENSE.txt)
