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

## load-all.el

Loads everything in this repo, so you can just add the following line
in your .emacs file to load everything:

    (load "~/path/to/emacs_utils_clone/load-all.el")

## new-shell.el

Adds a simple function sh, which can be used via "M-x sh" to launch a
new shell.  It differs from "M-x shell" by prompting you for which
buffer you want to launch the shell in, and then creates the new
buffer to launch the shell in (rather than opening an existing shell
if one exists).

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
default-rails-root global variable in your .emacs file:

    (setq default-rails-root "~/path/to/my/rails/root")

Note that you can tab complete for all possible values of the various
types.

# License

These utility scripts are licensed under the [MIT license](http://github.com/on-site/emacs_utils/blob/master/MIT-LICENSE.txt)
