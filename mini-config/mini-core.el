;;; mini-core.el --- core settings and definitions for minimal-configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Martin Marshall

;; Author: Martin Marshall <law@martinmarshall.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is for the following types of default settings:
;; * customizable variables in the 'mini group
;; * functions and macros
;; * menu entries and keybindings that don't depend on external packages

;;; Code:

(require 'cl-seq)
(require 'package)

;; Customizable variables

(defgroup mini nil
  "Customizable settings for a *sort-of* minimalistic Emacs starter config."
  :version "0.1"
  :group 'emacs)

(defcustom mini-my-settings-file (expand-file-name
				  "my-settings.el" user-emacs-directory)
  "Location of the user's handcrafted settings file."
  :type 'file
  :group 'mini)

(defcustom mini-add-imenu-package-headings t
  "Whether to add mini's style of headings to the imenu regex.
These are only used in `emacs-lisp-mode'.  Makes it possible to
jump to configuration sections.  Especially useful in the
mini-packages file for jumping to a specific package's
configuration section."
  :type 'boolean
  :group 'mini)

(defcustom mini-kill-current-buffer-by-default t
  "Default to killing the current instead of asking which."
  :type 'boolean
  :group 'mini)

(defcustom mini-use-ibuffer-over-list-buffers t
  "Replace the binding for `list-buffers' with `ibuffer'."
  :type 'boolean
  :group 'mini)

(defcustom mini-easier-tab-bar-bindings t
  "Add some easier to reach bindings for manipulating tab-bar tabs."
  :type 'boolean
  :group 'mini)

(defcustom mini-keep-defk-list t
  "Track bindings made with `mini-defk'.

This allows them to be viewed with the command
`mini-show-defk-list'."
  :type 'boolean
  :group 'mini)

(defcustom mini-bind-init-file t
  "Create a binding for quickly loading the init file."
  :type 'boolean
  :group 'mini)

(defcustom mini-transpose-bindings t
  "Create a prefix for accessing transpose commands.
This includes commands that built-in to Emacs but are not bound
by default."
  :type 'boolean
  :group 'mini)

(defcustom mini-kill-bindings t
  "Create a prefix for accessing kill commands.
This includes commands that built-in to Emacs but are not bound
by default."
  :type 'boolean
  :group 'mini)

(defcustom mini-to-word-bindings nil
  "Whether to bind \\M\-F to `forward-to-word' and \\M\-B to `backward-to-word'.

These bindings allow for quick and precise cursor movement around
words.  You hold the shift key instead of going an extra word and
then backing up.

The trade-off is that they prevent the shift-selection with the
same bindings."
  :type 'boolean
  :group 'mini)

(defcustom mini-unclutter-help-map nil
  "Whether to unbind some lesser used items in the `help-map'."
  :type 'boolean
  :group 'mini)

(defcustom mini-excluded-packages nil
  "List of package configuration blocks to ignore.
This applies to the following functions: `mini-pkgif',
`mini-ensure' and `mini-bltin'.

If one of the above functions includes an excluded package as its
argument (or as one of the items in its argument list), the forms
in that function call will be ignored.

Most of the time, you will want to simply add to one of the
existing configurations.  But this makes it possible to
completely the default settings for a package.

Additionally, `mini-ensure' will not install packages that are
included in this list."
  :type 'list
  :group 'mini)

(defcustom mini-swap-C-x-with-C-t nil
  "Whether to enable some custom key translation settings.
The specific settings are found at the top of the
mini-keybindings file.  This defaults to nil, so that others may
borrow this configuration without too much confusion."
  :type 'boolean
  :group 'mini)

(defcustom mini-swap-M-x-with-M-t nil
  "Whether to enable some custom key translation settings.
The specific settings are found at the top of the
mini-keybindings file.  This defaults to nil, so that others may
borrow this configuration without too much confusion."
  :type 'boolean
  :group 'mini)

(defcustom mini-swap-C-M-x-with-C-M-t nil
  "Whether to enable some custom key translation settings.
The specific settings are found at the top of the
mini-keybindings file.  This defaults to nil, so that others may
borrow this configuration without too much confusion."
  :type 'boolean
  :group 'mini)

(defcustom mini-xlate-C-M-g-to-C-g nil
  "Whether to enable some custom key translation settings.
The specific settings are found at the top of the
mini-keybindings file.  This defaults to nil, so that others may
borrow this configuration without too much confusion."
  :type 'boolean
  :group 'mini)

(defcustom mini-use-C-h-for-backspace nil
  "Whether to translate presses of `C-h' to backspace.
This increases consistency with GNU Readline keybindings, which
are used by e.g. the Bash shell.

Note, however, that this will require use of a different key for
accessing the `help-map.'  `<f1>' works without any further
configuration."
  :type 'boolean
  :group 'mini)

(defcustom mini-make-C-w-double-as-backward-kill-word nil
  "Whether to have `C-w' also do `backward-kill-word'.
With text selected, `C-w' does `kill-region' as usual.  But when
there is no text selected and this option is set to a non-nil
value, `C-w' will do `backward-kill-word'.

This increases consistency with GNU Readline keybindings, which
are used by e.g. the Bash shell."
  :type 'boolean
  :group 'mini)

(defcustom mini-keyboard-layout 'qwerty
  "Keyboard layout to optimize for.
This is used for determining locations of some bindings, such as the
navigation block and the `avy-keys' variable."
  :type '(radio (symbol :tag "QWERTY" qwerty)
                (symbol :tag "Dvorak" dvorak)
                (symbol :tag "Programmer Dvorak" programmer-dvorak)
                (symbol :tag "Capewell Dvorak" capewell-dvorak)
                ;; TODO Update navigation-block with option for left-side.
                (symbol :tag "Dvorak Left-Hand" dvorak-left-hand)
                (symbol :tag "Dvorak Right-Hand" dvorak-right-hand)
                (symbol :tag "Colemak" colemak)
                (symbol :tag "Colemak-DH" colemak-dh)
                (symbol :tag "ISRT" isrt)
                (symbol :tag "Boo" boo)
                (symbol :tag "Workman" workman)
                (symbol :tag "Norman" norman))
  :group 'mini)

(defcustom mini-simulate-C-x-at-C-t-in-Dvorak nil
  "This affects only the Dvorak and Programmer Dvorak layouts.
If non-nil (the default), and either of those layouts is
selected as `mini-keyboard-layout', then certain keybindings will
be altered to compensate for the inconvenience of pressing the
`C-x' key in those layouts.

In that case, `C-t' will *simulate* `C-x'.  Because it's a
simulation of the `C-x' keypress, all bindings that are available
from `C-x' will also be available from `C-t' even if the
bindings aren't in `ctl-x-map'.  (Example: In `org-mode-map',
`C-x n b' is bound to `org-narrow-to-block'.  If we were merely
binding `C-t' to the `Control-x-prefix' command, `C-t n b' would
not work.)

Since this displaces the normal `transpose-chars' `global-map'
binding of `C-t'.  That binding is now added to
`isearch-mode-map' (making it accessible via either `C-rC-t' or
`C-sC-t').

Further, `C-xC-t' will be bound to the `exchange-point-and-mark'
command in order to provide the same convenience that its other
binding, `C-xC-x' provides (allowing you to hold down Ctrl and
double-tap the \"t\" key).  This displaces the `transpose-lines'
command, which previously occupied the `C-xC-t' binding.

In order to make up for that, `C-xM-t' will be bound to
`transpose-lines'.  And since this key sequence was previously
unbound, no further keys are displaced, allowing all commands to
remain available.

Note that the `C-x' binding will remain in place, which allows
you to avoid swapping hands when the base key falls on the other
side.  So you're still able to use `C-xC-e' for
`eval-last-sexp', while using `C-tC-s' for `save-buffer'."
  :type 'boolean
  :group 'mini)

(defcustom mini-swap-Ctrl-with-Meta nil
  "Whether to swap the Ctrl and Meta keys when in Emacs.
This is probably better done through the desktop environment.
Solving it within Emacs seems to work fine.  But be warned that
it hasn't been heavily tested."
  :type 'boolean
  ;; '((const :tag "Swap the Ctrl and Meta keys." t)
          ;; (const :tag "Don't." nil))
  :group 'mini)

(defcustom mini-C-x+g+char-as-C-x+C-char nil
  "Whether to use `g' as a substitute for holding down Ctrl.
This applies to the `ctl-x-map'.  This is useful if you bind
CapsLock or some other key (such as a function key) to simulate
`C-x'.  In this way, you can almost completely avoid using
modifiers in `C-x' sequences.  Even more useful if you favor
modal editing."
  :type 'boolean
  ;; '((const :tag "Use g for Ctrl after pressing C-x." t)
          ;; (const :tag "Don't." nil))
  :group 'mini)

(defcustom mini-C-c+g+char-as-C-c+C-char nil
  "Whether to use `g' as a substitute for holding down Ctrl.
This applies to the `mode-specific-map'.  This is useful if you
bind CapsLock or some other key (such as a function key) to
simulate `C-c'.  In this way, you can almost completely avoid
using modifiers in `C-c' sequences.  Even more useful if you
favor modal editing."
  :type 'boolean
  ;; '((const :tag "Use g for Ctrl after pressing C-c." t)
          ;; (const :tag "Don't." nil))
  :group 'mini)

(defcustom mini-C-c+letter-as-C-c+C-letter nil
  "After the `C-c' prefix, whether to assume Ctrl modifier.
This applies to any letter key pressed after entering the
`mode-specific-map'.  This is useful if you bind CapsLock or some
other key (such as a function key) to simulate `C-c' in order
to invoke major and minor-mode bindings, but you don't intend to
put any of your user bindings under the `C-c' prefix at all.
Potentially useful if you favor modal editing, or just don't want
to juggle your keyboard when using `org-mode.'"
  :type 'boolean
  ;; '((const :tag "Imply Ctrl whenever pressing a letter under the 'C-c' prefix." t)
          ;; (const :tag "Don't." nil))
  :group 'mini)

(defcustom mini-preserve-dvp-numbering-for-args t
  "Whether to use `programmer-dvorak' numbering.
This applies to the `mini-digit-argument' function (for entering
digit arguments on the home-row).  It determines whether to use
the numbering order from Programmer Dvorak's number row, or to
use regular numeric order used by other layouts."
  :type '(radio (const
                 :tag "Yes, if layout is Programmer Dvorak, use
                 its numbering order." t)
                (const
                 :tag "No, use 1, 2, 3... when entering numbers
on the home-row, even if the layout is Programmer Dvorak." nil)
                (const
                 :tag "I'm feeling adventurous!  Use
                Programmer Dvorak's numbering order, even if my
                layout isn't Programmer Dvorak!" wild))
  :group 'mini)

;; Functions and Macros


;;; Make a shortcut for opening the config file.

(defun mini-find-init-file ()
  "Open the User's Emacs configuration file."
  (interactive)
  (find-file user-init-file))

 
;;; Menu functions

(defun mini-addmenu (mapstr itemlist &optional path before)
  "MAPSTR ITEMLIST PATH BEFORE."
  (declare (indent 1))
  (require 'easymenu)
  (dolist (item itemlist)
    (easy-menu-add-item
     (intern (if (symbolp mapstr)
		 (symbol-name mapstr)
	       (concat "menu-bar-" mapstr "-menu")))
     path
     item
     before)))

(defun mini-menu-toggler (sym)
  "Return a command to toggle the boolean value of variable SYM."
  (lambda () (interactive) (setq sym (not sym))))


;;; Keybinding


(defun mini-kbd (key)
  "If KEY is a string, convert it to Emacs's internal representation.
Otherwise, if it's not already a vector, make it one."
  (cond
   ((stringp key) (kbd key))
   ((vectorp key) key)
   ((listp key) (eval key))
   (t (vector key))))

(defvar mini-defk-list '()
  "List of bindings created by `mini-defk'.
A list of alists, where each alist contains the symbol of a
variable holding a keymap as its car.  The cdr of each alist is a
list of bindings.  Each binding is a list containing 3 strings, the
key, the command, and the which-key-string (if any).")

(defmacro mini-defk (key cmd &optional map wkstringk)
  "Bind KEY to CMD in MAP or `global-map'.
If which-key is installed, it can use the string provided as
WKSTRINGK"
  `(let* ((kmap-symbol (if ,map (quote ,map) (quote global-map)))
	  (kmap (eval kmap-symbol))
	  (kmap-name (mini-org-help-link (symbol-name kmap-symbol)))
	  (cmd-name (mini-org-help-link
		     (if ,cmd
			 (condition-case nil
			     (symbol-name ,cmd)
			   (error nil)))))
	  (cmd-original (lookup-key kmap (mini-kbd ,key)))
	  (cmd-original-name (mini-org-help-link
			      (if cmd-original
				  (condition-case nil
				      (symbol-name cmd-original)
				    (error nil))))))
     ;; Bind the key.
     (define-key kmap (mini-kbd ,key)
		 (if ,wkstringk
		     (cons ,wkstringk ,cmd)
		   ,cmd))
     ;; Add a row to mini-defk-list for the mini-show-defk-list command.
     (let ((newrow (list (key-description (mini-kbd ,key)) cmd-name cmd-original-name))
	   (kmapelt (cond
		     ((assoc kmap-name mini-defk-list))
		     ((and (push (list kmap-name) mini-defk-list)
			   (assoc kmap-name mini-defk-list))))))
       (unless (member newrow kmapelt)
	 (push newrow (cdr kmapelt))))))

(defmacro mini-xlate (key xkey &optional map)
  "Translate KEY to XKEY.

If MAP is omitted and both keys can be represented as
characters (such as with most Ctrl-modified keys), then the
`keyboard-translate' function is used.  This is because it is
more reliable, especially when used with prefix keys.  Otherwise,
the keys are bound to the specified MAP or `key-translation-map'."
  `(let* ;; Use keyboard-translate?
       ((kbt (and (characterp (aref (kbd ,key) 0))
		  (characterp (aref (kbd ,xkey) 0))
		  (not ,map)))
	(kmap-symbol (if ,map (quote ,map) (quote key-translation-map)))
	(kmap (eval kmap-symbol))
	(kmap-name (if kbt
		       (mini-org-help-link
			"keyboard-translate")
		     (mini-org-help-link
		      (symbol-name kmap-symbol))))
	(xkey-string (kbd ,xkey)))
     (if kbt
	 (keyboard-translate (aref (kbd ,key) 0) (aref (kbd ,xkey) 0))
       (define-key kmap (mini-kbd ,key) (mini-kbd ,xkey)))
     (let ((newrow (list (key-description (mini-kbd ,key)) (key-description (mini-kbd xkey-string)) ""))
	   (kmapelt (cond
		     ((assoc kmap-name mini-defk-list))
		     ((and (push (list kmap-name) mini-defk-list)
			   (assoc kmap-name mini-defk-list))))))
       (unless (member newrow kmapelt)
	 (push newrow (cdr kmapelt))))))

(defun mini-org-help-link (symname)
  "Generate an org-help-link for SYMNAME.
It can be either a \='variable or \='callable"
  (if (member symname '(nil ""))
      symname
    (format "[[help:%s][%s]]" symname symname)))

(defun mini-show-defk-list ()
  "Pretty-print the custom keybindings created using `mini-defk'.
along with the command that was previously bound to the same
key, if there was one."
  (interactive)
  (let* ((keys (copy-sequence mini-defk-list))
	 ;; sort each keymap section
	 (sectioned (mapcar (lambda (x) (if (> (length x) 2)
				       (cons (car x) (sort (cdr x) (lambda (a b) (string< (car a) (car b)))))
				     x))
			    keys))
	 ;; sort the sections among themselves
	 (sortedkeys (sort sectioned (lambda (a b) (string< (car a) (car b)))))
	 (buf (get-buffer-create "*Mini-Defk Keys*")))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (org-mode)
      (flyspell-mode-off)
      (if (fboundp 'read-only-mode)
          (read-only-mode -1)
        (setq buffer-read-only nil))
      (erase-buffer)
      (let ((title "User-Defined Keys and Predecessor Commands"))
	(insert (format "%s\n" title))
	(insert (format "%s\n\n" (make-string (length title) ?=))))
      (insert "|--------+-----+-------------+------------------------------|\n")
      (insert "| Keymap | Key | New Command | Predecessor Command (or nil) |\n")
      (insert "|--------+-----+-------------+------------------------------|\n")
      (mapc (lambda (m)
	      (insert (format "| *%s* | %s | %s | %s |\n" (car m) (nth 0 (cadr m)) (nth 1 (cadr m)) (nth 2 (cadr m))))
	      (mapc (lambda (n)
		      (unless (condition-case nil
				  (member (substring (nth 0 n) 0 6) '("C-c g " "C-x g "))
				(error nil))
			(insert (format "|  | %s | %s | %s |\n" (string-replace "\|" "\\vert" (nth 0 n)) (nth 1 n) (nth 2 n)))))
		    (cddr m))
	      (insert "|-\n"))
	    sortedkeys)
      (declare-function org-table-align nil)
      (org-table-align)
      (view-mode 1)
      (goto-char 0)
      (defvar org-mode-map)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key [?q] 'kill-current-buffer))))


(defvar mini-known-keyboard-layouts
  `((qwerty
     ,(vector
       ;;0 1  2  3  4  5  6  7  8  9 10 11 12
       ?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?- ?=
       ;;131415 16 17 18 19 20 21  22 23  24  25
       ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?\[ ?\] ?\\
       ;;262728 29 30 31 32 33 34  35 36
       ?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?'
       ;;373839 40 41 42 43 44 45 46
       ?z ?x ?c ?v ?b ?n ?m ?, ?. ?/))
    (dvorak
     ,(vector
       ?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?\[ ?\]
       ?' ?, ?. ?p ?y ?f ?g ?c ?r ?l ?/ ?= ?\\
       ?a ?o ?e ?u ?i ?d ?h ?t ?n ?s ?-
       ?\; ?q ?j ?k ?x ?b ?m ?w ?v ?z))
    (dvorak-left-hand
     ,(vector
       ?` ?\[ ?\] ?/ ?P ?F ?M ?L ?J ?4 ?3 ?2 ?1
       ?\; ?q ?b ?y ?u ?r ?s ?o ?. ?6 ?5 ?= ?\\
       ?- ?k ?c ?d ?t ?h ?e ?a ?z ?8 ?7
       ?' ?x ?g ?v ?w ?n ?i ?, ?0 ?9))
    (dvorak-right-hand
     ,(vector
       ?` ?1 ?2 ?3 ?4 ?j ?l ?m ?f ?p ?/ ?\[ ?\]
       ?5 ?6 ?q ?. ?o ?r ?s ?u ?y ?b ?\; ?= ?\\
       ?7 ?8 ?z ?a ?e ?h ?t ?d ?c ?k ?-
       ?9 ?0 ?x ?, ?i ?n ?w ?v ?g ?'))
    (programmer-dvorak
     ,(vector
       ?$ ?& ?\[ ?{ ?} ?\( ?= ?* ?\) ?+ ?\] ?! ?#
       ?\; ?, ?. ?p ?y ?f ?g ?c ?r ?l ?/ ?@ ?\\
       ?a ?o ?e ?u ?i ?d ?h ?t ?n ?s ?-
       ?' ?q ?j ?k ?x ?b ?m ?w ?v ?z))
    (capewell-dvorak
     ,(vector
       ?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?\[ ?\]
       ?' ?, ?. ?p ?y ?q ?f ?g ?r ?k ?/ ?= ?\\
       ?o ?a ?e ?i ?u ?d ?h ?t ?n ?s ?-
       ?z ?x ?c ?v ?j ?l ?m ?w ?b ?\;))
    (colemak
     ,(vector
       ?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?- ?=
       ?q ?w ?f ?p ?g ?j ?l ?u ?y ?\; ?\[ ?\] ?\\
       ?a ?r ?s ?t ?d ?h ?n ?e ?i ?o ?'
       ?z ?x ?c ?v ?b ?k ?m ?, ?. ?/))
    (colemak-dh
     ,(vector
       ?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?- ?=
       ?q ?w ?f ?p ?b ?j ?l ?u ?y ?\; ?\[ ?\] ?\\
       ?a ?r ?s ?t ?g ?m ?n ?e ?i ?o ?'
       ?z ?x ?c ?d ?v ?k ?h ?, ?. ?/))
    ;; I have to include this one, since the layout looks like it was
    ;; made for Emacs.  (See positioning of the n, p, f, b, r, l, and
    ;; x keys.)  https://notgate.github.io/layout/
    (isrt
     ,(vector
       ?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?= ?\[
       ?y ?c ?l ?m ?k ?z ?f ?u ?, ?' ?- ?\] ?\\
       ?i ?s ?r ?t ?g ?p ?n ?e ?a ?o ?\;
       ?v ?w ?d ?j ?q ?b ?h ?/ ?. ?x))
    ;; As with the above, this one too.  The Boo layout is a variation
    ;; on Dvorak.  But again, note the positions of the n, p, f, b, r,
    ;; l, and x keys.  https://ballerboo.github.io/boolayout/
    (boo
     ,(vector
       ?` ?1 ?2 ?3 ?4  ?5 ?6 ?7 ?8 ?9 ?0 ?\[ ?\]
       ?, ?. ?u ?c ?v  ?q ?f ?d ?l ?y ?/ ?= ?\\
       ?a ?o ?e ?s ?g  ?b ?n ?t ?r ?i ?-
       ?\; ?x ?' ?w ?z ?p ?h ?m ?k ?j))
    (workman
     ,(vector
       ?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?- ?=
       ?q ?d ?r ?w ?b ?j ?f ?u ?p ?\; ?\[ ?\] ?\\
       ?a ?s ?h ?t ?g ?y ?n ?e ?o ?i ?'
       ?z ?x ?m ?c ?v ?k ?l ?, ?. ?/))
    (norman
     ,(vector
       ?` ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0 ?- ?=
       ?q ?w ?d ?f ?k ?j ?u ?r ?l ?\; ?\[ ?\] ?\\
       ?a ?s ?e ?t ?g ?y ?n ?i ?o ?h ?'
       ?z ?x ?c ?v ?b ?p ?m ?, ?. ?/))))

(defvar mini-keyboard-layout 'qwerty)
(defun mini-getkeys (&optional layout)
  "Return the keys corresponding to LAYOUT from `mini-known-keyboard-layouts'.
With no arguments passed, use `mini-keyboard-layout' as the layout."
  (cadr (assoc (or layout mini-keyboard-layout) mini-known-keyboard-layouts)))

(defvar mini-keys
  (mini-getkeys)
  "List of keys in the order found in the chosen layout.
These can be referenced by their index in the list, so that
certain choices of keybindings can be location-specific and
layout-agnostic.")

(defun mini-keyvector (indexlist &optional layoutdef)
  "Return keys corresponding to the list of positions in INDEXLIST.
LAYOUTDEF is a definition of a keyboard layout in the style of
`mini-keys'.  If omitted, the value of `mini-keys' is used."
  (vconcat (mapcar (lambda (x) (aref (or layoutdef mini-keys) x))
                   indexlist)))

(defun mini-listkeys (keylist &optional layoutdef)
  "Return an index list based on KEYLIST.
LAYOUTDEF is a definition of a keyboard layout in the style of
`mini-keys'.  If omitted, the value of `mini-keys' is used."
  (mapcar (lambda (x) (cl-position x (or layoutdef mini-keys)))
          keylist))

;; (defun mini-keystring (indexlist &optional layoutdef)
;;   "Return a string based on INDEXLIST.
;; LAYOUTDEF is a definition of a keyboard layout in the style of
;; `mini-keys'.  If omitted, the value of `mini-keys' is used."
;;   (concat (mini-keyvector indexlist layoutdef)))

(defun mini-qwerty-equiv (char &optional currentlayout baselayout)
  "Return character currently residing at the location of CHAR in BASELAYOUT.
By default, BASELAYOUT contains the qwerty characters from
`mini-known-keyboard-layouts', and CURRENTLAYOUT is the value of
`mini-keys'.  If the layouts are the same, the given CHAR is simply
returned as-is."
  (let ((currentlayout (or currentlayout mini-keys))
        (baselayout (or baselayout (mini-getkeys 'qwerty))))
    (if (eq currentlayout baselayout)
        char
      (aref currentlayout (car (mini-listkeys (list char) baselayout))))))

(defvar mini-keys-homerow
  (mini-keyvector (number-sequence 26 36))
  "Almost all homerow keys.
Excludes CapsLock and Enter.  Assumes use of ANSI keyboard, so
backslash is also excluded.")

;; all homerow keys, excluding CapsLock and Enter.
(defvar mini-keys-homerow-10
  (seq-take mini-keys-homerow 10)
  "First ten printing homerow keys.
Excludes CapsLock, Enter, and the 11th self-inserting key (\' on
a QWERTY keyboard).  Assumes use of ANSI keyboard, so backslash
is also excluded.")

;; Make a plist for looking up digit events from pressed homerow keys.
(defvar mini-preserve-dvp-numbering-for-args t)
(defvar mini-digits (let ((homerow (append mini-keys-homerow-10 nil))
                          (nums (if (or (and (eq mini-keyboard-layout 'programmer-dvorak)
                                             mini-preserve-dvp-numbering-for-args)
                                        (eq mini-preserve-dvp-numbering-for-args 'wild))
                                  ;; exception to match DVP's number layout
                                  '(?7 ?5 ?3 ?1 ?9 ?0 ?2 ?4 ?6 ?8)
                                '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)))
                        (newlist))
                    (while (and homerow nums)
                      (setq newlist (cons (pop homerow) newlist))
                      (setq newlist (cons (pop nums) newlist)))
                    (nreverse newlist)))

(defvar mini-last-command nil
  "Used by `mini-simk' to keep track of the last command.
Primarily so that the `repeat' command will work as one expects.")
(defvar mini-last-command-event nil
  "Used by `mini-simk' to keep track of the last command event.
Primarily so that the `repeat' command will work as one expects.")
(defvar mini-last-repeatable-command nil
  "Used by `mini-simk' to keep track of the last repeatable command.
Primarily so that the `repeat' command will work as one expects.")
(defvar mini-last-prefix-arg nil
  "Used by `mini-simk' to keep track of the prefix argument.")

(defun mini-skeyname (skeydescrip)
  "Fix SKEYDESCRIP for use in a symbol name.
This used by `mini-simk' to construct function symbols."
  (let ((rstringf (if (>= emacs-major-version 28)
		      'string-replace
		    'replace-regexp-in-string)))
    (dolist (spair '((" "  "+")
		     ("\[" "LSqrBrkt")
		     ("\]" "RSqrBrkt")
		     ("\\" "BackSlash")
		     ("\|" "PipeSymbol")
                     (":"  "Colon"))
		   skeydescrip)
      (setq skeydescrip (funcall rstringf (car spair) (cadr spair) skeydescrip)))))

(defun mini-simk (simkey)
  "Create and return a function that simulates pressing SIMKEY."
  (let* ((normalsimkey (mini-kbd simkey))
	 (funcsym (intern
                   (concat "mini-do-" (mini-skeyname (key-description normalsimkey))))))
    ;; Make (for example) `mini-do-C-S-x' save `last-command' and `last-command-event' in global variables,
    ;; `mini-last-command' and `mini-last-command-event'.
    ;; Then add a function to `pre-command-hook' that checks if the last command was a `mini-simk' command.
    ;; If so, the function updates `last-command-event' and `last-repeatable-command' to match the command
    ;; that was called *before* the `mini-simk' command.
    (fset funcsym
          `(lambda () (interactive)
	    (setq prefix-arg current-prefix-arg)
	    (setq mini-last-prefix-arg last-prefix-arg)
	    (setq mini-last-command last-command)
	    (setq mini-last-command-event last-command-event)
	    (setq unread-command-events
		  (mapcar (lambda (x) (cons t x)) (listify-key-sequence ,normalsimkey)))))
    funcsym))

;; Equivalents to other universal keybindings that work in most
;; applications including Emacs...
;; C-y = S-<insert>
;; C-w = S-<delete>
;; M-w = C-<insert>

;; Modifier simulation under the standard prefix keys.
(defvar mini-modifiable-keys ;; Keys that can be modified.  Not
                           ;; absolutely comprehensive, but likely
                           ;; more than enough.
  (append '(?\t ?\r ?\e ?\s ?\d ?\b) ;; tab return escape space del
                                     ;; backspace (typically
                                     ;; translated to del)
          '(up down left right menu print scroll pause
               insert delete home end prior next
               tab return space backspace escape
               f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12)
          (number-sequence 33 126))
  "Nearly comprehensive list of modifiable keys.")

(defvar mini-lowercase-letters (number-sequence 97 122)
  "List of lowercase letters.")

(defvar mini-uppercase-letters (number-sequence 65 90)
  "List of uppercase letters.")

(defvar mini-letters (append mini-uppercase-letters mini-lowercase-letters)
  "List of both uppercase and lowercase letters.")

(defun mini-convenient-prefix (prefix keylist simkeys &optional hitkey)
  "Make it easier to enter sequences that involve several modifier keys.

PREFIX is the prefix key under which we will bind a convenience
key.

KEYLIST is a list of base keys, each of which, when pressed after
the HITKEY, will have SIMKEYS added as modifiers and/or
additional prefixes.

SIMKEYS must be a list of symbols and/or integers, representing
keys and modifiers which will modify the keys in KEYLIST when
HITKEY is used."
  ;; (dolist (key keylist)
  ;;   `(mini-simkey (if ,hitkey
  ;;                  (vector ,prefix ,hitkey ,key)
  ;;                (vector ,prefix ,key))
  ;;              (vector ,prefix (event-convert-list ,(append simkeys (list key)))))))
  (dolist (key keylist)
    (mini-defk (if hitkey
                   (vector prefix hitkey key)
                 (vector prefix key))
               (mini-simk (vector prefix (event-convert-list (append simkeys (list key))))))))

;; ;; Set up "C-c [a-zA-Z]" to simulate "C-c C-[a-zA-Z]".  (But leave out
;; ;; "g", since we'll use it to simulate Control with *any* character
;; ;; under "C-c", including non-letter characters.)
(when mini-C-c+letter-as-C-c+C-letter
  (mini-convenient-prefix ?\C-c (remq ?g mini-letters) '(control)))

;; ;; Set up "C-c g KEY" to simulate entering "C-c C-KEY".
(when mini-C-c+g+char-as-C-c+C-char
  (mini-convenient-prefix ?\C-c mini-modifiable-keys '(control) ?g))

;; ;; Set up "C-x g KEY" to simulate entering "C-x C-KEY".
(when mini-C-x+g+char-as-C-x+C-char
  (mini-convenient-prefix ?\C-x mini-modifiable-keys '(control) ?g))

;; ;; Swapping Ctrl with Meta

;; ;; There seems to be no built-in mechanism to swap modifier keys in
;; ;; Emacs, but it can be accomplished (for the most part) by
;; ;; translating a near-exhaustive list of modifiable keys.  In the case
;; ;; of 'control and 'meta, some keys must be omitted to avoid errors or
;; ;; other undesired effects.
;; (defun mini-make-key-string (modsymbol basic-event)
;;   "Convert the combination of MODSYMBOL and BASIC-EVENT.
;; BASIC-EVENT can be a character or a function-key symbol.  The
;; return value can be used with `define-key'."
;;   (vector (event-convert-list `(,modsymbol ,basic-event))))

;; (when mini-swap-Ctrl-with-Meta
;;   ;; Won't apply to C-\[ and M-\[, because that causes problems.
;;   (dolist (char (remq ?\[ mini-modifiable-keys))
;;     ;; Need to use `input-decode-map', because `key-translation-map' isn't early enough.
;;     (define-key input-decode-map (mini-make-key-string 'control char) (mini-make-key-string 'meta char))
;;     (define-key input-decode-map (mini-make-key-string 'meta char) (mini-make-key-string 'control char))))


(defun mini-C-w-dwim (arg)
  "If region is active, do `kill-region'.
Otherwise, do `backward-kill-word' and pass ARG to same."
  (interactive "p")
  (if (use-region-p)
      (call-interactively 'kill-region)
    (backward-kill-word arg)))


;;; Mini-set

(defvar mini-vars-from-custom-file
  (mapcar
   (lambda (x) (when (eq (car x) 'theme-value) (cadr x)))
   (get 'user 'theme-settings))
  "List of customized variables.
This must be set after the call to function,
`custom-set-variables' (either from within the main init.el file
or the custom file), and not before.  Used by the `mini-set' macro
to determine whether a setting has already been set by the custom
file.")

(defun mini-set-in-user-theme-p (var)
  "Helper function for the `mini-set' macro.
Checks if VAR is declared in the user theme."
  (memq var mini-vars-from-custom-file))

(defmacro mini-set (var val &optional comment)
  "Set VAR's default value to VAL, if not already set.
But if VAR has a custom :set property, use that function to set
VAR's value instead of `set-default'.  Optionally, set VAR's
:variable-comment property to COMMENT.  (This makes it possible
to override settings from the init.el file with settings in the
mini-custom.el file.)"
  (declare (indent 1))
  ;;(declare-function mini-set-in-user-theme-p nil)
  `(unless (mini-set-in-user-theme-p (quote ,var))
     ;; (cond ((string= ,comment "")
     ;; 	    (put (quote ,var) 'variable-comment nil))
     ;; 	   (,comment
     ;; 	    (put (quote ,var) 'variable-comment ,comment)))
     (funcall #'customize-set-variable (quote ,var) ,val ,comment)))


;;; Other

(defmacro mini-pkgif (pkgs &rest forms)
  "Check if any of PKGS are installed.
If so, the FORMS are executed.  PKGS can be either a package
symbol or a list of package symbols.  However, if any of the
packages is included in the `mini-excluded-packages' list, the
FORMS will not be executed."
  (declare (indent 1))
  (unless (listp pkgs)
    (setq pkgs (list `,pkgs)))
  (unless
       (or (cl-intersection `,pkgs
			    mini-excluded-packages) ; ignored packages?

	   ;; (seq-remove
	   ;;  'package-installed-p
	   ;;  ,pkgs))

	   ;; any packages not installed yet?
	   (seq-difference `,pkgs package-selected-packages))

     `(progn ,@forms)))

(defmacro mini-ensure (pkgs &rest forms)
  "Ensure that PKGS are installed.
If already installed, or they can be successfully installed, then
execute FORMS.  However, if any of the packages is included in the
`mini-excluded-packages' list, no PKGS will not be installed, and the
FORMS will not be executed."
  (declare (indent 1))
  (unless (listp pkgs)
    (setq pkgs (list `,pkgs)))
  (unless ; any excluded packages?
      (cl-intersection `,pkgs
		       mini-excluded-packages)
    ;; Install any missing packages.
    `(progn

       ;; (dolist
       ;; 	   (pkg (seq-remove
       ;; 		 'package-installed-p
       ;; 		 (quote ,pkgs)))
       ;; 	 (package-install pkg))

       (dolist (pkg (seq-difference
		     (quote ,pkgs)
		     package-selected-packages))
	 (package-install pkg))

       ,@forms)))

(defmacro mini-bltin (pkgs &rest forms)
  "Check if any of the built-in PKGS are in excluded.
If not, the FORMS are executed.  PKGS can be either a package
symbol or a list of package symbols.  However, if any of the
packages is included in the `mini-excluded-packages' list, the
FORMS will not be executed."
  (declare (indent 1))
  (unless (listp pkgs)
    (setq pkgs (list `,pkgs)))
  (unless
      (cl-intersection `,pkgs
		       mini-excluded-packages)
    `(progn ,@forms)))

(defun mini-nil-if-in-scratch ()
  "Check if ‘current-buffer’ is ‘*scratch*’.

If so, return nil.  If not, return name of buffer.

This goes in ‘kill-buffer-query-functions’, which temporarily
makes whatever buffer is being killed the ‘current-buffer’."
  (let ((buf (buffer-name (current-buffer))))
    (if (equal buf "*scratch*")
        (progn
          (message "Killing the *scratch* buffer has been disabled.")
          nil)
      buf)))

;; Enter numeric arguments using the homerow.
(defun mini-digit-arg ()
  "Call `digit-argument' using homerow keys to set the numbers.
Idea taken from the Vertigo package (see URL
`https://github.com/noctuid/vertigo.el').  But this method
doesn't use any additional `global-map' bindings.  It simply adds
the bindings to `universal-argument-map'.  Use
\\[universal-argument] to access the bindings."
  (interactive)
  (let* ((e last-command-event)
         (last-command-event (plist-get mini-digits e)))
    (call-interactively 'digit-argument)))

;; ;; Bind the above to keys in `universal-argument-map'.
;; (dolist (key (number-sequence 0 9))
;;   (mini-defk (nth (* key 2) mini-digits) 'mini-digit-arg universal-argument-map))

;; (dolist (key (number-sequence 0 9))
;;   (mini-defk (nth (* key 2) mini-digits) nil universal-argument-map))

;; A macro for evaluating forms after a package or list of packages
;; has loaded.
;;
;; Takes either a package symbol or a list of package symbols as first
;; argument and only evaluates the remaining forms after all such
;; packages have loaded.
(defmacro mini-eval (pkgs &rest forms)
  "Evaluate FORMS after PKGS have loaded.
PKGS is either a single package name or a list of packages."
  (declare (indent 1))
  (if pkgs
      (progn (unless (listp pkgs)
               (setq pkgs (list pkgs)))
             `(eval-after-load (quote ,(car pkgs))
                (lambda () (mini-eval ,(cdr pkgs) ,@forms))))
    `(progn ,@forms)))

;; Trying to ease binding of windmove commands...
(defmacro mini-windmove-defk (dirkeys cmdprefixmodlist &optional map)
  "Set up bindings for the windmove commands.

This is a more succinct alternative to the function
`windmove-default-keybindings' and related functions from the
built-in `windmove' package.  It allows for changing the
directional keys, which the other functions do not.  It also
allows for setting the prefix keys, which the default setup
functions only allow for the \"windmove-delete\" commands.
Further, it uses the `mini-defk' macro, which records the
bindings and the command they replaced, so that they can be
reviewed later.

The first argument, DIRKEYS is a list of the four base keys which
coincide with the direction of each command.  It is assumed that
each `windmove' command will use one of these base keys.  The
keys can be either symbols or characters.  The order of the keys
must be given as left, right, up, down.

CMDPREFIXMODLIST is a list of lists, each composed of three
elements.  The car of each sub-list is the command prefix which
uses the listed prefixes and modifiers.  It is given as a string.
The permissible command prefix cars are \"windmove\",
\"windmove-display\", \"windmove-delete\", and \"windmove-swap\".

The car of the cdr of each element is either a prefix key or a
list of prefix keys to be used for calling the commands that
begin with the command prefix.

The cdr of the cdr of each element are the modifier symbols to be
used with commands that begin with the command prefix.

Optionally, a keymap may be specified as MAP, otherwise
`global-map' is used.

Examples:

\(mini-windmove-defk
 (left right up down)
 ((\"windmove\"         nil   control)
  (\"windmove-display\" nil   meta)
  (\"windmove-delete\"  \?\\C\-x control)
  (\"windmove-swap\"    nil   control meta)))

or...

\(mini-windmove-defk
 (left right up down)
 ((\"windmove\"         (\?\\C\-c w) control)
  (\"windmove-display\" (\?\\C\-c w) meta)
  (\"windmove-delete\"  (\?\\C\-c w) control)
  (\"windmove-swap\"    (\?\\C\-c w) control meta)))"
  (dolist (cmdprefixmods cmdprefixmodlist)
    (dotimes (iter 4)
      (mini-defk
       (vconcat
	(if (listp (cadr cmdprefixmods))
	    (cadr cmdprefixmods)
	  (list (cadr cmdprefixmods)))                                   ; prefix keys
	(vector (event-convert-list
		 (append (cddr cmdprefixmods)                  ; modifiers
			 (list (nth iter dirkeys))))))         ; base-key
       (intern (concat
		(car cmdprefixmods)
		(nth iter '("-left" "-right" "-up" "-down")))) ; command
       map))))                                                 ; keymap, if given

(defun mini-xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line.
URL `http://xahlee.info/emacs/emacs/emacs_form_feed_section_paging.html'
Version 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 80 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))

(defun mini-isearch-bor-exit ()
  "Ensure point is at beginning of isearch result and exit.
If `last-command' was `isearch-repeat-backward', simply exit.
Otherwise, call `isearch-repeat-backward' and then
`isearch-exit'."
  (interactive)
  (when (< isearch-other-end (point))
    (goto-char isearch-other-end))
  (call-interactively 'isearch-exit))

(defun mini-isearch-eor-exit ()
  "Ensure point is at end of isearch result and exit."
  (interactive)
  (when (>= isearch-other-end (point))
    (goto-char (+ (point) (length isearch-string))))
  (call-interactively 'isearch-exit))


;;; Menus for built-in packages

;; File menu
(mini-addmenu "file"
  `(["***---" ignore]
    ["HTML-Fontify Buffer" htmlfontify-buffer]
    ["Config Files" (dired (expand-file-name "*.el" user-emacs-directory))]
    ["Org Directory" (progn (require 'org) (dired (expand-file-name org-directory)))]
    ,(let ((dirmenu nil)
	   (source-dirs-menu-name "Emacs Elisp sources"))
       (cons (format "%s" source-dirs-menu-name)
	     (nreverse
	      (dolist (dir load-path dirmenu)
		(when (string-match-p (regexp-quote "lisp") dir)
		  (setq dirmenu
			(cons (vector
			       (format "%s" (expand-file-name dir))
			       `(dired (format "%s"
					       ,(expand-file-name dir))))
			      dirmenu)))))))
    ["Autorevert-Mode" auto-revert-mode]))

;; Edit menu
(mini-addmenu "edit"
  '(["***---" ignore]
    ["Align-Regexp" align-regexp]
    ["Auto-Fill mode" auto-fill-mode]
    ["Subword mode" subword-mode]
    ["Superword mode" superword-mode]
    ;; For vcursor, we open the library, since it appears to be the
    ;; only documentation available.
    ["Vcursor (Load and Find vcursor.el)" (mini-menu-load-and-read 'vcursor)]
    ["DelSel-Mode" delete-selection-mod]))

;; Options menu
(mini-addmenu "options"
  `(["***---" ignore]
    ("View Enhancements"
     ["Fill-Column-Indicator" display-fill-column-indicator-mode]
     ["Hideshow mode" hs-minor-mode]
     ["Highlight-Changes" highlight-changes-mode]
     ["Highlight Current Line" hl-line-mode]
     ["Horizontal Ruler" ruler-mode]
     ["Scroll Lock Mode" scroll-lock-mode]
     ["Hide lines indented past..."
      ,(lambda () (interactive)
	 (if (y-or-n-p "Use selective-display to hide indented lines? ")
	     (set-selective-display
	      (read-number "Enter the minimum indentation of lines to hide: " 4))
	   (set-selective-display nil)))]
     ["Show Trailing Whitespace"
      ,(lambda () (interactive)
	 (setq show-trailing-whitespace (not show-trailing-whitespace)))]
     ["Show All Whitespace" whitespace-mode]
     ["Winner Mode (undo window arrangements)" winner-mode]
     ["View-Mode" view-mode])
    ("Editing Enhancements"
     ["Abbrev-Mode" abbrev-mode])
    ["Eldoc-Mode" eldoc-mode]))

;; Buffers menu
(mini-addmenu 'global-buffers-menu-map
  '(["***---" ignore]
    ["Org-Switchb" org-switchb]
    ["MSB-Mode" msb-mode]))

;; Tools menu
(mini-addmenu "tools"
  '(["***---" ignore]
    ["Dired" dired]
    ["Web Jump" webjump]
    ("Other Apps"
     ("Organizing"
      ["Calendar" calendar]
      ["Diary" (progn (diary) (switch-to-buffer "diary"))]
      ["Org-Agenda" org-agenda]
      ["ToDo-mode" todo-show]
      ["SES: Simple Emacs Spreadsheet" ses-mode])
     ("Communication"
      ["ERC: IRC client" erc]
      ["Gnus: Usenet, Email, and RSS client" gnus]
      ["rcirc: IRC client" rcirc]
      ["Rmail: Email client" rmail])
     ("Media"
      ["MPC: Music Player Daemon client" mpc]
      ["Newsticker: RSS feed reader" newsticker-show-news])
     ("Net"
      ["Dig" dig])
     ("System"
      ["Proced: System process viewer" proced]
      ("Command Lines"
       ["Term" term]
       ["Eshell" eshell]
       ["IELM" ielm]))
     ("Reference"
      ["Dictionary" dictionary]))))


;;; Key translations

;; For consistency with GNU Readline...
;; Use "C-h" for backspace.  (And rely on "<f1>" as the help-key
;; instead of "C-h".)
(when mini-use-C-h-for-backspace
  (mini-xlate "C-h" "DEL"))

;; Tweak for Dvorak layout...  Swap C-t with C-x in *all* key
;; sequences.  Thus, "C-x C-t" (for `upcase-region') becomes "C-t
;; C-x".  And in `org-mode', "C-c C-t" (for `outline-up-heading')
;; becomes "C-c C-x".  And all commands bound under org-mode's "C-c
;; C-x" prefix are instead accessed via "C-c C-t".  There are more
;; complex ways to fine-tune this (See
;; `mini-simulate-C-x-at-C-t-in-Dvorak'), but this is simpler and good
;; enough.
(when mini-swap-C-x-with-C-t
  (mini-xlate "C-t" "C-x")
  (mini-xlate "C-x" "C-t"))

;; Companion to the above for consistency.
(when mini-swap-M-x-with-M-t
  (mini-xlate "M-t" "M-x")
  (mini-xlate "M-x" "M-t"))

;; Companion to the above for consistency.
(when mini-swap-C-M-x-with-C-M-t
  (mini-xlate "C-M-t" "C-M-x")
  (mini-xlate "C-M-x" "C-M-t"))

;; Make C-M-g work as C-g.  Having the space bar do double duty as
;; Ctrl requires a short wait when using Space to enter C-g for
;; exiting the mini-buffer.  This gets annoying when you're
;; occasionally made to enter C-g twice.  But C-M-g has no default
;; binding, and CapsLock works well as a Ctrl+Meta modifier.  If you
;; only give CapsLock one role, there is no delay.  So we can use
;; C-M-g by holding down CapsLock instead of Space, and not have to
;; worry about whether `keyboard-escape-quit' was recognized this
;; time.
(when mini-xlate-C-M-g-to-C-g
  (mini-xlate "C-M-g" "C-g"))


;;; Keybindings

;; Have C-w work normally when region is active, but otherwise call
;; `kill-backward-word'.
(when mini-make-C-w-double-as-backward-kill-word
  (mini-defk "C-w" 'mini-C-w-dwim))

;;; Ctl-x-map
;; Skip the prompt asking which buffer to kill.
(when mini-kill-current-buffer-by-default
  (mini-defk "k"                 'kill-current-buffer ctl-x-map))
;; Easier to reach tab-bar commands
(when mini-easier-tab-bar-bindings
  (mini-defk "t q"               'tab-close           ctl-x-map) ;; "C-x t 0"
  (mini-defk "t k"               'tab-close-other     ctl-x-map) ;; "C-x t 1"
  (mini-defk "t h"               'tab-new             ctl-x-map) ;; "C-x t 2"
  (mini-defk "t s"               'tab-select          ctl-x-map))

;; As an alternative to translating C-x and C-t (and related) keys,
;; instead, simulate a C-x keypress and make a couple of other
;; adjustments for consistency.  The drawbacks are (1) special modes
;; which bind C-t to a command will shadow the C-x-simulated binding.
;; However since the real C-x remains available, that's just a minor
;; inconvenience.  (2)
;; Whether to use this, `mini-swap-C-x-with-C-t' or no
;; change at all in Dvorak/Programmer Dvorak is a matter of
;; preference.  However, `mini-simulate-C-x-at-C-t-in-Dvorak' will
;; only apply when `mini-keyboard-layout' has been set to either
;; 'dvorak or 'programmer-dvorak.
(when (and (memq mini-keyboard-layout '(dvorak programmer-dvorak))
	   (not mini-swap-C-x-with-C-t)
	   (not mini-swap-M-x-with-M-t)
	   (not mini-swap-C-M-x-with-C-M-t)
	   mini-simulate-C-x-at-C-t-in-Dvorak)
  ;; Simulate "C-x" using "C-t" (easier on Dvorak layout)
  (mini-defk "C-t"    (mini-simk ?\C-x))
  ;; Make "C-t C-t" work like "C-x C-x".
  (mini-defk "C-t"    'exchange-point-and-mark  ctl-x-map)
  ;; Make "C-t M-t" work like "C-x C-t" did before.
  (mini-defk "M-t"    'transpose-lines          ctl-x-map))
  ;; Fix `ibuffer' C-t binding

;;; Mode-specific-map *** C-c ***
(when mini-keep-defk-list
  (mini-defk "b"      'mini-show-defk-list      mode-specific-map))  ;; View user bindings + priors.
(when mini-bind-init-file
  (mini-defk "i"      'mini-find-init-file      mode-specific-map))
;; (mini-defk "w q"    'delete-window            mode-specific-map)  ;; Some easier to reach
;; (mini-defk "w k"    'delete-other-windows     mode-specific-map)  ;;   window commands...
;; (mini-defk "w h"    'split-window-below       mode-specific-map)
;; (mini-defk "w v"    'split-window-right       mode-specific-map)
;; (mini-defk "f q"    'delete-frame             mode-specific-map)  ;; Some easier to reach
;; (mini-defk "f k"    'delete-other-frames      mode-specific-map)  ;;   frame commands...
;; (mini-defk "f h"    'make-frame-command       mode-specific-map)
;; (mini-defk "f o"    'other-frame              mode-specific-map)
;; (mini-defk "v"      'view-mode                mode-specific-map)

(when mini-transpose-bindings
  (defvar mini-transpose-prefix-map (make-sparse-keymap))
  (define-prefix-command 'mini-transpose-prefix-command 'mini-transpose-prefix-map)
  (dolist (kb '((?c . transpose-chars)
		(?l . transpose-lines)
		(?s . transpose-sexps)
		(?w . transpose-words)
		(?r . transpose-regions)
		(?e . transpose-sentences)
		(?p . transpose-paragraphs)))
    (mini-defk (car kb) (cdr kb) mini-transpose-prefix-map))
  (mini-defk "t"      'mini-transpose-prefix-command mode-specific-map))


(when mini-kill-bindings
  (defvar mini-kill-prefix-map (make-sparse-keymap))
  (define-prefix-command 'mini-kill-prefix-command 'mini-kill-prefix-map)
  (dolist (kb '((?K . kill-whole-line)
		(?s . kill-sexp)
		(?w . kill-word)
		(?m . kill-region)
		(?c . kill-comment)
		(?e . kill-sentence)
		(?E . backward-kill-sentence)
		(?p . kill-paragraph)
		(?P . backward-kill-paragraph)
		(?r . kill-rectangle)
		(?v . kill-visual-line)
		(?o . kill-matching-lines)
		(?u . kill-backward-up-list)
		(?z . zap-to-char)
		(?Z . zap-up-to-char)))
    (mini-defk (car kb) (cdr kb) mini-kill-prefix-map))
  (mini-defk "k"      'mini-kill-prefix-command mode-specific-map))



(provide 'mini-core)
;;; mini-core.el ends here
