;;; init.el --- Minimal Config init file  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is as an attempt at a minimalistic Emacs configuration.  Some
;; of the basic principles followed are:
;;
;; * Use built-in package.el for package management.
;;
;; * Have reasonable default settings and a reasonable set of default
;;   packages to start from.
;;
;; * Provide Numerous intuitive and useful macros.
;;
;; * Changes to keybindings (when set using the included macros
;;   `mini-defk' or `mini-xlate') are tracked and can be viewed by
;;   pressing "C-c b".
;;
;; * Configuration adjusts to the packages that you install or remove.
;;
;; * User options are set through "Easy Customization" (accessible
;;   from the menu-bar via Options : Customize Emacs : Specific
;;   Group).  Select the "mini" group to list settings that are
;;   specific to this configuration).
;;
;; * Make judicious use of the menu-bar, adding menu entries for
;;   various features and thereby improving discoverability.
;;
;; * Provide an advanced environment for developing in Python.  (This
;;   is provided by the `eglot' package.  You must install one of the
;;   Python language servers onto your system to take advantage of
;;   it.  It supports pylsp, pyls, pyright out of the box.)

;; Structure:
;;
;; + Most of the default settings are found in the "mini-packages.el"
;;   file.  There are 3 ways you can override those default settings.
;;
;;   - Settings made from the "Easy Customization" interface override
;;     variable values set using the `mini-set' macro.
;;
;;   - Any set of configurations for a package that are defined using
;;     `mini-pkgif', `mini-bltin', or `mini-ensure' can be ignored by
;;     adding the package to `mini-excluded-packages'.
;;
;;   - You can put your own Emacs Lisp code in "my-settings.el", which
;;     loads after "mini-packages.el".
;;
;; Settings are arrange according to their packages, even for built-in
;; packages.  The only exceptions are those settings defined in
;; Emacs's C code, rather than its elisp packages.  Those settings are
;; placed with the most relevant built-in package's settings.
;;
;; Note that for an external package, the settings defined in
;; "mini-packages.el" will only apply if the package of that name has
;; been installed.  If the associated package is not installed, its
;; group of settings will be skipped.
;;
;; There are three macros used for organizing packages:
;;
;; + `mini-pkgif' takes a package symbol as its first argument,
;;   followed by any number of forms.  As described above, the forms
;;   will only be evaluated if the package has been installed already.
;;
;; + `mini-bltin' is used for built-in packages.  That is, those
;;   packages which are included with Emacs.
;;
;; + `mini-ensure' can be used to ensure that a given external package
;;   has been installed.  This configuration only uses it with
;;   `mini-pkigif' to install related external packages.  Of course,
;;   you can use it within your "my-settings.el" file however you
;;   like.
;;
;; Additionally, there are several macros that may be of use and which
;; can be further explored by reviewing their included documentation
;; (via "M-x describe-function RET [name] RET") or by reviewing the
;; source code.  A few of them are described below.
;;
;; + `mini-defk' is used to define keybindings.  It has a more
;;   convenient syntax than the usual functions provided for that
;;   purpose.  It also adds all bindings to a list which includes the
;;   command previously bound (if any).  This list can be reviewed by
;;   pressing "C-c b".
;;
;; + `mini-xlate' provides a simplified way to define
;;   key-translations.  That is, to cause one key to act as another.
;;   It uses `keyboard-translate' if the keys can be translated using
;;   that function.  Otherwise, it binds the keys in the given
;;   translation map, using `key-translation-map' if none is supplied.
;;
;; + `mini-simk' provides yet another way to cause a key to act as
;;   another.  It works by simulating keypresses and adding them to
;;   `unread-command-events'.  It returns a named command and can be
;;   called directly as the command argument to `mini-defk'.
;;
;; + `mini-set' provides a way to set variables that don't override
;;   settings from the "custom.el" file.  This is useful for setting
;;   defaults and is used widely throughout this configuration.
;;
;; + `mini-addmenu' simplifies the process of adding menu entries and
;;   is also used throughout this configuration.
;;
;; + `mini-eval' waits until all given packages have loaded before
;;   running the included code.  The first argument is either a
;;   package symbol or a list of package symbols.

;;; TODO
;;
;; + Finish implementing the automated selection of bindings based on
;;   the chosen layout.
;;
;; + Make a user option for setting the location of the "my-settings"
;;   file.
;;
;; + Maybe write a macro for non-Elpa-hosted packages that would clone
;;   a package's git repo, and then run `package-install-file' on it.

;;; Code:


;;; Speed-up Startup

(defvar mini-gct-placeholder gc-cons-threshold
  "Hold the default `gc-cons-threshold' value.")

;; Defer garbage collection during startup.
(setq gc-cons-threshold most-positive-fixnum)

;; After startup, go back to normal.
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold mini-gct-placeholder)))


;;; "Easy Customization"

;; Designate the file to which Easy Customization (e.g. "M-x
;; customize-variable RET menu-bar-mode RET") will save settings.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load the Easy Customization settings file if it exists.
(load custom-file 'noerror)



;;; Load the core configuration.

;; This is where functions, macros and customizable options are
;; defined.

(require 'mini-core
	 (expand-file-name "mini-config/mini-core" user-emacs-directory))


;;; Set package defaults

;; These settings only apply if they weren't already set by the custom
;; file that was loaded above.

;; Default list of external packages to install.
;;
;; Whenever any packages get installed or deleted, Emacs updates the
;; `package-selected-packages' variable and saves its new value to the
;; `custom-file'.  Therefore, the following declaration only has an
;; effect the first time you load this config.  From that point on, it
;; will be ignored, unless you erase that variable's customization.
(mini-set package-selected-packages
  '( ace-isearch avy consult consult-eglot corfu eglot eldoc embark lin magit
     marginalia minimap orderless org-modern project pulsar treemacs vertico
     xref yasnippet ))

;; Add packages that require Emacs 28 if we have that.
(when (version< "28" emacs-version)
  (append package-selected-packages
	  '(vundo modus-themes)))

;; Add Melpa and Nongnu repos.
(mini-set package-archives
  '(("gnu"	. "https://elpa.gnu.org/packages/")
    ("nongnu"	. "https://elpa.nongnu.org/nongnu/")
    ("melpa"	. "https://melpa.org/packages/")))

;; Give preference to packages from Gnu and Nongnu Elpa, since they
;; might be curated more carefully.  But still allow packages from
;; Melpa that aren't otherwise available.
(mini-set package-archive-priorities '(("gnu" . 2) ("nongnu" . 1)))

;; Make sure package-archives have been downloaded at least once.
(unless
    (file-exists-p
     (expand-file-name
      "elpa/archives/gnu/archive-contents"
      user-emacs-directory))
  (package-refresh-contents))


;;; Install any selected but not-yet-installed packages.

(package-install-selected-packages)


;;; Configure installed and built-in packages.

(require 'mini-packages
	 (expand-file-name "mini-config/mini-packages" user-emacs-directory))


;;; Load the handcrafted settings file, if it exists.

(load (expand-file-name "my-settings" user-emacs-directory) t)

(provide 'init)
;;; init.el ends here
