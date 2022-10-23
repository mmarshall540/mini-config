;;; mini-mode-line.el --- mini configuration of mode-line  -*- lexical-binding: t; -*-

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

;; Mode-line customizations and mode-name customizations for built-in
;; modes go here.

;;; Code:

(require 'mini-core)

;; Mode-line customizations


;;; Mode-line mode-name dimming and replacing for built-in modes

;; Major-mode replacements
(mini-mode-rename 'cperl-mode	      "🐪")
(mini-mode-rename 'emacs-lisp-mode    "𝝠")
(mini-mode-rename 'help-mode	      "❓")
(mini-mode-rename 'electric-help-mode "🔌❓")
(mini-mode-rename 'Info-mode	      "📖")
(mini-mode-rename 'java-mode	      "☕")
(mini-mode-rename 'org-mode	      "📓")
(mini-mode-rename 'org-agenda-mode    "📅")
(mini-mode-rename 'python-mode	      "🐍")

;; Minor-mode replacements
(mini-mode-rename 'isearch-mode
		  (propertize " 🔎" 'face '(:foreground "firebrick")) 'minor)
(mini-mode-rename 'visual-line-mode   " VL"     'minor)
(mini-mode-rename 'auto-fill-function " ¶"      'minor)
(mini-mode-rename 'view-mode
		  (propertize " [VIEW]" 'face '(:foreground "cyan")) 'minor)
(mini-mode-rename 'outline-minor-mode " ➤"      'minor)
(mini-mode-rename 'org-indent-mode    ""       'minor)


;;; Sections of mode-line

(mini-set
    mode-line-modified
  (list '(:eval
	  (propertize
	   ;; Set read-only-indicator as a lock symbol.
	   (if buffer-read-only "🔒" "")
	   'help-echo 'mode-line-read-only-help-echo
	   'local-map (purecopy (make-mode-line-mouse-map
				 'mouse-1
				 #'mode-line-toggle-read-only))
	   'mouse-face 'mode-line-highlight))
	;; Set modified-indicator as a solid yellow circle.
	'(:eval
	  (propertize
	   (if (buffer-modified-p) "🔴" "")
	   'help-echo 'mode-line-modified-help-echo
	   'local-map (purecopy (make-mode-line-mouse-map
				 'mouse-1 #'mode-line-toggle-modified))
	   'mouse-face 'mode-line-highlight
	   'face '(:foreground "yellow")))))

(mini-set
    mode-line-remote
  (list '(:eval
	  (propertize
	   ;; "%1@"
	   (if (file-remote-p default-directory) "📡" "")
	   'mouse-face 'mode-line-highlight
	   'help-echo (purecopy (lambda (window _object _point)
 				  (format "%s"
					  (with-selected-window window
					    (if (stringp default-directory)
						(concat
						 (if (file-remote-p default-directory)
						     "Current directory is remote: "
						   "Current directory is local: ")
						 default-directory)
					      "Current directory is nil")))))))))

;; Move `which-function' to end of mode-line.
(mini-set
    mode-line-misc-info
  (list '(global-mode-string
	  ("" global-mode-string))
	'(which-function-mode
	  (which-func-mode
	   (" " which-func-format)))))

;; Reduce spacing after column number.
(mini-set
    mode-line-position
  (list '(:propertize
	  ("" mode-line-percent-position)
	  local-map #1=(keymap
			(mode-line keymap
				   (down-mouse-1 keymap
						 (column-number-mode menu-item . #2=("Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
										     (:toggle . column-number-mode)))
						 (line-number-mode menu-item . #3=("Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
										   (:toggle . line-number-mode)))
						 (size-indication-mode menu-item . #4=("Display Size Indication" size-indication-mode :help "Toggle displaying a size indication in the mode-line" :button
										       (:toggle . size-indication-mode)))
						 #5="Toggle Line and Column Number Display")))
	  display
	  (min-width
	   (5.0))
	  mouse-face mode-line-highlight help-echo "Window Scroll Percentage
mouse-1: Display Line and Column Mode Menu")
	'(size-indication-mode
	  (8
	   #(" of %I" 0 6
	     (local-map #1# mouse-face mode-line-highlight help-echo "Size indication mode
mouse-1: Display Line and Column Mode Menu"))))
	'(line-number-mode
	  (column-number-mode
	   (column-number-indicator-zero-based
	    (10
	     (:propertize mode-line-position-column-line-format
			  ;; display
			  ;; (min-width
			  ;;  (10.0))
			  . #6=(local-map
				(keymap
				 (mode-line keymap
					    (down-mouse-1 keymap
							  (column-number-mode menu-item . #2#)
							  (line-number-mode menu-item . #3#)
							  (size-indication-mode menu-item . #4#)
							  #5#)))
				mouse-face mode-line-highlight help-echo "Line number and Column number
mouse-1: Display Line and Column Mode Menu")))
	    (10
	     (:propertize
	      (:eval
	       (string-replace "%c" "%C"
			       (car mode-line-position-column-line-format)))
	      ;; display
	      ;; (min-width
	      ;;  (10.0))
	      . #6#)))
	   (6
	    (:propertize mode-line-position-line-format display
			 (min-width
			  (6.0))
			 . #6#)))
	  (column-number-mode
	   (column-number-indicator-zero-based
	    (6
	     (:propertize mode-line-position-column-format display
			  (min-width
			   (6.0))
			  . #6#))
	    (6
	     (:propertize
	      (:eval
	       (string-replace "%c" "%C"
			       (car mode-line-position-column-format)))
	      display
	      (min-width
	       (6.0))
	      . #6#)))))))

;; Improve look of `line-number-mode' and `column-number-mode'.
(mini-set mode-line-position-column-line-format '("%l ǀ %c"))

;; This is not actually defined in bindings.el.  It's in C code, but
;; it fits with the above variables.
(mini-set
    mode-line-format
  (list "%e" 'mode-line-front-space
	'(:propertize
	  ;; ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
	  ("" mode-line-client mode-line-modified mode-line-remote)
	  'display
	  '(min-width
	    (0.0)))
	'mode-line-frame-identification 'mode-line-buffer-identification "   " 'mode-line-position
	'(vc-mode vc-mode)
	"  " 'mode-line-modes 'mode-line-misc-info 'mode-line-end-spaces))

(provide 'mini-mode-line)
;;; mini-mode-line.el ends here
