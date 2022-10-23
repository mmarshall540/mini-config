;;; mini-menus.el --- additional menus for the mini-config  -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'mini-core)

;; Menus for built-in packages

;; File menu
(mini-addmenu "file"
  `(,(mini-addmenu-divider)
    ["HTML-Fontify Buffer" htmlfontify-buffer]
    ["Open Init File" mini-find-init-file]
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
    ["Autorevert-Mode" auto-revert-mode]
    ,(when (version< "29" emacs-version)
       ["Restart Emacs" restart-emacs])))

;; Edit menu
(mini-addmenu "edit"
  `(,(mini-addmenu-divider)
    ["Abbrev-Mode" abbrev-mode]
    ["Align-Regexp" align-regexp]
    ["Auto-Fill mode" auto-fill-mode]
    ["Subword mode" subword-mode]
    ["Superword mode" superword-mode]
    ;; For vcursor, we open the library, since it appears to be the
    ;; only documentation available.
    ["Vcursor (Load and Find vcursor.el)" (mini-menu-load-and-read 'vcursor)]
    ["DelSel-Mode" delete-selection-mod]
    ;; ["Kill Commands" mini-kill-prefix-command]
    ;; ["Marking Commands" mini-mark-prefix-command]
    ;; ["Transpose Cmds" mini-transpose-prefix-command]
    ))

;; Options menu
(mini-addmenu "options"
  `(,(mini-addmenu-divider)
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
     ["View-Mode" view-mode])
    ["Eldoc-Mode" eldoc-mode]
    ["Winner Mode (undo window arrangements)" winner-mode]))

;; Buffers menu
(mini-addmenu 'global-buffers-menu-map
  `(,(mini-addmenu-divider)
    ["Ibuffer" ibuffer]
    ["MSB-Mode" msb-mode]
    ["Org-Switchb" org-switchb]))

;; Tools menu
(mini-addmenu "tools"
  `(,(mini-addmenu-divider)
    ("Other Apps"
     ("Organizing"
      ["Diary" (progn (diary) (switch-to-buffer "diary"))]
      ["Org-Agenda" org-agenda]
      ["ToDo-mode" todo-show]
      ["SES: Simple Emacs Spreadsheet" ses-mode])
     ("Communication"
      ["ERC: IRC client" erc]
      ["Gnus: Usenet, Email, and RSS client" gnus]
      ["rcirc: IRC client" rcirc])
     ("Media"
      ["MPC: Music Player Daemon client" mpc]
      ["Newsticker: RSS feed reader" newsticker-show-news])
     ("Utilities"
      ["Dig" dig]
      ["Proced: System process viewer" proced]
      ("Command Lines"
       ["Ansi-Term" ansi-term]
       ["Term" term]
       ["Eshell" eshell]
       ["IELM" ielm]))
     ("Reference"
      ["Dictionary" dictionary]))
    ["Web Jump" webjump]))

(provide 'mini-menus)
;;; mini-menus.el ends here
