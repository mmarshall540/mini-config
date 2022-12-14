;;; mini-packages.el --- shorter configurations  -*- lexical-binding: t -*-

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

;; Configuration is organized by the relevant packages.  For those
;; packages configured with `mini-pkgif', the configuration will be
;; skipped, unless the package is a member of
;; `package-selected-packages'.
;; ---
;; Press "M-i" to select a package config using `consult-imenu'.
;; ---
;; Package types:
;; + built-in
;; + installed by package.el via package-install
;; + installed by package.el via package-install-file
;; + installed by package.el via package-vc-unpack
;; + installed by operating system (e.g. mu4e)

;;; Code:

(require 'mini-core)


;;; Abbrev (built-in)

(mini-bltin abbrev
  ;; Cause the text-mode-abbrev-table to be used in comments and
  ;; strings in programming modes.
  (defvar mini-original-abbrev-expand-function)
  (set 'mini-original-abbrev-expand-function abbrev-expand-function)
  (defun mini-abbrev-expand-function ()
    (if (or (not (derived-mode-p 'prog-mode))
            (not (memq (get-pos-property (point) 'font)
                       '(face-lock-comment-face font-lock-string-face))))
        ;; Performs normal expansion.
        (funcall mini-original-abbrev-expand-function)
      ;; We're inside a comment: use the text-mode abbrevs.
      (let ((local-abbrev-table text-mode-abbrev-table))
        (funcall mini-original-abbrev-expand-function))))
  (mini-set abbrev-expand-function 'mini-abbrev-expand-function))


;;; Align (built-in)

(mini-bltin align
  (mini-set align-to-tab-stop nil))


;;; Autoinsert (built-in)

(mini-bltin autoinsert
  (defvar auto-insert-alist)
  (add-hook 'after-init-hook 'auto-insert-mode)
  (mini-eval autoinsert
    ;; Don't try to auto-insert when the custom-file is created.
    (add-to-list
     'auto-insert-alist
     '("custom.el" . (ignore)))))


;;; Autorevert (built-in)

(mini-bltin autorevert
  (run-at-time 4 nil 'global-auto-revert-mode))


;;; Avy

(mini-pkgif avy
  (autoload 'avy-with "avy")

  (mini-set avy-all-windows nil)
  (mini-set avy-all-windows-alt t)
  (mini-set avy-case-fold-search nil)
  (mini-set avy-column-line-overlay t)
  (mini-set avy-dispatch-alist ;; Must not conflict with keys in avy-keys
    '((?x . avy-action-kill-move) ;; Move point to item and kill it.
      (?X . avy-action-kill-stay) ;; (default ?X) Kill item but leave point at current location.
      (?p . avy-action-teleport) ;; (default ?t, alternative is ?p which is mnemonic for Pull or telePort) Kill item and yank it to point.
      (?m . avy-action-mark) ;; Mark item and activate region (mark at beginning and point at end).
      (?c . avy-action-copy) ;; (default ?n) Copy item to kill-ring.
      (?y . avy-action-yank) ;; Copy and yank item to point.
      (?Y . avy-action-yank-line) ;; (default ?Y) Copy item up to end-of-its-line and yank to present point.
      (?$ . avy-action-ispell) ;; (default ?i, alt: $ because that key is used for `ispell-word') Run spell check on item.
      (?z . avy-action-zap-to-char)))
  (mini-set avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (mini-set avy-indent-line-overlay nil)
  (mini-set avy-orders-alist
    '((avy-goto-char	     . avy-order-closest)
      (avy-goto-char-2	     . avy-order-closest)
      (avy-goto-word-0	     . avy-order-closest)
      (avy-goto-word-0-above . avy-order-closest)
      (avy-goto-word-0-below . avy-order-closest)
      (avy-isearch	     . avy-order-closest)
      (avy-goto-line-above   . avy-order-closest)))

  (mini-defk "C-;" 'avy-goto-char-2-dwim)
  (mini-defk ?\M-p          'avy-goto-word-0-above)
  (mini-defk ?\M-n          'avy-goto-word-0-below)

  ;; isearch
  (mini-eval isearch
    (mini-defk ?\C-'        'avy-isearch           isearch-mode-map))

  ;; M-g prefix
  ;; (mini-defk ?c             'avy-goto-char         goto-map) ;; conflicts with goto-char and is duplicative.
  (mini-defk ?\M-c          'avy-goto-char-2       goto-map)
  (mini-defk ?w             'avy-goto-word-1       goto-map) ;; any word on the screen
  (mini-defk ?0             'avy-goto-word-0       goto-map)
  (mini-defk ?\M-b          'avy-goto-word-1-above goto-map)
  (mini-defk ?\M-f          'avy-goto-word-1-below goto-map)
  (mini-defk ?\M-g          'avy-goto-line         goto-map) ;; replaces 'goto-line
  (mini-defk ?g             'avy-goto-line         goto-map) ;; "M-g g" or "M-g M-g"
  (mini-defk ?1             'avy-goto-word-1       goto-map)
  (mini-defk ?t             'avy-goto-char-timer   goto-map)
  ;; (mini-defk ?r             'avy-prev              goto-map)
  ;; (mini-defk ?s             'avy-next              goto-map)

  (defun avy-goto-char-2-in-line (char1 char2)
    "Jump to the currently visible CHAR1 followed by CHAR2.
Scope is limited to the current line."
    (interactive (list (read-char "char 1: " t)
                       (read-char "char 2: " t)))
    (avy-with avy-goto-char-2
      (avy-jump
       (regexp-quote (string char1 char2))
       :beg (line-beginning-position)
       :end (line-end-position))))

  (defun mini-over-one-nil (list)
    "If LIST longer than 1, return nil, otherwise LIST."
    (if (> (length list) 1)
	nil
      list))

  (defun avy-goto-char-2-dwim (&optional arg beg end)
    ;; Check line:
    ;; * no results --> repeat search in window scope (done)
    ;; * exactly one result --> jump to it (done)
    ;; * multiple results --> repeat search in window scope (or present
    ;; in-line candidates but allow switching to window-scope with
    ;; `keyboard-quit'. this option is done)
    ;;
    ;; If we are invoking immediately after landing on a single in-line
    ;; result from this same command, then go straight to window-scope
    ;; and repeat the search without needing to reenter the
    ;; characters. To implement this, we would need to 1. Check
    ;; `last-command'. 2. Store the characters and the result in a
    ;; variable, or at least the result, because Avy must already be
    ;; storing the characters somewhere or else `avy-next' and
    ;; `avy-resume' wouldn't be things... But it's probably enough to
    ;; just use `last-command', because you can always interrupt with
    ;; `keyboard-quit' to prevent repetition.
    ;;
    ;; TODO Provide a way to switch to the larger scope when there was
    ;; only one result on the current line which was automatically
    ;; jumped to, since that would otherwise end the search.
    ;;
    ;; Idea: If already positioned at the one result, then expand the
    ;; scope. But for this to be useful, there needs to be an easy way
    ;; to repeat the search without reentering it, so you don't have to
    ;; type the whole thing in again... So we could check the
    ;; last-command variable, and if we just did the same command, and
    ;; it landed us on a single result within the current-line scope,
    ;; simply repeat the same search with the wider scope?

    ;; (Is `avy-resume' useful for this?  Or maybe it would be better to
    ;; add advice to `keyboard-quit'.  Then, you can use C-g to expand
    ;; the scope either way...  Actually, whenever there are more than
    ;; one result on the current line, it shouldn't require pressing C-g
    ;; at all.  Rather, it should automatically expand the scope in that
    ;; case, because it doesn't cost any extra keystrokes to stay on the
    ;; current line).  So you would only need to press another key if
    ;; you've landed on a result within the line but didn't want to
    ;; restrict it to the line.  Maybe repeating the command would cause
    ;; it to check if it was the last command and if so also check if it
    ;; landed on target without having to choose one. ... Once this is
    ;; done, you can get rid of the separate binding for
    ;; avy-goto-char-2.

    "Jump to the currently visible CHAR1 followed by CHAR2.
Scope is initially limited to the current line.  But if no
candidate is found, expand to the scope as determined by
`avy-all-windows', unless ARG is non-nil, in which case, do the
opposite of `avy-all-windows'.  If there are multiple results on
the current line, select one to go there, or press
\\[keyboard-quit] to expand the scope.  If region is active, BEG
and END define the scope where candidates are searched if not
initially found within the current line."
    (interactive (list current-prefix-arg
		       nil nil))
    (defvar jumpoffpt)
    (setq jumpoffpt (point))
    (if (eq this-command last-command)
	;; If repeating, just call `avy-resume'.
	(avy-resume)
      ;; Else, get the chars and attempt to find targets...
      (let ((char1 (read-char "char 1: " t))
            (char2 (read-char "char 2: " t)))
	;; Try the current line.  And if there is more than one
	;; result, go straight to window-scope.
	(avy-with avy-goto-char-2
	  (advice-add 'avy--regex-candidates :filter-return 'mini-over-one-nil)
	  (avy-jump
	   (regexp-quote (string char1 char2))
	   :beg (line-beginning-position)
	   :end (line-end-position)
	   :pred
	   (lambda ()
	     ;; Check if match target is point.
	     (/= (+ 2 jumpoffpt) (point))))
	  (advice-remove 'avy--regex-candidates 'mini-over-one-nil))
	(if (eq 1 (length avy-last-candidates))
	    ;; If that works, just set avy-resume to window-scope.
	    (setf (symbol-function 'avy-resume)
		  (lambda ()
		    (interactive)
		    (avy-with avy-goto-char-2
		      (avy-jump
		       (regexp-quote (string char1 char2))
		       :beg beg
		       :end end
		       :window-flip arg
		       :pred
		       (lambda () (/= (+ 2 jumpoffpt) (point)))))))
	  ;; Otherwise, do an actual attempt with window-scope.
	  ;; Note that this also sets avy-resume to window-scope, so
	  ;; that if we flub target-entry, we can quickly reattempt.
	  (avy-with avy-goto-char-2
	    (avy-jump
	     (regexp-quote (string char1 char2))
	     :beg beg
	     :end end
	     :window-flip arg
	     :pred
	     (lambda () (/= (+ 2 jumpoffpt) (point)))))))))

  (mini-defk "C-," 'avy-goto-char-2-dwim))


;;; Bindings (built-in)

;; C-c map
;; Keep descriptions to 25 chars or less.
(dolist (submap
	 `((("a" org-agenda "Org Agenda"))
	   (("c" org-capture "Org Capture"))
	   (("d" denote "Denote"))
	   (("e" delete-pair "Erase Pairs"))
	   ;; (("e" mini-leader-editing-map "Editing")
	   ;; ("c" 'cape-prefix)
	   ;; ("k" 'mini-kill-prefix-command "Kill Commands")
	   ;; ("m" 'mini-mark-prefix-command "Marking Commands")
	   ;; ("t" 'mini-transpose-prefix-command "Transpose Cmds"))
	   (("l" org-store-link "Org Store Link"))
	   (("n" next-buffer "Next Buffer"))
	   (("p" previous-buffer "Previous Buffer"))
	   ;; (("o" ,(mini-simk "C-x o") "Other Window"))
	   ;; (("p" project-prefix-map "group:Project Cmds")) ;; ,(mini-simk "C-x p")
	   ;; (("r" ,(mini-simk "C-x r") "group:Rect/Reg/Bkmrks")) ;; ctl-x-r-map
	   ;; (("s" ,(mini-simk "C-x C-s") "Save File"))
	   ;; (("t" ,(mini-simk "M-`") "group:TMM Menu-Bar")) ;; not really a prefix, but acts similarly
	   ;; (("v" vc-prefix-map "Version Control")) ;; ,(mini-simk "C-x v")
	   (("w" mini-leader-windows-map "Windows/Tabs/Frames")
	    ("c" ,(mini-simk "C-x 6") "group:2-Columns - C-x 6") ; *
	    ("f" ,(mini-simk "C-x 5") "group:Other Frame - C-x 5") ; *
	    ("h" ,(mini-simk "C-x 2") "Split Horiz - C-x 2")
	    ("o" ,(mini-simk "C-x 4") "group:Other Window - C-x 4") ; *
	    ("q" ,(mini-simk "C-x 0") "Quit Current Window")
	    ("k" ,(mini-simk "C-x 1") "Kill Other Windows")
	    ("t" ,(mini-simk "C-x t") "group:Tab-Bar - C-x t") ; *
	    ("v" ,(mini-simk "C-x 3") "Split Vert - C-x 3"))))
  ;; Create prefix map and command.
  (unless (= 1 (length submap))
    ;; Make a prefix-map out of the first item in the list.
    (define-prefix-command (cadar submap)))
  ;; Bind it in mode-specific-map ("C-c" prefix).
  (mini-defk (caar submap)
	     (cadar submap)
	     mode-specific-map (caddar submap))
  ;; Remaining items are bound in the prefix map.
  (when (cdr submap)
    (dolist (smbinding (cdr submap)) ; smbinding = everything in an item except for the key.
      (eval `(mini-defk ,(car smbinding) (quote ,(cadr smbinding))
			,(cadar submap) ,(caddr smbinding))))))

(defvar mini-leader-windows-map)
(dolist (submap '((("m" mini-leader-windmove-map "Windmove")
		   ("h" windmove-left)
		   ("t" windmove-right)
		   ("p" windmove-up)
		   ("n" windmove-down))
 		  (("d" mini-leader-windmove-display-map "Windmove-Display")
		   ("h" windmove-display-left)
		   ("t" windmove-display-right)
		   ("p" windmove-display-up)
		   ("n" windmove-display-down)
		   ("S" windmove-display-same-window)
		   ("T" windmove-display-new-tab)
		   ("F" windmove-display-new-frame))
		  (("D" mini-leader-windmove-delete-map "Windmove-Delete")
		   ("h" windmove-delete-left)
		   ("t" windmove-delete-right)
		   ("p" windmove-delete-up)
		   ("n" windmove-delete-down))
		  (("s" mini-leader-windmove-swap-map "Windmove-Swap")
		   ("h" windmove-swap-states-left)
		   ("t" windmove-swap-states-right)
		   ("p" windmove-swap-states-up)
		   ("n" windmove-swap-states-down))))
  ;; Create prefix map and command.
  ;; Make a prefix-map out of the first item in the list.
  (define-prefix-command (cadar submap))
  ;; Bind it in mini-leader-windows-map.
  (mini-defk (caar submap)
	     (cadar submap)
	     mini-leader-windows-map (caddar submap))
  ;; Remaining items are bound in the prefix map.
  (dolist (smbinding (cdr submap)) ; smbinding = everything in an item except for the key.
    (eval `(mini-defk ,(car smbinding) (quote ,(cadr smbinding))
		      ,(cadar submap))))
  ;; This is necessary, because merely loading windmove.el enables
  ;; `windmove-mode'.  The `defcustom' declaration for each
  ;; `windmove-...-default-keybindings' variable calls a function in
  ;; its :set properties which copies all keybindings for windmove
  ;; commands to `windmove-mode-map', which then shadows the "C-c w"
  ;; binding defined above.  But it doesn't copy the description
  ;; strings, so in order for `which-key' to show the description
  ;; strings again, we have to disable `windmove-mode', so its copied
  ;; bindings won't shadow the descriptions for the prefix keys
  ;; defined above.
  (mini-eval windmove
    (windmove-mode -1)))


;;; Cape

(mini-pkgif cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  (dolist (pair '(("p p" . completion-at-point) ;; capf
		  ("p t" . complete-tag)        ;; etags
		  ("p d" . cape-dabbrev)        ;; or dabbrev-completion
		  ("p h" . cape-history)
		  ("p f" . cape-file)
		  ("p k" . cape-keyword)
		  ("p s" . cape-symbol)
		  ("p a" . cape-abbrev)
		  ("p i" . cape-ispell)
		  ("p l" . cape-line)
		  ;; ("p w" . cape-dict)
		  ("p \\" . cape-tex)
		  ("p _" . cape-tex)
		  ("p ^" . cape-tex)
		  ("p &" . cape-sgml)
		  ("p r" . cape-rfc1345)))
    ;; (mini-defk (car pair) (cdr pair) mode-specific-map)
    )

  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions 'cape-dabbrev)
  (add-to-list 'completion-at-point-functions 'cape-file)
  (add-to-list 'completion-at-point-functions 'cape-history)
  (add-to-list 'completion-at-point-functions 'cape-keyword)
  (add-to-list 'completion-at-point-functions 'cape-tex)
  (add-to-list 'completion-at-point-functions 'cape-sgml)
  (add-to-list 'completion-at-point-functions 'cape-rfc1345)
  (add-to-list 'completion-at-point-functions 'cape-abbrev)
  (add-to-list 'completion-at-point-functions 'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions 'cape-dict)
  (add-to-list 'completion-at-point-functions 'cape-symbol)
  (add-to-list 'completion-at-point-functions 'cape-line))


;;; Checkdoc (built-in)

(mini-bltin checkdoc
  ;; Too many false positives, as the original function
  ;; assumed that every period was a sentence ending.
  (mini-eval checkdoc
    (defun checkdoc-sentencespace-region-engine (begin end)
      (ignore begin end))))


;;; Consult

(mini-pkgif consult

  ;; Use `consult-completion-in-region' if Vertico is enabled and
  ;; neither Company nor Corfu are installed.  Otherwise use the default
  ;; `completion--in-region' function.
  (defvar vertico-mode)
  (declare-function consult-completion-in-region "consult")
  (unless (cl-intersection '(company corfu) package-selected-packages)
    (mini-set completion-in-region-function
      (lambda (&rest args)
	(apply (if vertico-mode
                   #'consult-completion-in-region
		 #'completion--in-region)
               args))))
  (mini-set consult-imenu-config
    '((emacs-lisp-mode :toplevel "Headings" :types
		       ((102 "Functions" font-lock-function-name-face)
			(109 "Macros" font-lock-function-name-face)
			(112 "Packages" font-lock-constant-face)
			(116 "Types" font-lock-type-face)
			(118 "Variables" font-lock-variable-name-face)))))
  (mini-set consult-narrow-key [60])
  (defvar mini-consult-prefix-map (make-sparse-keymap))
  (define-prefix-command 'mini-consult-prefix-command 'mini-consult-prefix-map)
  ;; (mini-defk [?u] 'mini-consult-prefix-command mode-specific-map)

  (dolist (kb '(("h" . consult-history)
		("m" . consult-mode-command)
		("k" . consult-kmacro)
		("t" . consult-theme)))
    (mini-defk (car kb) (cdr kb) mini-consult-prefix-map))
  ;; C-x bindings (ctl-x-map)
  (dolist (kb '(("M-:" . consult-complex-command)     ;; orig. repeat-complex-command
		("b" . consult-buffer)                ;; orig. switch-to-buffer
		("4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
		("5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
		("r b" . consult-bookmark)            ;; orig. bookmark-jump
		("p b" . consult-project-buffer)))    ;; orig. project-switch-to-buffer
    (mini-defk (car kb) (cdr kb) ctl-x-map))
  ;; Custom M-# bindings for fast register access
  (dolist (kb '(("M-#" . consult-register-load)
		("M-'" . consult-register-store)      ;; orig. abbrev-prefix-mark (unrelated)
		("C-M-#" . consult-register)
		;; Other custom bindings
		("M-i" . consult-imenu)
		("M-y" . consult-yank-pop)))          ;; orig. yank-pop
    (mini-defk (car kb) (cdr kb)))
  (mini-defk "a" 'consult-apropos help-map)             ;; orig. apropos-command
  ;; M-g bindings (goto-map)
  (dolist (kb '(("e" . consult-compile-error)
		;; ("g" . consult-goto-line)          ;; orig. goto-line
		;; ("M-g" . consult-goto-line)        ;; orig. goto-line
		;; ("o" . consult-outline)            ;; Alternative: consult-org-heading
		("m" . consult-mark)
		("k" . consult-global-mark)
		;; ("i" . consult-imenu)
		("I" . consult-imenu-multi)))
    (mini-defk (car kb) (cdr kb) goto-map))
  (mini-eval org
    (defvar org-mode-map)
    (mini-defk [?\M-g ?h] 'consult-org-heading org-mode-map))
  ;; M-s bindings (search-map)
  (dolist (kb '(("d" . consult-find)
		("D" . consult-locate)
		("g" . consult-grep)
		("G" . consult-git-grep)
		("r" . consult-ripgrep)
		("l" . consult-line)
		("L" . consult-line-multi)
		("m" . consult-multi-occur)
		("k" . consult-keep-lines)
		("u" . consult-focus-lines)
		;; Isearch integration
		("e" . consult-isearch-history)
		("t" . consult-imenu)))
    (mini-defk (car kb) (cdr kb) search-map))
  (mini-eval isearch
    (dolist (kb '(;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string (conflicting and duplicative)
		  ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
		  ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
		  ("M-s L" . consult-line-multi)))          ;; needed by consult-line to detect isearch
      (mini-defk (car kb) (cdr kb) isearch-mode-map)))
  ;; Minibuffer history
  (dolist (kb '(("M-s" . consult-history)                 ;; orig. next-matching-history-element
		("M-r" . consult-history)))               ;; orig. previous-matching-history-element
    (mini-defk (car kb) (cdr kb) minibuffer-local-map))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (mini-set register-preview-delay 0.5)
  (when (fboundp 'consult-register-format)
    (mini-set register-preview-function #'consult-register-format))

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (when (fboundp 'consult-register-window)
    (advice-add #'register-preview :override #'consult-register-window))

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (when (fboundp 'consult-completing-read-multiple)
    (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

  ;; Use Consult to select xref locations with preview
  (when (fboundp 'consult-xref)
    (mini-set xref-show-xrefs-function #'consult-xref)
    (mini-set xref-show-definitions-function #'consult-xref))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (mini-set consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; Configure other variables and modes after lazily loading the
  ;; package.

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (mini-set consult-preview-key 'any)
  ;; (mini-set consult-preview-key (kbd "M-."))
  ;; (mini-set consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-recent-file
  ;;  consult--source-project-recent-file
  ;;  :preview-key (kbd "M-."))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;    1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;    2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;    3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;    4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))

  (mini-defk ?f 'consult-flymake goto-map))


;;; Corfu

(mini-pkgif corfu
  ;; (mini-set corfu-auto-delay 0.4)
  ;; (mini-set corfu-auto-prefix 4)
  (mini-set corfu-auto nil)
  (run-at-time 2 nil
	       (lambda () (add-hook 'window-state-change-hook 'corfu-mode))))


;;; Cperl-mode (built-in)

(mini-bltin cperl-mode
  (defvar cperl-mode-map)
  (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . 'cperl-mode))
  (dolist (intr '("perl" "perl5" "miniperl"))
    (add-to-list 'interpreter-mode-alist (cons intr 'cperl-mode)))
  (define-auto-insert
    '(cperl-mode . "Perl Program")
    '(nil
      "#! /usr/bin/perl\n\n"
      "# File: " (file-name-nondirectory buffer-file-name) "\n"
      "# Created: " (format-time-string "%Y-%m-%d") "\n#\n"
      "# Copyright (C) " (format-time-string "%Y")
      " by " user-full-name "\n#\n"
      "# Author: " user-full-name "\n#\n"
      "# Description:\n\n"
      "use warnings;\nuse strict;\n")))


;;; Darkroom

(mini-pkgif darkroom
  (defvar mini-darkroom-last-tab-bar-state)
  (defvar darkroom-mode)
  (defun mini-darkroom-tab-bar-check ()
    (if darkroom-mode
	(progn
          (setq mini-darkroom-last-tab-bar-state tab-bar-mode)
	  (tab-bar-mode 0))
      (when mini-darkroom-last-tab-bar-state
	(tab-bar-mode 1))))
  (add-hook 'darkroom-mode-hook 'mini-darkroom-tab-bar-check)
  (add-hook 'darkroom-tentative-mode-hook 'mini-darkroom-tab-bar-check)
  (mini-defk "<f7>" 'darkroom-mode)
  (mini-eval menu-bar
    (mini-addmenu "options"
      '(["Darkroom-Mode" darkroom-mode]))))


;;; Delsel (built-in)

(mini-bltin delsel
  (run-at-time 3 nil 'delete-selection-mode))


;;; Denote

(mini-pkgif denote
  (add-hook 'find-file-hook 'denote-link-buttonize-buffer)
  (add-hook 'dired-mode-hook 'denote-dired-mode)
  (mini-eval org-capture
    (add-to-list 'org-capture-templates
		 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))


;;; Diff-Mode (built-in)

(mini-bltin diff-mode
  (mini-eval diff-mode
    (defvar diff-mode-map)
    ;; Conflicts with the "M-o" `other-window' binding.
    (mini-defk "ESC o" nil diff-mode-map)))


;;; Dired (built-in)

(mini-bltin dired
  (mini-eval which-key
    (mini-eval dired
      (defvar dired-mode-map)
      (mini-defk [remap dired-summary]
		 'which-key-show-major-mode dired-mode-map))))


;;; Dired-aux (built-in)

(mini-bltin dired-aux
  (mini-eval dired
    (require 'dired-aux)))


;;; Dired-x (built-in)

(mini-bltin dired-x
  (mini-eval dired
    (require 'dired-x)))


;;; Eglot (built-in, if version > 29)

(when
    (or (version< "29" emacs-version)
	(memq 'eglot package-selected-packages))
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'java-mode-hook 'eglot-ensure))


;;; Eglot-java

(mini-pkgif eglot-java
  (mini-eval eglot
    (eglot-java-init)))


;;; Ehelp (built-in)

(mini-bltin ehelp
  (when mini-use-electric-help
    (autoload 'ehelp-command "ehelp"
      "Prefix command (definition is a keymap associating keystrokes with commands)."
      nil 'keymap)
    (if mini-use-C-h-for-backspace
	(mini-defk "h"   'ehelp-command mode-specific-map "Help")
      (mini-defk "C-h" 'ehelp-command))  ;; electric-help
    (mini-defk "<f1>"   'ehelp-command)  ;; electric-help
    (mini-defk "<help>" 'ehelp-command)  ;; electric-help
    (defvar ehelp-map)))


;;; Eldoc (built-in)

(mini-bltin eldoc
  (add-hook 'prog-mode-hook 'eldoc-mode))


;;; Elec-pair (built-in)

(mini-bltin elec-pair
  (mini-set electric-pair-inhibit-predicate 'ignore)
  (mini-set electric-pair-skip-self t)
  (mini-set electric-pair-text-pairs
    '((34 . 34) (8216 . 8217) (8220 . 8221) (42 . 42)))
  (run-at-time 2 nil
	       'electric-pair-mode))


;;; Embark

(mini-pkgif embark
  (mini-defk "C-." 'embark-act)
  (mini-defk "M-." 'embark-dwim)
  (mini-defk "B"   'embark-bindings help-map)

  (mini-set prefix-help-command 'embark-prefix-help-command)

  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))

  ;; If both 'embark and 'consult are installed, ensure that
  ;; 'embark-consult is also installed, and load it once both of them
  ;; have loaded.
  (mini-pkgif (consult)
    (mini-ensure embark-consult
      (mini-eval (embark consult)
	(require 'embark-consult)
	(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))))

  ;; Add avy-dispatch for embark
  (mini-pkgif (embark avy)
    (defvar avy-ring)
    (declare-function ring-ref "ring")
    (declare-function embark-act "embark")
    (declare-function embark-dwim "embark")
    (defun avy-action-embark (pt)
      "Call `embark-act' on item at PT.
From https://karthinks.com/software/avy-can-do-anything/."
      (unwind-protect
	  (save-excursion
            (goto-char pt)
            (embark-act))
	(select-window
	 (cdr (ring-ref avy-ring 0))))
      t)
    (defun avy-action-embark-dwim (pt)
      "Call `embark-dwim' on item at PT.
Based on `avy-action-embark' found at
https://karthinks.com/software/avy-can-do-anything/."
      (unwind-protect
	  (save-excursion
            (goto-char pt)
            (embark-dwim))
	(select-window
	 (cdr (ring-ref avy-ring 0))))
      t)

    (defvar avy-dispatch-alist)
    (mini-eval avy
      (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
      (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-embark-dwim))))


;;; Executable (built-in)

(mini-bltin executable
  ;; Automatically make scripts executable.
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))


;;; Files (built-in)

(mini-bltin files
  (mini-set delete-old-versions t)
  (mini-set kept-new-versions 10)
  (mini-set kept-old-versions 10)
  ;; (`kill-buffer-query-functions' is defined in C source code, so
  ;; there's no associated package, but it is related to other
  ;; functions found in the built-in files package.)  This prevents
  ;; killing the scratch buffer.
  (add-hook 'kill-buffer-query-functions 'mini-nil-if-in-scratch)
  ;; Show "^L" characters as horizontal lines in file buffers.
  ;; (These characters are used as page-breaks/section dividers in
  ;; Emacs, particularly in these configuration files.)
  (add-hook 'find-file-hook 'mini-xah-show-formfeed-as-line)
  ;; Make sure Emacs can find my user executables.
  ;; This setting plus my "~/.local/bin/firefox" script are needed so
  ;; that Emacs can open URLs in Firefox when Emacs is running inside of
  ;; a toolbx container.  That's how I usually run it on Fedora
  ;; Silverblue or Kinoite.
  (add-to-list 'exec-path "~/.local/bin")
  (when (version< "29" emacs-version)
    (mini-defk "x" 'restart-emacs mode-specific-map "Restart Emacs")))


;;; Flymake (built-in)

(mini-bltin flymake
  (unless (memq 'flycheck package-selected-packages)
    (add-hook 'prog-mode-hook 'flymake-mode)
    (mini-eval flymake
      (defvar flymake-mode-map) ;; get rid of compiler warning
      (mini-defk [?\M-g ?n]    'flymake-goto-next-error flymake-mode-map)
      (mini-defk [?\M-g ?\M-n] 'flymake-goto-next-error flymake-mode-map)
      (mini-defk [?\M-g ?p]    'flymake-goto-prev-error flymake-mode-map)
      (mini-defk [?\M-g ?\M-p] 'flymake-goto-prev-error flymake-mode-map))))


;;; Frame (built-in)

(mini-bltin frame
  (mini-set blink-cursor-blinks 1)
  ;; Disable the 'suspend-frame command when running in GUI.
  (when (display-graphic-p) (put 'suspend-frame 'disabled t)))


;;; Free-keys

(mini-pkgif free-keys
  ;; Shows keys available binding to commands.
  ;; (mini-defk ?f 'free-keys-shift mini-prefix-key-map)
  (mini-eval menu-bar
    (mini-addmenu "tools"
      '(["Free Keys (w/shift/nonshift)" free-keys-shift])
      '("Other Apps")))
  (defun free-keys-shift (&optional prefix buffer free-keys-nonshifted-keys)
    "Call the `free-keys' command with shifted/non-shifted-keys added.
On the second run, insert only non-shifted keys (or those passed
to the command).  PREFIX and BUFFER are passed to the original
command.  FREE-KEYS-NON-SHIFTED-KEYS is the list of keys used for
the second run.  If not provided, use the `mini-keys' global
variable.  (Especially useful for programmer-dvorak layout, since
it causes many of the special characters to become unshifted.)"
    (interactive (list (when current-prefix-arg
                         (read-from-minibuffer "Prefix: "))))
    (declare-function free-keys nil)
    (free-keys prefix buffer)
    (when (or free-keys-nonshifted-keys (boundp 'mini-keys))
      (let ((tempkeys (or free-keys-nonshifted-keys ;; `sort' is destructive, so copy `mini-keys' first
                          mini-keys)))
        (setq buffer-read-only nil)
        (setq prefix (or prefix ""))
        (goto-char (point-max))
        (insert "\n*** NO-SHIFT free keys ***\n
(Same as above, but non-shifted keys only.  This depends on
layout.  So if you use something like programmer-dvorak, for
example, you will have more keys listed because the symbols in
the number row are un-shifted.)\n\n")
        (let ((free-keys-keys (sort tempkeys #'<)))
          (ignore free-keys-keys)
          (defvar free-keys-modifiers)
          (mapc (lambda (m) (free-keys--process-modifier prefix m)) free-keys-modifiers))
        (insert "\n*** SHIFTED free keys ***\n(same as above, but shifted keys only)\n\n")
        (defvar free-keys-keys)
        (let ((free-keys-keys (seq-remove (lambda (x) (memq x (append tempkeys nil))) free-keys-keys)))
          (ignore free-keys-keys)
          (defvar free-keys-modifiers)
          (declare-function free-keys--process-modifier nil)
          (mapc (lambda (m) (free-keys--process-modifier prefix m)) free-keys-modifiers)))
      (setq buffer-read-only t)
      (goto-char 0))))


;;; Help (built-in)

(mini-bltin help
  ;; Using C-h for backspace, <f1> for accessing help.  So `help-char'
  ;; needs to be set in order for certain things to work.  Example:
  ;; navigation of `which-key' menus.
  (mini-set help-char 'f1)
  (mini-set describe-bindings-outline t)
  ;; Show "^L" characters as horizontal lines in help-mode buffers.
  (add-hook 'help-mode-hook 'mini-xah-show-formfeed-as-line)
  ;; Unclutter the help map.
  (when mini-unclutter-help-map
    (dolist (kb '((?g     . nil) ;; opens GNU webpage (describe-gnu-project)
                  (?\C-a  . nil) ;; (available in the Info manual) (about-emacs)
                  (?\C-c  . nil) ;; duplicate of "C-h C" (describe-copying)
                  (?d     . nil) ;; (apropos-documentation)
                  ;; (?A     . apropos-documentation) ;; Re-binding
                  (?\C-e  . nil) ;; (available in the Info manual) (view-external-packages)
                  (?h     . nil) ;; (view-hello-file)
                  (?H     . view-hello-file) ;; Re-bound to prevent accidental invocation
		  (?L     . find-library) ;; from find-func.el (original binding is available in the Info manual)
                  (?\C-o  . nil) ;; message about distribution
                  (?\C-m  . nil) ;; don't kill trees
                  (?\C-n  . nil) ;; duplicate of "C-h n"
                  (?\C-p  . nil) ;; Emacs problems
                  (?\C-s  . nil) ;; duplicative of help-for-help
                  (?\C-t  . nil) ;; the Emacs TO-DO buffer
                  (?\C-w  . nil) ;; "no warranty" pop-up
                  (?\C-\\ . nil) ;; duplicate of "C-h I"
                  (?T     . help-with-tutorial)))
      (mini-defk (car kb) (cdr kb) help-map))))


;;; Hippie-exp (built-in)

(mini-bltin hippie-exp
  (mini-defk "M-/"	'hippie-expand)) ;; to replace 'dabbrev-expand


;;; Hl-line (built-in)

(mini-bltin hl-line
  (mini-set hl-line-sticky-flag nil)
  (mini-set global-hl-line-sticky-flag nil)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'text-mode-hook 'hl-line-mode)
  (mini-eval hl-line
    (set-face-attribute 'hl-line nil :extend t)))


;;; Ibuffer (built-in)

;; Use `ibuffer' instead of `list-buffers'
(when mini-use-ibuffer-over-list-buffers
  (mini-defk "C-b" 'ibuffer ctl-x-map))


;;; Icomplete (built-in)

(mini-bltin icomplete
  (unless (cl-intersection
	   '(vertico selectrum ivy helm mct)
	   package-selected-packages)

    (unless (version< emacs-version "28")
      (add-hook 'after-init-hook 'fido-vertical-mode)

      ;; Fix wrapping of lines in minibuffer with marginalia and
      ;; icomplete-vertical-mode / fido-vertical-mode.
      (add-hook 'icomplete-minibuffer-setup-hook
		(lambda () (setq truncate-lines t)))

      ;; Make the completion list appear immediately.
      (mini-set icomplete-show-matches-on-no-input t)

      ;; Avoid annoying *Completions* buffer pop-up.
      (mini-defk "TAB" 'icomplete-force-complete
		 minibuffer-local-completion-map))))


;;; Imenu (built-in)

(mini-bltin imenu
  (unless (memq 'consult package-selected-packages)
    (mini-defk "M-i" 'imenu))
  (when mini-add-imenu-package-headings
    ;; Include page headings in Imenu.  A page heading is defined as a
    ;; line break (^J), followed by a page break (^L), followed by ";;; ".
    ;; (The ^L and ^J are converted from integers to improve readability.)
    (defun mini-add-imenu-package-headings ()
      "Add headings to `imenu-generic-expression'."
      (push
       `("Headings" ,(concat "\\(" (string 12 10) ";;; \\)\\(.*\\)$") 2)
       imenu-generic-expression))
    (add-hook 'emacs-lisp-mode-hook 'mini-add-imenu-package-headings)))


;;; Isearch (built-in)

(mini-bltin isearch
  (mini-set isearch-allow-motion t)
  (mini-set isearch-allow-scroll t)
  (mini-set isearch-lazy-count t)
  (mini-eval isearch
    (dolist (kb '(([up]    . isearch-ring-retreat)
                  ([down]  . isearch-ring-advance)
                  ([left]  . isearch-repeat-backward)
                  ([right] . isearch-repeat-forward)
		  ("C-p"   . isearch-repeat-backward)
		  ("C-n"   . isearch-repeat-forward)
		  ("C-m"   . mini-isearch-bor-exit)
		  ("<return>" . mini-isearch-eor-exit)
		  ("M-s"   . isearch-forward)
		  ("M-r"   . isearch-backward)))
      (mini-defk (car kb) (cdr kb) isearch-mode-map))
    (dolist (kb '(([left]  . isearch-reverse-exit-minibuffer)
                  ([right] . isearch-forward-exit-minibuffer)))
      (mini-defk (car kb) (cdr kb) minibuffer-local-isearch-map))
    ;; Allow exiting isearch with the binding for `capitalize-word'.
    ;; Other than yanking and completion commands, this is the only
    ;; default binding in isearch-mode-map that conflicts with an
    ;; editing command.  `isearch-toggle-case-fold-search' (the
    ;; command bound to it by default) already has a duplicate
    ;; binding at "M-s c".
    (mini-defk "M-c" nil isearch-mode-map))

  ;; Keep cursor at start of search result.
  (add-hook 'isearch-update-post-hook
            #'endless/goto-match-beginning)

  (defun endless/goto-match-beginning ()
    "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
    (when (and (not (eq this-command 'isearch-exit))
	       isearch-forward
               (number-or-marker-p isearch-other-end)
               (not mark-active)
               (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end))))

;; But I usually want the cursor to be at the start, regardless of the
;; direction of the search.  When I end it, I can use a different key
;; to move it to the end.
;; isearch-update-post-hook


;;; Kbd-mode

(mini-pkgif kbd-mode
  (add-to-list 'auto-mode-alist '("\\.kbd\\'" . kbd-mode)))


;;; Ligature

(mini-pkgif ligature
  (global-ligature-mode t)
  (mini-eval ligature
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Cascadia Code ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
					 ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
					 "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
					 "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
					 "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
					 "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
					 "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
					 "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
					 ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
					 "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
					 "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
					 "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
					 "\\\\" "://"))))


;;; Lin

(mini-pkgif lin
  (mini-eval hl-line (require 'lin))
  (mini-set lin-face 'lin-blue) ; check doc string for alternative styles
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  (mini-set lin-mode-hooks
    '(bongo-mode-hook
      dired-mode-hook
      elfeed-search-mode-hook
      git-rebase-mode-hook
      ibuffer-mode-hook
      ilist-mode-hook
      ledger-report-mode-hook
      log-view-mode-hook
      magit-log-mode-hook
      mu4e-headers-mode
      notmuch-search-mode-hook
      notmuch-tree-mode-hook
      occur-mode-hook
      org-agenda-mode-hook
      tabulated-list-mode-hook)))


;;; Link-hint

(mini-pkgif link-hint
  (mini-defk [?\M-g ?o] 'link-hint-open-link)
  ;; (mini-eval ibuffer
  ;;   (defvar ibuffer-mode-map)
  ;;   (mini-defk ?\M-g nil ibuffer-mode-map)
  ;;   (mini-defk [?\M-g ?o] 'link-hint-open-link ibuffer-mode-map))
  (mini-eval menu-bar
    (mini-addmenu "tools"
      '(["Link Hint" link-hint-open-link]))))


;;; "Lisp" (built-in)

(mini-bltin "lisp"
  (mini-set parens-require-spaces nil)
  (mini-set delete-pair-blink-delay 0.1)
  (mini-defk ?\C-\M-r 'raise-sexp)
  ;; `raise-sexp' seems more useful than `kill-backward-up-list', and I don't
  ;; want to shadow `backward-kill-sexp', as it is also useful.
  ;; Also, on Dvorak it's often easier to do "C-M-- C-M-<backspace>"
  ;; than "C-M-k".
  ;; OTOH, I like how `kill-backward-up-list' raises the whole sexp at point
  ;; and not just the sexp (or portion thereof) that follows point.
  ;;(mini-defk [(control meta backspace)] 'kill-backward-up-list)

  ;; Delete paired punctuation.  How can this be made to work within
  ;; comments?  Parentheses are highlighted, but using this command
  ;; results in a message about unbalanced parentheses.
  ;; (mini-defk "d" 'delete-pair mode-specific-map)
  ;;
  ;; insert-pair-alist default value is:
  ;; '((?\( ?\)) (?\[ ?\]) (?\{ ?\}) (?\< ?\>) (?\" ?\") (?\' ?\') (?\` ?\'))
  ;;
  ;; Because lisp.el doesn't have a provide statement, the easiest way
  ;; to add pairs to `insert-pair-alist' is to just define the whole
  ;; list, rather than using `add-to-list'.
  )


;;; Macrostep

(mini-pkgif macrostep
  (mini-defk "C-<tab>" 'macrostep-expand emacs-lisp-mode-map)
  (mini-defk "C-<tab>" 'macrostep-expand lisp-interaction-mode-map))


;;; Marginalia

(mini-pkgif marginalia
  ;; If neither Helm nor Ivy are installed.
  (unless (cl-intersection '(helm ivy) package-selected-packages)
    (add-hook 'emacs-startup-hook 'marginalia-mode)
    (if (memq 'selectrum package-selected-packages)
	(mini-eval selectrum
	  (defvar selectrum-minibuffer-map)
	  (mini-defk "M-m" 'marginalia-cycle selectrum-minibuffer-map))
      (mini-defk "M-m" 'marginalia-cycle minibuffer-local-map))
    (add-hook 'minibuffer-setup-hook 'marginalia-mode)))


;;; Markdown-mode

(mini-pkgif markdown-mode
  (mini-mode-rename 'markdown-mode " M????")) ;; placeholder


;;; Meow

(mini-pkgif meow
  
  (mini-set meow-thing-selection-directions
    '((inner	 . backward)
      (bounds	 . forward)
      (beginning . backward)
      (end	 . forward)))

  (defun meow-isearch ()
    (interactive)
    (if (meow--direction-backward-p) ; not sure how should work
	(isearch-backward t)
      (isearch-forward t))
    (when isearch-success
      (thread-first
	(meow--make-selection '(select . visit) isearch-other-end (point))
	(meow--select))))

  (defun mini-meow-set-state (mode state)
    "Set the default Meow STATE for MODE."
    (let ((statecons (assoc mode meow-mode-state-list)))
      (if statecons
	  (setcdr statecons state)
	(add-to-list 'meow-mode-state-list (cons mode state)))))

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
    ;; (meow-leader-define-key
    ;;  '("?" . meow-cheatsheet))
    (meow-motion-overwrite-define-key
     ;; custom keybinding for motion state
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("?" . meow-cheatsheet)
     '("*" . meow-expand-0)
     '("=" . meow-expand-9)
     '("!" . meow-expand-8)
     '("[" . meow-expand-7)
     '("]" . meow-expand-6)
     '("{" . meow-expand-5)
     '("+" . meow-expand-4)
     '("}" . meow-expand-3)
     '(")" . meow-expand-2)
     '("(" . meow-expand-1)
     '("1" . digit-argument)
     '("2" . digit-argument)
     '("3" . digit-argument)
     '("4" . digit-argument)
     '("5" . digit-argument)
     '("6" . digit-argument)
     '("7" . digit-argument)
     '("8" . digit-argument)
     '("9" . digit-argument)
     '("0" . digit-argument)
     '("/" . meow-isearch)
     '("-" . negative-argument)
     '(";" . meow-comment) ;meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("a" . meow-join) ;meow-append)
     ;; '("A" . meow-open-below)
     '("b" . meow-undo) ;meow-back-word)
     '("B" . meow-undo-in-selection) ;meow-back-symbol)
     '("c" . meow-next-word) ;meow-change)
     '("C" . meow-next-symbol)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-line)
     '("E" . meow-goto-line)
     '("f" . meow-find)
     '("g" . meow-back-word) ;meow-cancel-selection)
     '("G" . meow-back-symbol) ;meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-mark-word) ;meow-join)
     '("J" . meow-mark-symbol)
     '("k" . meow-kill)
     '("K" . backward-kill-word)
     '("l" . meow-till)
     '("m" . meow-change) ;meow-mark-word)
     '("M" . meow-grab) ;meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-prev)
     '("P" . meow-prev-expand)
     '("q" . meow-quit)
     '("Q" . meow-cancel-selection)
     '("r" . meow-reverse) ;meow-replace)
     ;; '("R" . meow-swap-grab)
     '("s" . avy-goto-char-2-dwim)
     '("S" . meow-search)
     '("t" . meow-right)
     '("T" . meow-right-expand)
     '("u" . meow-append) ;meow-insert) ;meow-undo)
     '("U" . meow-open-below) ;meow-open-above) ;meow-undo-in-selection)
     '("v" . meow-visit)
     '("V" . meow-swap-grab)
     '("w" . meow-save) ;meow-next-word)
     '("W" . meow-sync-grab) ;meow-next-symbol)
     '("x" . meow-backward-delete) ;meow-save)
     '("X" . delete-region) ;meow-sync-grab)
     '("y" . meow-yank)
     '("Y" . meow-replace)
     '("z" . meow-pop-selection)
     '("'" . repeat) ;meow-reverse)
     '("<escape>" . ignore)))

  (require 'meow)
  (meow-setup)
  ;; (meow-global-mode 1)
  ;; (require 'key-chord)
  ;; (key-chord-mode 1)
  ;; (key-chord-define meow-insert-state-keymap ",."
  ;; 		  'meow-normal-mode)

  (setq meow-two-char-escape-sequence ",.")
  (setq meow-two-char-escape-delay 0.5)

  (defun meow--two-char-exit-insert-state (s)
    (when (meow-insert-mode-p)
      (let ((modified (buffer-modified-p)))
	(insert (elt s 0))
	(let* ((second-char (elt s 1))
	       (event
		(if defining-kbd-macro
                    (read-event nil nil)
		  (read-event nil nil meow-two-char-escape-delay))))
          (when event
            (if (and (characterp event) (= event second-char))
		(progn
                  (backward-delete-char 1)
                  (set-buffer-modified-p modified)
                  (meow--execute-kbd-macro "<escape>"))
	      (push event unread-command-events)))))))

  (defun meow-two-char-exit-insert-state ()
    (interactive)
    (meow--two-char-exit-insert-state meow-two-char-escape-sequence))

  (define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
	      #'meow-two-char-exit-insert-state)
  
  
  (mini-eval meow
    (mini-mode-rename 'meow-normal-mode (propertize " [N]" 'face 'mode-line-emphasis) 'minor)
    (mini-mode-rename 'meow-insert-mode (propertize " [I]" 'face 'italic) 'minor)
    (mini-meow-set-state 'help-mode 'motion)
    (mini-meow-set-state 'comint-mode 'insert)
    (mini-meow-set-state 'term-mode 'insert)
    (mini-meow-set-state 'eshell-mode 'insert)
    (mini-meow-set-state 'log-edit-mode 'insert))

  ;; leader key map
  ;; Keep descriptions to 25 chars or less.
  (dolist (submap '(
		    ;; (("/" mini-snippets-map "Snippets")
		    ;;  ("a" "C-x a" "Abbrev"))
		    ;; ((")" "C-x C-k" "Keyboard Macros: C-x C-k"))
		    ;; (("a" mini-appearance-map "Appearance")
		    ;;  ("e" "C-x RET" "Encoding: C-x RET")
		    ;;  ("n" "C-x n" "Narrowing: C-x n")
		    ;;  ("p" 'pulsar-pulse-line)
		    ;;  ("t" 'consult-theme))
		    (("b" "C-x b" "Change Buffer"))
		    ;; (("b" mini-buffer-map "Buffers")
		    ;;  ("b" 'consult-buffer)
		    ;;  ("i" 'ibuffer "IBuffer")
		    ;;  ("x" "C-x x" "Buffer Cmds: C-x x"))
		    ;; (("d" mini-code/lsp-map "Code/Debugging")
		    ;;  ("a" "C-x C-a" "Edebug Pt.1: C-x C-a")
		    ;;  ("X" "C-x X" "Edebug Pt.2: C-x X"))
		    (("e" mini-leader-editing-map "Editing")
		     ;; ("c" 'cape-prefix)
		     ("e" 'mini-meow-delete-pair-of-things "Erase Pairs")
		     ("k" 'mini-kill-prefix-command "Kill Commands")
		     ("m" 'mini-mark-prefix-command "Marking Commands")
		     ("t" 'mini-transpose-prefix-command "Transpose Cmds"))
		    ;; ("d" 'delete-pair "Delete Pair")
		    ;; ("r" 'mini-delete-parentheses "Delete Parentheses")
		    ;; ("s" 'mini-delete-square-brackets "Delete Square Brackets")
		    ;; ("c" 'mini-delete-curly-brackets "Delete Curly Brackets")
		    ;; ("g" 'mini-delete-quotes "Delete Double Quotes")
		    ;; (("i" mini-leader-insert-map "Insert")
		    ;;  ("f" 'insert-file "Insert File")
		    ;;  ("$" "C-x 8" "group:C-x 8"))
		    (("h" "<f1>" "Help"))
		    (("k" "C-x k" "Kill Buffer"))
		    (("j" mini-get-around-map "Jump Around")
		     ("l" 'recenter-top-bottom "Recenter-Top-Bottom")
		     ("m" 'back-to-indentation "Back-to-Indentation")
		     ("r" 'move-to-window-line-top-bottom "Move-to-Top-Bottom"))
   		    ;; (("o" "C-x o" "Other Window"))
		    (("n" mini-notes-map "Notes & Org-Mode")
		     ("c" 'org-capture)
		     ("l" 'org-store-link)
		     ("d" 'denote)
		     ("a" 'org-agenda))
		    ;; (("o" mini-open-map "Open")
		    ;;  ("v" 'vundo))
		    (("o" "C-x o" "Other Window"))
		    ;; (("p" "C-x p" "Project Cmds: C-x p"))
		    ;; (("n" "C-x n" "group:Narrowing"))
		    (("p" "C-x p" "group:Project Cmds"))
		    (("q" "C-q" "Quoted Insert"))
		    ;; (("q" mini-quit/restart-map "Quit/Restart")
		    ;;  ("r" 'restart-emacs))
		    (("r" "C-x r" "group:Rect/Reg/Bkmrks"))
		    ;; (("r" "C-x r" "Rectangles & Reg'rs: C-x r"))
		    ;; (("s" "M-s" "Search: M-s"))
		    (("s" "C-x C-s" "Save File"))
		    (("t" "M-`" "group:TMM Menu-Bar"))
		    ;; (("t" mini-toggle/features-map "Features/Toggle")
		    ;;  ("r" "C-x C-q" "Read-Only: C-x C-q")
		    ;;  ("v" 'view-mode "View Mode"))
		    (("u" "C-u" "Universal Argument"))
		    (("v" "C-x v" "group:Version Control"))
		    (("w" mini-leader-windows-map "Windows/Tabs/Frames")
		     ("6" "C-x 6" "group:2-Columns - C-x 6")
		     ("5" "C-x 5" "group:Other Frame - C-x 5")
		     ("h" "C-x 2" "Split Horiz - C-x 2")
		     ("4" "C-x 4" "group:Other Window - C-x 4")
		     ("q" "C-x 0" "Quit Current Window")
		     ("k" "C-x 1" "Kill Other Windows")
		     ("t" "C-x t" "group:Tab-Bar - C-x t")
		     ("v" "C-x 3" "Split Vert - C-x 3")
		     ("<left>" 'winner-undo "Winner Undo")
		     ("<right>" 'winner-redo "Winner Redo"))
		    (("w m" mini-leader-windmove-map "Windmove")
		     ("h" 'windmove-left)
		     ("t" 'windmove-right)
		     ("p" 'windmove-up)
		     ("n" 'windmove-down))
		    ;; "c" as in "choose".
		    (("w c" mini-leader-windmove-display-map "Windmove-Display")
		     ("h" 'windmove-display-left)
		     ("t" 'windmove-display-right)
		     ("p" 'windmove-display-up)
		     ("n" 'windmove-display-down)
		     ("S" 'windmove-display-same-window)
		     ("T" 'windmove-display-new-tab)
		     ("F" 'windmove-display-new-frame))
		    (("w d" mini-leader-windmove-delete-map "Windmove-Delete")
		     ("h" 'windmove-delete-left)
		     ("t" 'windmove-delete-right)
		     ("p" 'windmove-delete-up)
		     ("n" 'windmove-delete-down))
		    (("w s" mini-leader-windmove-swap-map "Windmove-Swap")
		     ("h" 'windmove-swap-left)
		     ("t" 'windmove-swap-right)
		     ("p" 'windmove-swap-up)
		     ("n" 'windmove-swap-down))
		    (("y" "M-y" "Yank Pop"))))
    ;; Create prefix map and command.
    (unless (= 1 (length submap))
      ;; Make a prefix-map out of the first item in the list.
      (define-prefix-command (cadar submap)))
    ;; Bind it in
    mode-specific-map, which Meow uses by default with its leader key.
    (mini-defk (caar submap) (meow--parse-def (cadar submap)) mode-specific-map (caddar submap))
    ;; Remaining items are bound in the prefix map.
    (when (cdr submap)
      (dolist (smbinding (cdr submap)) ; smbinding = everything in an item except for the key.
	(eval `(mini-defk ,(car smbinding) (meow--parse-def ,(cadr smbinding)) ,(cadar submap) ,(caddr smbinding))))))
  (mini-defk "h" 'ehelp-command mode-specific-map "Help")


  ;; Switch to other states from keypad-state.  (Helps in buffers that
  ;; default to motion-state by providing a means to switch to
  ;; normal-state in order to select and copy text.  Insert mode
  ;; included for completeness, but not really necessary.)
  (mini-defk "N" 'meow-normal-mode mode-specific-map "Normal mode")
  (mini-defk "M" 'meow-motion-mode mode-specific-map "Motion mode")
  (mini-defk "I" 'meow-insert-mode mode-specific-map "Insert mode")

  (defun mini-meow-delete-pair-of-things (things)
    "Delete pair of chosen THINGS."
    (interactive (list
		  (let ((meow-char-thing-table
			 (seq-filter
			  (lambda (x)
			    (memq (cdr x)
				  '(round square curly string)))
			  meow-char-thing-table)))
		    (meow-thing-prompt "Delete surrounding pair: "))))
    (save-window-excursion
      (let ((back (equal 'backward (meow--thing-get-direction 'inner)))
	    (bounds (meow--parse-inner-of-thing-char things)))
	(meow--select-range back bounds)))
    (let ((deactivate-mark))
      (meow-kill)
      (backward-delete-char 1)
      (delete-char 1)
      (push-mark (point) t t)
      (yank)
      (exchange-point-and-mark)))

  ;; Enable delete-active-region, but only in insert-state.
  (add-hook 'meow-insert-enter-hook (mini-make-setter 'delete-active-region t))
  (add-hook 'meow-insert-exit-hook (mini-make-setter 'delete-active-region nil))

  ;; Enable delete-selection-mode, but only in insert-state.
  (add-hook 'meow-insert-enter-hook (mini-make-setter 'delete-selection-mode t))
  (add-hook 'meow-insert-exit-hook (mini-make-setter 'delete-selection-mode nil))

  ;; A simpler approach.  Make the most frequently used "C-x [a-z]" and
  ;; "C-[a-z]" commands available directly from the leader key.

  ;; (defun mini-meow-macro-to-leader (prestring stringlist &optional map)
  ;;   (dolist (cmdltr stringlist)
  ;;     (eval `(mini-defk
  ;; 	    ,(car cmdltr)
  ;; 	    (if ,(cdr cmdltr)
  ;; 		(cons ,(cdr cmdltr)
  ;; 		      (meow--parse-def (concat ,prestring ,(car cmdltr))))
  ;; 	      (meow--parse-def (concat ,prestring ,(car cmdltr))))
  ;; 	    ;; FIXME get this to directly look up the chosen leader key map.
  ;; 	    ,(if map map 'mode-specific-map)))))

  ;; Others you might consider but which aren't as necessary:
  ;; C-o (splits line at point, which is different from I; but "RET p e g" is good enough for me.)
  ;; C-t (but alternative is "S-h k h y", which isn't bad if you don't use it a lot... better alternative?)
  ;; C-v (but PgDn key is an alternative)

  (mini-eval ehelp
    ;; was `view-emacs-faq', removing because it blocks
    ;; `electric-describe-function' in `meow-leader-map'.
    (mini-defk "C-f" nil ehelp-map))
  (mini-eval help
    ;; was `view-emacs-faq', removing because it blocks
    ;; `describe-function' in `meow-leader-map'.
    (mini-defk "C-f" nil help-map)))


;;; Minibuffer (built-in)

;; Some bindings to use with the built-in completion system
(unless (cl-intersection '(vertico selectrum ivy helm) package-selected-packages)
  (define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  (define-key minibuffer-mode-map (kbd "C-s") 'minibuffer-next-completion)
  (define-key minibuffer-mode-map (kbd "C-r") 'minibuffer-previous-completion)
  (define-key minibuffer-mode-map (kbd "C-v") 'scroll-other-window)
  ;; need to fix below, so it doesn't move focus to *Completions* buffer window
  ;; is there a customization setting for this?
  (define-key minibuffer-mode-map (kbd "M-v") 'scroll-other-window-down)
  (define-key minibuffer-mode-map (kbd "M-<") 'beginning-of-buffer-other-window)
  (define-key minibuffer-mode-map (kbd "M->") 'end-of-buffer-other-window)
  (define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-s") 'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-r") 'minibuffer-previous-completion))


;;; Minimap

(mini-pkgif minimap
  (mini-defk "<f9>" 'minimap-mode)
  (mini-set minimap-window-location 'right)
  (mini-mode-rename 'minimap-mode "" 'minor))


;;; Misc (built-in)

(mini-bltin misc
  (when (display-graphic-p)
    (mini-defk "C-z"	'zap-up-to-char));; replace 'suspend-frame in GUI

  ;; These bindings prevent use of shift-selection with word
  ;; movement.  But they make it easy to move precisely where you
  ;; want.
  (when mini-to-word-bindings
    (mini-defk "M-B" 'backward-to-word)
    (mini-defk "M-F" 'forward-to-word)))


;;; Modus-themes

(mini-pkgif modus-themes
  (mini-set modus-themes-bold-constructs t)
  (mini-set modus-themes-box-buttons '(flat accented))
  (mini-set modus-themes-completions
    '((matches nil background intense)
      (selection nil text-also accented intense)))
  (mini-set modus-themes-inhibit-reload nil)
  (mini-set modus-themes-intense-hl-line t)
  (mini-set modus-themes-intense-mouseovers t)
  (mini-set modus-themes-italic-constructs t)
  (mini-set modus-themes-mixed-fonts t)
  (mini-set modus-themes-mode-line '(nil accented borderless 2))
  (mini-set modus-themes-org-blocks 'gray-background)
  (mini-set modus-themes-paren-match '(bold intense))
  (mini-set modus-themes-region '(accented))
  (mini-set modus-themes-scale-headings t)
  (mini-set modus-themes-subtle-line-numbers t)
  (mini-set modus-themes-tabs-accented t)
  (mini-set modus-themes-variable-pitch-headings t)
  (mini-set modus-themes-variable-pitch-ui t)
  (mini-set modus-themes-vivendi-color-overrides '((bg-active . "black")))
  ;; If `modus-themes-variable-pitch-ui' is enabled, space the tab-bar
  ;; tabs a bit further apart.
  (when (and (boundp 'modus-themes-variable-pitch-ui)
	     modus-themes-variable-pitch-ui)
    (customize-set-variable 'tab-bar-separator "    "))
  (mini-set custom-enabled-themes '(modus-vivendi)))


;;; Mu4e
;; This can only be installed via the operating system.

;; :system-deps ("mu" "mbsync") ;; packages are "maildir-utils" and "isync"
(mini-eval menu-bar
  (when (locate-library "mu4e")
    (mini-addmenu "tools"
      '(["Mu4e: Email client" mu4e])
      '("Other Apps" "Communication")))
  ;; (mini-set mu4e-get-mail-command "mbsync -c ~/.config/mbsync/mbsyncrc gmail")
  (autoload 'mu4e "mu4e" "Mu4e" 'interactive)

  (mini-eval mu4e
    (mini-set mu4e-headers-draft-mark     '("D" . "????"))
    (mini-set mu4e-headers-flagged-mark   '("F" . "????"))
    (mini-set mu4e-headers-new-mark       '("N" . "????"))
    (mini-set mu4e-headers-passed-mark    '("P" . "???"))
    (mini-set mu4e-headers-replied-mark   '("R" . "???"))
    (mini-set mu4e-headers-seen-mark      '("S" . "???"))
    (mini-set mu4e-headers-trashed-mark   '("T" . "????"))
    (mini-set mu4e-headers-attach-mark    '("a" . "????"))
    (mini-set mu4e-headers-encrypted-mark '("x" . "????"))
    (mini-set mu4e-headers-signed-mark    '("s" . "????"))
    (mini-set mu4e-headers-unread-mark    '("u" . "???"))
    (mini-set mu4e-headers-list-mark      '("s" . "????"))
    (mini-set mu4e-headers-personal-mark  '("p" . "????"))
    (mini-set mu4e-headers-calendar-mark  '("c" . "????")))) ;; This may need to be customized.


;;; Mwim

(mini-pkgif mwim
  (mini-defk "C-a" 'mwim-beginning)
  (mini-defk "C-e" 'mwim-end))

;; Configure the movements by customizing the variables
;; `mwim-beginning-position-functions' and
;; `mwim-end-position-functions'.


;;; Newcomment (built-in)

(mini-bltin newcomment
  (mini-defk "M-;" 'comment-line)) ; to replace 'comment-dwim


;;; Newsticker (built-in)

(mini-set newsticker-url-list
  '(("Free Software Foundation Europe" "https://fsfe.org/news/news.it.rss")
    ("Free Software Foundation USA" "https://static.fsf.org/fsforg/rss/blogs.xml")
    ("Guix system" "https://www.gnu.org/software/guix/feeds/blog.atom")
    ("Emacs Blog" "http://emacsblog.org/feed/")
    ("Howardism - Howard Abrams blog" "http://howardism.org/index.xml")
    ("Endless parentheses" "http://endlessparentheses.com/atom.xml")
    ("Mastering Emacs" "https://www.masteringemacs.org/feed")
    ("Scripter" "https://scripter.co/posts/atom.xml")
    ("One Of Us" "https://oneofus.la/have-emacs-will-hack/feed.xml")
    ("Org-mode upcoming changes" "https://updates.orgmode.org/feed/changes")
    ("Reddit - Emacs" "https://www.reddit.com/r/emacs.rss")
    ("Reddit - Org-mode" "https://www.reddit.com/r/orgmode.rss")
    ("Reddit - Stallman Was Right" "https://www.reddit.com/r/StallmanWasRight.rss")
    ("XKCd" "https://xkcd.com/atom.xml")))


;;; Orderless

(mini-pkgif orderless
  (mini-set completion-styles '(orderless basic))
  (mini-set completion-category-defaults nil)
  (mini-set completion-category-overrides
    '((file (styles partial-completion)))))


;;; Org (built-in)

(mini-bltin org
  ;; To get latest version of org, use mini-ensure instead.
  ;; needed for export to pdf: wrapfig.sty ulem.sty capt-of.sty
  ;; :system-deps ("/usr/share/texlive/texmf-dist/tex/generic/ulem/ulem.sty"      ;; texlive-capt-of
  ;;               "/usr/share/texlive/texmf-dist/tex/latex/capt-of/capt-of.sty"  ;; texlive-ulem
  ;;               "/usr/share/texlive/texmf-dist/tex/latex/wrapfig/wrapfig.sty)" ;; texlive-wrapfig
  ;; (mini-defk ?a 'org-agenda     mode-specific-map)
  ;; (mini-defk ?c 'org-capture    mode-specific-map)
  ;; (mini-defk ?l 'org-store-link mode-specific-map)
  
  (mini-eval org-agenda
    (mini-set org-agenda-block-separator ????)
    (mini-set org-agenda-current-time-string
      "??? now ???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????")
    (mini-set org-agenda-time-grid
      '((daily today require-timed)
	(800 1000 1200 1400 1600 1800 2000)
	" ??????????????? " "?????????????????????????????????????????????")))
  (mini-set org-auto-align-tags nil)
  (mini-set org-catch-invisible-edits 'show-and-error)
  (mini-set
      org-confirm-babel-evaluate
    (lambda (lang body)
      (ignore body)
      (not
       (member lang '("python"))))
    "List of languages for org-src blocks that don't require
confirmation to evaluate.")
  (mini-set org-directory "~/org")
  (mini-set org-ellipsis "???")
  (mini-set org-enforce-todo-checkbox-dependencies t)
  (mini-set org-enforce-todo-dependencies t)
  (mini-set org-hide-emphasis-markers t)
  (mini-set org-hide-leading-stars t)
  (mini-set org-insert-heading-respect-content t)
  (mini-set org-pretty-entities t)
  (mini-set org-special-ctrl-a/e t)
  (mini-set org-startup-indented t)
  (mini-set org-support-shift-select t)
  (mini-set org-tags-column 0)
  (mini-set org-todo-keywords
    '((sequence "TODO(t)" "PROJECT(p)" "WAIT(w)" "IDEA(i)" "|" "DONE(d)" "KILLED(k)")
      (sequence "CALL(c)" "EMAIL(e)" "MSG(m)" "|" "GHOSTED(g)")))
  (mini-set org-use-speed-commands t)
  (declare-function which-key-add-major-mode-key-based-replacements nil)
  (mini-eval (org which-key)
    (which-key-add-major-mode-key-based-replacements 'org-mode
      "C-c \"" "plot prefix"
      "C-c C-v" "org-babel prefix"
      "C-c C-x" "extra prefix"))
  (mini-eval org
    ;; It's built-in, but it depends on org.
    (require 'org-mouse)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (perl . t)
       (python . t))))

  (add-hook 'org-mode-hook 'auto-fill-mode))


;;; Org-modern

(mini-pkgif org-modern
  (add-hook 'org-mode-hook 'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook 'org-modern-agenda)
  (mini-set org-modern-hide-stars nil))


;;; Paragraphs (built-in)

(mini-bltin paragraphs
  (mini-defk ?R 'repunctuate-sentences search-map))


;;; Paren (built-in)

(mini-bltin paren
  (add-hook 'prog-mode-hook 'show-paren-mode))


;;; Pdf-tools

(mini-pkgif pdf-tools
  ;; :system-deps ("make"                         ;; make
  ;;               "automake"                     ;; automake
  ;;               "autoconf"                     ;; autoconf
  ;;               "gcc"                          ;; gcc
  ;;               "g++"                          ;; gcc-c++
  ;;               "libpng-config"                ;; libpng-devel
  ;;               "/usr/include/zlib.h"          ;; zlib-devel
  ;;               "/usr/include/poppler/Annot.h" ;; poppler-devel
  ;;               "/usr/include/poppler/glib"    ;; poppler-glib-devel
  ;;               "mogrify")                   ;; ImageMagick
  (declare-function pdf-loader-install nil)
  (pdf-loader-install))

;; Needs make automake autoconf gcc gcc-c++ libpng-devel zlib-devel poppler


;;; Pixel-scroll (built-in)

(mini-bltin pixel-scroll
  (when (version< "29" emacs-version)
    (declare-function pixel-scroll-precision-mode nil)
    (pixel-scroll-precision-mode)))


;;; Pulsar

(mini-pkgif pulsar
  (mini-eval menu-bar
    (mini-addmenu "options"
      '(["Pulsar (cursor pulse on big moves)" pulsar-mode])))
  (add-hook 'after-init-hook 'pulsar-global-mode)
  (mini-set pulsar-face 'pulsar-generic)
  (mini-set pulsar-iterations 20)
  (mini-set pulsar-delay 0.03))


;;; Python (built-in)

(mini-bltin python
  ;; Show problematic whitespace in Python code.
  (add-hook 'python-mode-hook
	    (lambda ()
	      (setq-local whitespace-line-column nil)
	      (setq-local fill-column 79)
	      ;; (require 'whitespace)
	      ;; (setq-local whitespace-style
	      ;; 	'( face trailing lines-tail empty big-indent indentation::tab
	      ;; 	   space-after-tab::tab space-before-tab::tab ))
	      (setq-local tab-width 4)
	      ;; (whitespace-mode)
	      ))
  (add-hook 'python-mode-hook 'subword-mode)
  (mini-eval python
    (mini-set python-shell-completion-native-disabled-interpreters
      '("jupyter" "pypy")) ; try to autoload python
    (mini-set python-shell-interpreter "jupyter")
    (mini-set python-shell-interpreter-args "console --simple-prompt")
    (mini-set python-shell-prompt-detect-failure-warning nil)
    (mini-eval which-key
      (declare-function which-key-add-keymap-based-replacements nil)
      (defvar python-mode-map)
      (which-key-add-keymap-based-replacements
	python-mode-map
	"C-c TAB" "Python Imports"
	"C-c C-t" "Python Skeletons"))))


;;; Recentf (built-in)

(mini-bltin recentf
  (run-at-time 1.5 nil 'recentf-mode))


;;; Repeat (built-in)

;; Enable repeat-mode, but don't echo the message about how many
;; commands it's enabled for, since it's distracting and quickly
;; becomes no-longer accurate.
(mini-bltin repeat
  (when (version< "28" emacs-version)
    (run-at-time
     2 nil
     (lambda ()
       (let ((inhibit-message t))
	 (repeat-mode))))))


;;; Savehist (built-in)

(mini-bltin savehist
  (add-hook 'minibuffer-mode-hook 'savehist-mode))


;;; Saveplace (built-in)

(mini-bltin saveplace
  (add-hook 'after-init-hook 'save-place-mode))


;;; Sdcv-Mode
;; from https://github.com/gucong/emacs-sdcv
;; as recommended at http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
;; for use with Webster's Revised Unabridged Dictionary (1913), as described here:
;; http://jsomers.net/blog/dictionary
;; (Requires the "sdcv" command-line tool, found in most Linux distro repositories.)

(add-to-list 'load-path (expand-file-name "lisp/emacs-sdcv" user-emacs-directory))
(autoload 'sdcv-search "sdcv-mode"
  "Search WORD through the command-line tool sdcv." 'interactive)
(mini-defk "s" 'sdcv-search mode-specific-map "Search Dictionary")


;;; Sh-mode (built-in)

(mini-bltin sh-mode
  (mini-set sh-basic-offset 2)
  (mini-set sh-indentation 2))


;;; Simple (built-in)

(mini-bltin simple
  (mini-set completion-show-help nil)
  (mini-set kill-whole-line t)
  (mini-set set-mark-command-repeat-pop t)
  (when (version< emacs-version "29")
    ;; IDEA Create an option in the sentends package to
    ;; make cycle-spacing follow its lead.  You could
    ;; either have it insert 2 spaces after the end of a
    ;; sentence, instead of just one, or add another step
    ;; so that it cycles from 1 to none to 2, or some
    ;; other arrangement, or add the additional step, but
    ;; change the order depending on whether a sentence
    ;; ending was detected.
    (mini-defk "M-SPC"	'cycle-spacing)) ;; to replace 'just-one-space
  (mini-defk "M-u"	'upcase-dwim)
  (mini-defk "M-c"	'capitalize-dwim)
  (mini-defk "M-l"	'downcase-dwim)
  (mini-defk "C-d"      'delete-forward-char) ;; replacement for 'delete-char, deletes region if active.
  (add-hook 'emacs-startup-hook 'turn-on-auto-fill))


;;; Smtpmail (built-in)

(mini-bltin smtpmail

  (mini-eval smtpmail
    (mini-set send-mail-function 'smtpmail-send-it))
  (mini-set smtpmail-stream-type 'ssl))


;;; Sly

(mini-pkgif sly
  (autoload 'sly "sly" nil t))


;;; Speedbar (built-in)

(add-hook 'speedbar-mode-hook
	  (lambda ()
	    (setq-local cursor-in-non-selected-windows nil)))


;;; Sr-speedbar
;; Turn Speedbar into a sidebar window in the same frame.

(mini-pkgif sr-speedbar
  (mini-defk "<f8>" 'sr-speedbar-toggle))


;;; Startup (built-in)

(mini-bltin startup
  ;; Insert startup time at bottom of splash screen.
  (unless (version< emacs-version "28")
    (advice-add
     'fancy-startup-tail
     :after
     (lambda (&optional concise)
       (ignore concise)
       (fancy-splash-insert
	:face 'variable-pitch
	(emacs-init-time "\n\nEmacs started in %.2f seconds."))))))


;;; Subword (built-in)

(mini-bltin subword
  (autoload 'subword-mark "subword" "Do the same as `mark-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `mark-word'." t))


;;; Tab-bar (built-in)

(mini-bltin tab-bar
  ;; (add-hook 'after-init-hook 'tab-bar-mode)
  (mini-set tab-bar-format
    '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
  (mini-set tab-bar-select-tab-modifiers '(meta))
  (mini-set tab-bar-tab-hints t)
  (mini-set tab-bar-close-button-show nil)
  ;; Name tabs automatically based on the current file's project, if
  ;; it has one.
  (defvar mini-projects-directory "~/Projects/")
  (autoload 'project-root "project")
  (defun mini-tab-bar-tab-name-function ()
    (let* ((project (project-current))
	   (projdir (when project
		      (replace-regexp-in-string "/$" "" (project-root project)))))
      ;; Is current file part of a project?
      (if project
	  ;; Check if project is in the "~/Projects" directory.
	  (if (string-prefix-p mini-projects-directory projdir)
	      ;; (and (string= "~" (car projdirsplit))
	      ;; 	   (string= "Projects" (cadr projdirsplit)))
	      ;; If so, use only the project root directory's name.
	      (concat "P: " (file-name-nondirectory projdir))
	    ;; Otherwise, use full path of project root directory's name.
	    (concat "P: " projdir))
	;; Use default naming if not in a project.
	(tab-bar-tab-name-current))))
  (mini-set tab-bar-tab-name-function 'mini-tab-bar-tab-name-function))


;;; Tempel

(mini-pkgif tempel
  (mini-defk "M-+" 'tempel-complete) ;; Alternative tempel-expand
  (mini-defk "M-*" 'tempel-insert)
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons 'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )


;;; Term (built-in)

(mini-bltin term
  (defun mini-termsh ()
    "Invoke `term' without having to press Enter first."
    (interactive)
    (term "/bin/bash")))


;;; Time (built-in)

(mini-bltin time
  (mini-set display-time-default-load-average nil)
  (mini-set display-time-format "%l:%M%#p ")
  (mini-set display-time-mode t))


;;; Vertico

(mini-pkgif vertico
  (add-hook 'after-init-hook 'vertico-mode)
  (mini-set vertico-cycle t)
  (defun mini-tmm-with-vertico ()
    "A wrapper around `tmm-menubar' to ensure all items are shown."
    (interactive)
    (let ((vertico-count 40)
	  (vertico-count-format nil))
      (call-interactively 'tmm-menubar)))

  (mini-defk "M-`" 'mini-tmm-with-vertico)
  (mini-defk [f10] 'mini-tmm-with-vertico)

  (mini-eval vertico
    (defvar vertico-map)
    ;; Use M-s and M-r to navigate between results (esp. for consult-line)
    (mini-defk "M-s" 'vertico-next         vertico-map)
    (mini-defk "M-r" 'vertico-previous     vertico-map)
    (mini-defk "M-e" 'vertico-quick-exit   vertico-map)
    (mini-defk "M-i" 'vertico-quick-insert vertico-map)

    ;; Avoid duplicate menu when using `tmm-menubar'.
    (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)

    ;; Avoid duplicate completion ui with the `ffap-menu' command.
    (declare-function ffap-menu-ask "ffap")
    (advice-add #'ffap-menu-ask
		:around
		(lambda (&rest args)
		  (cl-letf
		      (((symbol-function
			 #'minibuffer-completion-help)
			#'ignore))
		    (apply args))))))


;;; View (built-in)

(mini-bltin view
  (unless (memq 'vundo package-selected-packages)
    (mini-defk ?v 'view-mode mode-specific-map)))


;;; Vundo

(mini-pkgif vundo
  (mini-eval menu-bar
    (mini-addmenu "tools"
      '(["Visualize Undos" vundo]))))


;;; Warnings (built-in)

(mini-bltin warnings
  (mini-set warning-minimum-level :error))


;;; Which-func (built-in)

(mini-bltin which-func
  (add-hook 'prog-mode-hook 'which-function-mode)
  (add-hook 'org-mode-hook 'which-function-mode))


;;; Which-key

(mini-pkgif which-key
  (mini-eval menu-bar
    ;; TODO How to add checkbox in regular menu-bar for this?
    (mini-addmenu "options"
      '(["Which-Key (pop-up keybindings)" which-key-mode])
      '("Show/Hide")))
  (add-hook 'window-setup-hook
            'which-key-setup-side-window-right-bottom)
  (mini-eval which-key
    (declare-function which-key-mode nil)
    (which-key-mode)
    (dolist (kb '((?t . which-key-show-top-level)
                  (?M . which-key-show-major-mode)))
      (mini-defk (car kb) (cdr kb) help-map))
    (defvar which-key-replacement-alist)
    (push (cons '(nil . "\\([[:alnum:]- ]+\\)mode$")
		(lambda (kb)
                  (cons (car kb)
			(let* ((kbstr (cdr kb))
                               (cmdsym (intern kbstr))
                               (vblsym (cond
					;; mode commands where a different symbol's variable value determines status:
					((eq cmdsym 'read-only-mode) 'buffer-read-only)
					;; Meow-mode state when shown in keypad-state leader map:
					;; (keypad-state is temporary and switches back to previous state on exit)
					((bound-and-true-p meow-keypad-mode)
					 (cond
					  ((eq cmdsym 'Normal\ mode)
					   '(eq meow--keypad-previous-state 'normal))
					  ((eq cmdsym 'Motion\ mode)
					   '(eq meow--keypad-previous-state 'motion))
					  ((eq cmdsym 'Insert\ mode)
					   '(eq meow--keypad-previous-state 'insert))
					  ((boundp cmdsym) cmdsym)))
					;; Meow-mode state when accessing mode-specific-map directly:
					((eq cmdsym 'Normal\ mode)
					 'meow-normal-mode)
					((eq cmdsym 'Motion\ mode)
					 'meow-motion-mode)
					((eq cmdsym 'Insert\ mode)
					 'meow-insert-mode)
					;; most other mode commands
					((boundp cmdsym) cmdsym))))
                          ;; TODO Fix checkbox for edebug
                          ;; modes ("C-x X" and "C-x C-a")?
                          ;; These several modes set the
                          ;; value of a couple variables to
                          ;; indicate which one is active or
                          ;; will be active.
                          (cond
                           ;; commands ending in "mode" that aren't modes.
                           ((member kbstr '("which-key-show-major-mode" "describe-mode" "electric-describe-mode")) kbstr)
                           (t (if (eval vblsym)
                                  (concat "[X] " kbstr)
				(concat "[ ] " kbstr))))))))
          which-key-replacement-alist)
    ;; If "C-c [letter]" is simulating "C-c C-[letter]",
    ;; don't clutter the which-key window with all of the
    ;; "C-c [letter]" bindings.
    (when mini-C-c+letter-as-C-c+C-letter
      (defvar which-key--ignore-non-evil-keys-regexp)
      (set 'which-key--ignore-non-evil-keys-regexp
           (concat "\\(?:C-c "
                   (if mini-C-c+g+char-as-C-c+C-char
                       "[A-Za-fh-z]"
                     "[A-Za-z]")
                   "$\\|drag-\\|mouse-\\|remap\\|s\\(?:croll-bar\\|elect-window\\|witch-frame\\)\\|wh\\(?:eel-\\|ich-key\\)\\)")))
    ;; (let ((orig '("mouse-" "wheel-" "remap" "drag-" "scroll-bar"
    ;;                    "select-window" "switch-frame" "which-key"))
    ;;            (letters (mapcar 'string mini-letters)))
    ;;   (dolist (ltr letters)
    ;;          (unless (string= ltr "g")
    ;;            (push (concat "C-c " ltr) orig)))
    ;;   (regexp-opt orig))
    (which-key-add-key-based-replacements
      "C-c" "mode-specific-map"
      "C-c g" "Ctrl"
      "C-x" "ctl-x-map"
      "C-x RET" "encoding"
      "C-x 4" "other-window"
      "C-x 5" "other-frame"
      "C-x 6" "2-columns"
      "C-x 8" "characters"
      "C-x 8 e" "emojis"
      "C-x X" "edebug"
      "C-x a" "abbrev"
      "C-x a i" "inverse-abbrev"
      "C-x g" "Ctrl"
      "C-x n" "narrow"
      "C-x p" "project"
      "C-x r" "rectangles/registers"
      "C-x t" "tab-bar"
      "C-x v" "version-control"
      "C-x v M" "mergebase"
      "C-x x" "buffer"
      "C-x C-a" "edebug (more?)"
      "M-g" "goto-map"
      "M-s" "search-map"
      "M-s h" "highlighting"
      "<menu>" "user-bindings"
      "<menu> h 4" "other-window-help"
      "C-h 4" "other-window-help"
      "<f1> 4" "other-window-help"
      "<help> 4" "other-window-help")
    (which-key-add-keymap-based-replacements
      ctl-x-map
      ;; "c" "mode-specific-map"
      ;; "c @" "outline-map"
      ;; "x" "ctl-x-map"
      "RET" "encoding"
      "4" "other-window"
      "5" "other-frame"
      "6" "2-columns"
      "8" "characters"
      "8 e" "emojis"
      "X" "edebug"
      "a" "abbrev"
      "a i" "inverse-abbrev"
      "g" "Ctrl"
      "n" "narrow"
      "p" "project"
      "r" "rectangles/registers"
      "t" "tab-bar"
      "t ^" "detach"
      "v" "version-control"
      "v M" "mergebase"
      "v b" "branches"
      "w" "window"
      "w ^" "detach"
      "x" "buffer"
      ;; "C-a" "edebug (more?)"
      ;; "m g" "goto-map"
      ;; "m s" "search-map"
      ;; "m s h" "highlighting"
      ;; "h 4" "other-window-help"
      )))


;;; Winner (built-in)

(mini-bltin winner
  ;; This automatically binds "C-c <left>" to `winner-undo' and
  ;; "C-c <right>" to `winner-redo'.  To prevent that, you can set
  ;; `winner-dont-bind-my-keys' to a non-nil value beforehand.
  (add-hook 'window-setup-hook 'winner-mode))

;;; Window (built-in)

(mini-bltin window
  (mini-defk "M-o" 'other-window)
  (mini-defk "<insert>" 'other-window))


;;; Yasnippet

(mini-pkgif yasnippet
  (add-hook 'after-init-hook 'yas-global-mode)
  (mini-mode-rename 'yas-minor-mode " ????" 'minor)
  ;; (setcdr (assoc 'yas-minor-mode minor-mode-alist) '(" ????" nil))
  (mini-eval yasnippet
    (mini-eval which-key
      (which-key-add-keymap-based-replacements
	yas-minor-mode-map
	"C-c &" "YASnippet"))))

(provide 'mini-packages)
;;; mini-packages.el ends here
