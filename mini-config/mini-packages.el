;;; mini-packages.el --- shorter configurations  -*- lexical-binding: t; -*-

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

;; Configuration is divided organized by the relevant packages.

;;; Code:

(require 'mini-core)


;;; Abbrev
;; built-in

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


;;; Ace-isearch

(mini-pkgif ace-isearch
  (mini-set ace-isearch-function 'avy-goto-char)
  (mini-set ace-isearch-2-function 'avy-goto-char-2)
  (mini-set ace-isearch-jump-based-on-one-char nil)
  (declare-function global-ace-isearch-mode nil)
  (global-ace-isearch-mode))


;;; Ace-link

(mini-pkgif ace-link
  ;; This is most useful for non-tabular modes, in which links are
  ;; interspersed within paragraphs or other non-symmetrical
  ;; constructs.
  (dolist (hook '(Info-mode-hook
                  help-mode-hook
                  package-menu-mode-hook
                  geiser-doc-mode-hook
                  elbank-report-mode-hook
                  elbank-overview-mode-hook
                  slime-trace-dialog-mode-hook
                  helpful-mode-hook
                  Man-mode-hook
                  woman-mode-hook
                  eww-mode-hook
                  w3m-mode-hook
                  compilation-mode-hook
                  grep-mode-hook
                  compilation-shell-minor-mode-hook
                  gnus-article-mode-hook
                  gnus-summary-mode-hook
                  mu4e-view-mode-hook
                  notmuch-show-mode-hook
                  erc-mode-hook
                  elfeed-show-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  eshell-mode-hook
                  telega-chat-mode-hook
                  org-mode-hook
                  org-agenda-mode-hook
                  Custom-mode-hook
                  sldb-mode-hook
                  slime-xref-mode-hook
                  slime-inspector-mode-hook
                  indium-inspector-mode-hook
                  indium-debugger-frames-mode-hook
                  magit-commit-mode-hook
                  cider-inspector-mode-hook))
    (add-hook hook 'ace-link-setup-default))
  ;; Ace-link with Org
  (mini-eval org
    (defvar org-mode-map)
    (mini-defk [?\M-g ?o] 'ace-link-org org-mode-map)
    (mini-defk [?\M-g ?\C-o] 'ace-link-org org-mode-map))
  ;; (defvar org-speed-commands))
  ;; The default org-speed-command bound to "o" is
  ;; `org-open-at-point'.  But speed-commands can only be used
  ;; when point is at the start of a heading.  So that means it's
  ;; only doing the same thing that TAB would do at that spot.
  ;; Better to replace it with `ace-link-org'.
  ;;(setcdr (assoc "o" org-speed-commands) 'ace-link-org))
  ;;
  ;; Ace-link with Avy
  (mini-eval avy
    (defvar avy-styles-alist)
    (add-to-list 'avy-styles-alist
		 '(mini-ace-link-dashboard . post)))
  ;; Ace-link with Dashboard
  (mini-set dashboard-footer-messages '("Press 'o' or 'v' to select an item."))
  (mini-eval dashboard
    (require 'ace-link)
    (defvar dashboard-mode-map)
    (mini-defk [home] 'mini-ace-link-dashboard dashboard-mode-map)
    (mini-defk ?o 'mini-ace-link-dashboard dashboard-mode-map)
    (mini-defk ?v 'mini-ace-link-dashboard dashboard-mode-map)
    (mini-defk ?\C-o 'mini-ace-link-dashboard dashboard-mode-map)
    (mini-defk ?\s-v 'mini-ace-link-dashboard dashboard-mode-map)
    (defun mini-ace-link--dashboard-collect (offset)
      (let ((end (window-end))
            points)
	(save-excursion
	  (goto-char (window-start))
	  (declare-function widget-forward nil)
	  (when (ignore-errors (widget-forward 1) t)
            (push (- (point) offset) points)
            (widget-forward 1)
            (while (and (< (point) end)
			(> (point) (car points)))
              (push (- (point) offset) points)
              (widget-forward 1))
            (nreverse points)))))
    (defun mini-ace-link--dashboard-action (pt offset)
      (when (numberp pt)
	(goto-char (+ pt offset))
	(declare-function dashboard-return nil)
	(dashboard-return)))
    (defun mini-ace-link-dashboard ()
      "Open a visible link in a `dashboard' buffer."
      (interactive)
      (let ((offset 1)) ;; set offset to 1, -1 or -2.
	(defvar mini-ace-link-dashboard)
	(declare-function mini-ace-link--dashboard-collect nil)
	(declare-function mini-ace-link-dashboard nil)
	(declare-function avy-with nil)
	(let ((pt (avy-with mini-ace-link-dashboard
                    (defvar avy-style)
		    (declare-function avy-process nil)
                    (declare-function avy--style-fn nil)
                    (declare-function mini-ace-link--dashboard-action nil)
                    (avy-process
                     (mini-ace-link--dashboard-collect offset)
                     (avy--style-fn avy-style)))))
	  (mini-ace-link--dashboard-action pt offset))))))


;;; Ace-window

(mini-pkgif ace-window
  ;; TODO Add a warning about not using the same modifiers for
  ;; `mini-aw-defk' as any of the keys listed in `aw-keys' or
  ;; `aw-dispatch-alist'.  This includes Shift when used with
  ;; letters!  (But it doesn't include shift when used with
  ;; symbols or punctuation) See end of block...
  ;; (defvar mini-aw-defk (vector mini-prefix-key ?w)) ;; This way, we can analyze the key later.  See end of  block...
  (mini-defk "w a" 'ace-window mode-specific-map)
  (mini-defk ?\s-w 'mini-ace-window-always-dispatch)
  ;; (mini-defk ?w 'mini-ace-window-always-dispatch mini-prefix-key-map)
  (mini-set aw-keys (append (mini-keyvector (number-sequence 27 34)) nil))
  (mini-set aw-dispatch-alist
    ;; TODO These are for Dvorak.  Need to write a function as
    ;; described in the 'avy config block and use it here too.
    '((?k aw-delete-window "Delete Window")  ;; prefer to always delete current window.
      (?s aw-swap-window "Swap Windows")
      (?y aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?b aw-switch-buffer-in-window "Select Buffer")
      (?l aw-flip-window "Last window")
      (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?q aw-split-window-fair "Split Fair Window")
      (?w aw-split-window-vert "Split Vert Window")
      (?v aw-split-window-horz "Split Horz Window")
      (?m delete-other-windows "Delete Other Windows") ;; prefer to always leave current window remaining.
      (?x aw-transpose-frame "Transpose Frame") ;; depends on `transpose-frame' package.
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")
  ;; (defun mini-ace-window-always-dispatch (arg)
  ;;   "Call `ace-window' and dispatch, regardless of settings."
  ;;   (interactive "p")
  ;;   (require 'ace-window)
  ;;   (let ((aw-dispatch-always t))
  ;;     (ignore aw-dispatch-always)
  ;;     (ace-window arg)))
  (mini-set aw-dispatch-always t)
  (mini-set aw-dispatch-when-more-than 2)
  (mini-set aw-fair-aspect-ratio 3)
  (mini-set aw-minibuffer-flag t)
  ;; (mini-set aw-translate-char-function
  ;;   ;; Make `ace-window' work even if you don't release the
  ;;   ;; modifier key(s) after invoking it.
  ;;   ;;
  ;;   ;; (This means we can't use any keys in `aw-keys' or
  ;;   ;; `aw-dispatch-alist' that have the same modifiers as
  ;;   ;; `mini-aw-defk', since they'd always get translated to their
  ;;   ;; base character.  But it is hard to think of a situation
  ;;   ;; where one would want to do that.  Even using a shifted
  ;;   ;; letter in `aw-keys' or `aw-dispatch-alist', you would not
  ;;   ;; also set `mini-aw-defk' to any letters modified *only* by
  ;;   ;; Shift, because that goes in the global-map where it's
  ;;   ;; needed for `self-insert-command'.  So this could only be
  ;;   ;; an issue with say `C-S-o'/`C-O' as `mini-aw-defk' and
  ;;   ;; something like `C-S-k'/`C-K' in one of the key lists.  But
  ;;   ;; there are enough unmodified keys available for the key
  ;;   ;; lists that you're not likely to use any more than the
  ;;   ;; Shift modifier in either of them.)
  ;;   (lambda (event)
  ;;     (if (and (event-modifiers event)
  ;;              (equal (event-modifiers mini-aw-defk) (event-modifiers event))
  ;;              ;; Exempt C-g from this, so
  ;;              ;; you can still cancel the
  ;;              ;; command.
  ;;              (not (eq event 7)))
  ;; 	  (event-basic-type event)
  ;; 	event)))
  )


;;; Align
;; built-in

(mini-bltin align
    (mini-set align-to-tab-stop nil))


;;; All-the-icons

(mini-pkgif all-the-icons
  (when (display-graphic-p)
    (declare-function all-the-icons-install-fonts nil)
    (unless (file-exists-p
             "~/.local/share/fonts/all-the-icons.ttf")
      (all-the-icons-install-fonts t))
    (require 'all-the-icons)
    (mini-ensure all-the-icons-completion
      (declare-function all-the-icons-completion-mode nil)
      (if (package-installed-p 'marginalia)
	  (add-hook 'marginalia-mode-hook 'all-the-icons-completion-marginalia-setup)
	(dolist (pkg '(vertico mct selectrum ivy helm))
	  (with-eval-after-load pkg 'all-the-icons-completion-mode))))
    (mini-ensure all-the-icons-dired
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
    (mini-ensure all-the-icons-ibuffer
      (add-hook 'ibuffer-mode-hook 'all-the-icons-ibuffer-mode))))


;;; Anaconda-mode

(mini-pkgif anaconda-mode
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (mini-eval anaconda-mode
    (require 'pyvenv)))


;;; Auctex

(mini-pkgif auctex
  (mini-set TeX-auto-save t)
  (mini-set TeX-parse-self t)
  (defvar TeX-macro-global)
  (mini-eval auctex
    (add-to-list 'TeX-macro-global "~/.config/tex/")))


;;; Autoinsert
;; built-in

(mini-bltin autoinsert
  (add-hook 'find-file-hook 'auto-insert-mode))


;;; Autorevert
;; built-in

(mini-bltin autorevert
  (run-at-time 4 nil 'global-auto-revert-mode))


;;; Avy

(mini-pkgif avy
  (mini-set avy-indent-line-overlay nil)
  (mini-set avy-column-line-overlay t)
  (mini-set avy-orders-alist
    '((avy-goto-char	     . avy-order-closest)
      (avy-goto-char-2	     . avy-order-closest)
      (avy-goto-word-0	     . avy-order-closest)
      (avy-goto-word-0-above . avy-order-closest)
      (avy-goto-word-0-below . avy-order-closest)
      (avy-isearch	     . avy-order-closest)
      (avy-goto-line-above   . avy-order-closest)))
  (mini-defk ?\C-,          'avy-goto-char-in-line)
  (mini-defk ?\M-p          'avy-goto-word-0-above)
  (autoload 'avy-goto-word-0-above "avy")
  (mini-defk ?\M-n          'avy-goto-word-0-below)
  (autoload 'avy-goto-word-0-below "avy")

  ;; mode-specific-map "C-c"
  ;; These commands are useless until some other command has been
  ;; called.  Autoloading them anyway, so it'll be clear that the keys
  ;; are already in use.
  ;; (mini-defk ?p             'avy-prev             mode-specific-map)
  ;; (autoload 'avy-prev "avy" "Go to the previous candidate of the last ‘avy-read’." t)
  ;; (mini-defk ?n             'avy-next             mode-specific-map)
  ;; (autoload 'avy-next "avy" "Go to the next candidate of the last ‘avy-read’." t)
  (mini-defk ?r             'avy-resume           mode-specific-map)
  (autoload 'avy-resume "avy" nil t)

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

  ;; Recommended bindings from https://github.com/abo-abo/avy
  (mini-defk "C-'" 'avy-goto-char)
  (mini-defk "C-\"" 'avy-goto-char-2)
  ;; But the above conflicts with `org-cycle-agenda-files' in org-mode buffers.
  ;; So...
  (defvar org-mode-map)
  (mini-eval org
    (mini-defk "C-'" nil org-mode-map))

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

  (mini-set avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))


;;; Calfw

(mini-pkgif calfw
  (autoload 'cfw:open-calendar-buffer "calfw")
  (mini-addmenu "tools"
    '(["CalFW (full-size calendar)" cfw:open-calendar-buffer])
    '("Other Apps*" "Organizing")))


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
    (mini-defk (car pair) (cdr pair) mode-specific-map))

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


;;; Checkdoc
;; built-in

(mini-bltin checkdoc
  ;; Too many false positives, as the original function
  ;; assumed that every period was a sentence ending.
  (mini-eval checkdoc
    (defun checkdoc-sentencespace-region-engine (begin end)
      (ignore begin end))))


;;; Company

(mini-pkgif company
  (add-hook 'after-init-hook 'global-company-mode))


;;; Company-anaconda

(mini-pkgif company-anaconda
  (mini-eval company
    (defvar company-backends)
    (add-to-list 'company-backends 'company-anaconda)))


;;; Consult

(mini-pkgif consult

  ;; Use `consult-completion-in-region' if Vertico is enabled and
  ;; neither Company nor Corfu are installed.  Otherwise use the default
  ;; `completion--in-region' function.
  (defvar vertico-mode)
  (declare-function consult-completion-in-region "consult")
  (unless (or (package-installed-p 'company)
	      (package-installed-p 'corfu))
    (mini-set completion-in-region-function
      (lambda (&rest args)
	(apply (if vertico-mode
                   #'consult-completion-in-region
		 #'completion--in-region)
               args))))

  (defvar mini-consult-prefix-map (make-sparse-keymap))
  (define-prefix-command 'mini-consult-prefix-command 'mini-consult-prefix-map)
  (mini-defk [?u] 'mini-consult-prefix-command mode-specific-map)

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
  (mini-set corfu-auto t)
  (mini-set corfu-auto-delay 0.4)
  (mini-set corfu-auto-prefix 4)
  (run-at-time 1 nil
	       (lambda () (add-hook 'window-state-change-hook 'corfu-mode))))


;;; Cperl-mode
;; built-in

(mini-bltin cperl-mode
  (defvar cperl-mode-map)
  (add-to-list 'auto-mode-alist "\\.\\([pP][Llm]\\|al\\)\\'" 'cperl-mode)
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
      "use warnings;\nuse strict;\n"))
  (mini-eval cperl-mode
    (mini-defk [?\C-c ?\C-c] 'quickrun-shell cperl-mode-map)))


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
  (mini-addmenu "options"
    '(["Darkroom-Mode" darkroom-mode])))


;;; Dashboard

(mini-pkgif dashboard
  ;; (mini-defk ?e 'dashboard-return dashboard-mode-map) ;; make `e' work as in 'dired, 'ibuffer, etc.
  ;; (mini-defk ?p 'dashboard-previous-line dashboard-mode-map)
  ;; (mini-defk ?n 'dashboard-next-line dashboard-mode-map)
  (mini-set dashboard-agenda-prefix-format "%s ")
  (mini-set dashboard-center-content t)
  (mini-set dashboard-item-shortcuts nil)
  (mini-set dashboard-set-file-icons t)
  (mini-set dashboard-set-heading-icons t)
  (mini-set dashboard-startup-banner 'logo)
  (mini-set dashboard-week-agenda nil)
  (mini-set dashboard-items '((agenda . 10) (bookmarks . 10) (recents . 10)))
  (declare-function dashboard-setup-startup-hook nil)
  (dashboard-setup-startup-hook)
  (mini-eval dashboard
    (add-hook 'dashboard-mode-hook (lambda () (setq-local cursor-type nil)))
    ;; Modus themes change the color setting for the widget-items
    ;; face which dashboard-items-face relies on.  But widget-items
    ;; normally inherits straight from the bold face.  So problem is
    ;; solved by having dashboard-items inherit from the bold face.
    (when (memq (car custom-enabled-themes) '(modus-operandi modus-vivendi))
      (face-spec-set 'dashboard-items-face '((t (:inherit bold)))))))


;;; Delsel
;; built-in

(mini-bltin delsel
  (run-at-time 3 nil 'delete-selection-mode))


;;; Dired
;; built-in

(mini-bltin dired
  (mini-eval which-key
    (mini-eval dired
      (defvar dired-mode-map)
      (mini-defk [remap dired-summary]
		 'which-key-show-major-mode dired-mode-map))))


;;; Dired-aux
;; built-in

(mini-bltin dired-aux
  (mini-eval dired
    (require 'dired-aux)))


;;; Dired-subtree

(mini-pkgif dired-subtree
  (mini-eval dired
    (defvar dired-mode-map)
    (mini-defk ?\t 'dired-subtree-toggle dired-mode-map)))


;;; Dired-x
;; built-in

(mini-bltin dired-x
  (mini-eval dired
    (require 'dired-x)))


;;; Display-line-numbers
;; built-in

(mini-bltin display-line-numbers
  ;; Show line-numbers in programming modes.
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;;; Docker

(mini-pkgif docker
  ;; Not compatible with podman yet.  (It was as of version 1.3.0,
  ;; but there were unanticipated complications, and it was
  ;; removed in version 1.4.0.  Hopefully someone will solve those
  ;; problems eventually?)
  (mini-defk "k" 'docker))


;;; Docker-tramp

(mini-pkgif docker-tramp
  (mini-set docker-tramp-docker-executable "podman"))


;;; Doom-modeline

(mini-pkgif doom-modeline
  (declare-function doom-modeline-mode nil)
  (doom-modeline-mode 1))


;;; Doom-themes

(mini-pkgif doom-themes
  nil) ;; placeholder


;;; Dumb-jump

(mini-pkgif dumb-jump
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate)
  (mini-set xref-show-definitions-function
    'xref-show-definitions-completing-read))


;;; Eglot

(mini-pkgif eglot
  ;; Eglot needs the most recent versions of these built-in packages.
  (mini-ensure (eldoc xref project))
  (add-hook 'python-mode-hook 'eglot-ensure)
  ;; "jdtls" is already in 'eglot-server-programs.
  ;; =You just have to make sure that it's in your path.=
  (add-hook 'java-mode-hook 'eglot-ensure)
  ;; Add jedi-language-server to the list of auto-detected Python LSPs.
  (mini-eval eglot
    (defvar eglot-server-programs)
    (declare-function eglot-alternatives nil)
    (setcdr
     (assq 'python-mode eglot-server-programs)
     (eglot-alternatives
      '("jedi-language-server" "pylsp" "pyls" ("pyright-langserver" "--stdio"))))))


;; Ehelp
;; built-in

(mini-bltin ehelp
  (autoload		'ehelp-command "ehelp"
    "Prefix command (definition is a keymap associating keystrokes with commands)."
    'interactive 'keymap)
  (mini-defk "C-h"	'ehelp-command)  ;; electric-help
  (mini-defk "<f1>"	'ehelp-command)  ;; electric-help
  (mini-defk "<help>"	'ehelp-command)) ;; electric-help


;;; Eldoc
;; built-in

(mini-bltin eldoc
  (add-hook 'prog-mode-hook 'eldoc-mode))


;;; Elec-pair
;; built-in

(mini-bltin elec-pair
  (run-at-time 1 nil
	       (lambda ()
		 (add-hook 'window-state-change-hook 'electric-pair-mode))))


;;; Electric
;; built-in

(mini-bltin electric
  (add-hook 'text-mode-hook 'electric-quote-local-mode)
  ;; (mini-eval org
  ;;   (add-hook 'org-mode-hook 'mini-electric-quote-inhibit-add))
  )


;;; Elpy, Exec-path-from-shell, and Py-autopep8

(mini-pkgif elpy
  ;; Inherit shell settings properly.  (Elpy needs this.)
  (mini-ensure exec-path-from-shell)
  (add-hook 'python-mode-hook 'elpy-enable)
  (mini-pkgif py-autopep8
    (add-hook 'elpy-mode-hook 'py-autopep8-mode))
  (add-hook 'elpy-mode-hook 'subword-mode)
  (mini-set elpy-modules
    '(elpy-module-company
      elpy-module-eldoc
      elpy-module-pyvenv
      elpy-module-highlight-indentation
      elpy-module-yasnippet
      elpy-module-django
      elpy-module-sane-defaults))

  (mini-pkgif exec-path-from-shell
    (when (or (memq window-system '(mac ns x))
	      (daemonp))
      (declare-function exec-path-from-shell-initialize nil)
      (exec-path-from-shell-initialize))))


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


;;; Expand-region

(mini-pkgif expand-region
  (autoload 'er/mark-text-sentence "expand-region")
  (mini-defk ?\C-= 'er/expand-region nil "expand-region")
  (mini-defk ?\M-h 'er/mark-text-sentence ctl-x-map "mark-sentence"))


;;; Executable
;; built-in

(mini-bltin executable
  ;; Automatically make scripts executable.
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))


;;; Files
;; built-in

(mini-bltin files
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
  (mini-defk "x"      'restart-emacs            mode-specific-map)
  (mini-addmenu "file"
    '(["Restart Emacs" restart-emacs]))))


;;; Flycheck

(mini-pkgif flycheck
  (add-hook 'prog-mode-hook 'global-flycheck-mode)
  (mini-eval which-key
    (declare-function which-key-add-key-based-replacements nil)
    (which-key-add-key-based-replacements
     "C-c !" "flycheck-prefix")))


;;; Flymake
;; built-in

(mini-bltin flymake
  (unless (package-installed-p 'flycheck)
    (add-hook 'prog-mode-hook 'flymake-mode)
    (mini-eval flymake
      (defvar flymake-mode-map) ;; get rid of compiler warning
      (mini-defk [?\M-g ?n]    'flymake-goto-next-error flymake-mode-map)
      (mini-defk [?\M-g ?\M-n] 'flymake-goto-next-error flymake-mode-map)
      (mini-defk [?\M-g ?p]    'flymake-goto-prev-error flymake-mode-map)
      (mini-defk [?\M-g ?\M-p] 'flymake-goto-prev-error flymake-mode-map))))


;;; Frame
;; built-in

(mini-bltin frame
  ;; Disable the 'suspend-frame command when running in GUI.
  (when (display-graphic-p) (put 'suspend-frame 'disabled t)))


;;; Free-keys

(mini-pkgif free-keys
  ;; Shows keys available binding to commands.
  ;; (mini-defk ?f 'free-keys-shift mini-prefix-key-map)
  (mini-addmenu "tools"
    '(["Free Keys (w/shift/nonshift)" free-keys-shift])
    '("Other Apps"))
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


;;; Geiser

(mini-pkgif geiser
  (mini-set geiser-active-implementations '(guile)))


;;; Geiser-guile

(mini-pkgif geiser-guile
  (mini-eval geiser-guile
    (unless (executable-find "guile")
      (mini-set geiser-guile-binary "guile3.0"))))


;;; God-mode

(mini-pkgif god-mode
  (mini-set god-mode-enable-function-key-translation nil)
  (mini-set god-exempt-major-modes nil)
  (mini-set god-exempt-predicates nil)
  ;; potentials: i, q, j, x, SPC, g, c, m, h, r, RET
  ;; "SPC" conflicts with `god-literal-key', but I'm using "<menu>" for prefixes, so I don't need it.
  ;; For the same reason, I also don't need to reserve "x", "c", or "h".
  ;; It also conflicts with "C-SPC", but I can use the "@" symbol, which doesn't even have to be
  ;; shifted in programmer-dvorak.
  (mini-set god-mode-alist '((nil . "C-")
                             ;; ("j" . "M-")
                             ("m" . "M-")
                             ("j" . "C-M-")
                             ("c" . "C-M-")))
                                        ;(mini-set god-literal-key "SPC")
  (declare-function god-local-mode nil)
  (defvar god-local-mode-map)
  (defun mini-god-mode-start ()
    (interactive)
    (god-local-mode 1))
  (defun mini-god-mode-stop ()
    (interactive)
    (god-local-mode -1))
  ;; Bind to foot pedal press and release, respectively.
  (mini-defk [Launch6] 'mini-god-mode-start) ;; this is f15
  (mini-defk [Launch7] 'mini-god-mode-stop god-local-mode-map)) ;; this is f16


;;; Gumshoe

(mini-pkgif gumshoe
  (add-hook 'after-init-hook 'global-gumshoe-mode)
  (mini-defk "g b" 'gumshoe-backtrack-back    mode-specific-map)
  (mini-defk "g f" 'gumshoe-backtrack-forward mode-specific-map)
  (mini-eval repeaters
    (declare-function repeaters-define-maps nil)
    (repeaters-define-maps
     '(("gumshoe-backtracking"
	gumshoe-backtrack-back    "b"
	gumshoe-backtrack-forward "f")))))


;;; Help
;; built-in

(mini-bltin help
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
                  (?\C-f  . find-library) ;; from find-func.el (original binding is available in the Info manual)
                  (?h     . nil) ;; (view-hello-file)
                  (?H     . view-hello-file) ;; Re-bound to prevent accidental invocation
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


;;; Helpful

(mini-pkgif helpful
  ;; Put the helpful commands a little further down, since the
  ;; electric-help commands are usually good enough and less
  ;; obtrusive.
  (defvar helpful-help-map)
  (define-prefix-command 'helpful-help-map)
  (mini-defk "h"      'helpful-help-map          mode-specific-map)

  ;; helpful-help-map (C-h h, <f1> h, <help> h)
  (mini-defk "f"      'helpful-callable         helpful-help-map)
  (mini-defk "v"      'helpful-variable         helpful-help-map)
  (mini-defk "k"      'helpful-key              helpful-help-map)
  (mini-defk "F"      'helpful-function         helpful-help-map)
  (mini-defk "c"      'helpful-command          helpful-help-map)
  (mini-defk "C"      'helpful-command          helpful-help-map)
  (mini-defk "m"      'helpful-macro            helpful-help-map)
  (mini-defk "s"      'helpful-symbol           helpful-help-map)
  (mini-defk "p"      'helpful-at-point         helpful-help-map)

  ;; Make org-link to help use helpful.el instead of help.el
  (autoload 'helpful-callable "helpful")
  (autoload 'helpful-variable "helpful")
  (defun mini-org-link-helpful (orig &rest args)
    "Alternative way to open `org-mode' help: links.
If the `helpful' package is installed, use `helpful-callable' and
`helpful-variable', instead of `describe-function' and
`describe-variable', respectively.  Otherwise, no change.  Takes
ORIG and ARGS as arguments."
    (load "helpful" t)
    (cl-letf (((symbol-function 'describe-function) 'helpful-callable)
              ((symbol-function 'describe-variable) 'helpful-variable))
      (apply orig args))
    (apply orig args))

  (advice-add 'org-link--open-help :around 'mini-org-link-helpful))


;;; Highlight-indent-guides

(mini-pkgif highlight-indent-guides
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (mini-set highlight-indent-guides-method 'column))


;;; Highlight-indentation

(mini-pkgif highlight-indentation
  (add-hook 'python-mode-hook 'highlight-indentation-mode))


;;; Hippie-exp

(mini-bltin hippie-exp
  (mini-defk "M-/"	'hippie-expand)) ;; to replace 'dabbrev-expand


;;; Hl-line
;; built-in

(mini-bltin hl-line
  (mini-set hl-line-sticky-flag nil)
  (mini-set global-hl-line-sticky-flag nil)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'text-mode-hook 'hl-line-mode)
  (mini-eval hl-line
    (set-face-attribute 'hl-line nil :extend t)))


;;; Hl-todo

(mini-pkgif hl-todo
  (dolist (hk '(prog-mode-hook
                text-mode-hook))
    (add-hook hk 'hl-todo-mode))
  (mini-eval hl-todo
    (defvar hl-todo-keyword-faces)
    (add-to-list 'hl-todo-keyword-faces '("IDEA" . "#d0bf8f"))))


;;; Hyperbole

(mini-pkgif hyperbole
  (add-hook 'after-init-hook 'hyperbole-mode)
  (mini-eval avy
    (defvar avy-dispatch-alist)
    (add-to-list 'avy-dispatch-alist '(?: . (lambda (pt)
					      (goto-char pt)
					      (hkey-either))))))


;;; Ibuffer

;; Use `ibuffer' instead of `list-buffers'
(when mini-use-ibuffer-over-list-buffers
  (mini-defk [remap list-buffers] 'ibuffer))


;;; Icomplete / Fido-vertical
;; built-in

(mini-bltin icomplete
  (unless (or (package-installed-p 'vertico)
	      (package-installed-p 'selectrum)
	      (package-installed-p 'ivy)
	      (package-installed-p 'helm)
	      (package-installed-p 'mct))

    (unless (version< emacs-version "28")
      (add-hook 'after-init-hook 'fido-vertical-mode)

      ;; Fix wrapping of lines in minibuffer with marginalia and
      ;; icomplete-vertical-mode / fido-vertical-mode.
      (add-hook 'icomplete-minibuffer-setup-hook
		(lambda () (setq truncate-lines t)))

      ;; Make the completion list appear immediately.
      (mini-set icomplete-show-matches-on-no-input t)

      ;; Avoid annoying *Completions* buffer pop-up.
      (mini-defk "TAB" 'icomplete-force-complete minibuffer-local-completion-map))))


;;; Imenu
;; built-in

(mini-bltin imenu
  (unless (package-installed-p 'consult)
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


;;; Isearch
;;built-in

(mini-bltin isearch
  ;; Allow exiting isearch with the binding for `capitalize-word'.
  ;; Other than yanking and completion commands, this is the only
  ;; default binding in isearch-mode-map that conflicts with an
  ;; editing command.  `isearch-toggle-case-fold-search' (the
  ;; command bound to it by default) already has a duplicate
  ;; binding at "M-s c".
  (mini-defk "M-c" nil isearch-mode-map)
  (mini-set isearch-allow-motion t)
  (mini-set isearch-allow-scroll t)
  (mini-set isearch-lazy-count t)

  (mini-eval isearch
    (mini-defk "M-m" 'mini-isearch-bor-exit isearch-mode-map))
  (dolist (kb '(([up]    . isearch-ring-retreat)
                ([down]  . isearch-ring-advance)
                ([left]  . isearch-repeat-backward)
                ([right] . isearch-repeat-forward)))
    (mini-defk (car kb) (cdr kb) isearch-mode-map))
  (dolist (kb '(([left]  . isearch-reverse-exit-minibuffer)
                ([right] . isearch-forward-exit-minibuffer)))
    (mini-defk (car kb) (cdr kb) minibuffer-local-isearch-map)))


;;; Jit-lock
;; built-in

;; (mini-bltin jit-lock
;;   (mini-set jit-lock-stealth-time 1)
;;   (mini-set jit-lock-defer-time 1)
;;   (mini-set jit-lock-stealth-load 200))


;;; Jump-char

(mini-pkgif jump-char
  (mini-defk "M-m" 'jump-char-forward)
  (mini-defk "M-M" 'jump-char-backward)
  (mini-eval jump-char
    (defvar jump-char-base-map)
    (mini-defk "C-m" 'jump-char-exit jump-char-base-map)))


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
  (mini-eval ibuffer
    (defvar ibuffer-mode-map)
    (mini-defk ?\M-g nil ibuffer-mode-map)
    (mini-defk [?\M-g ?o] 'link-hint-open-link ibuffer-mode-map))
  ;; (mini-defk "C-c l c" 'link-hint-copy-link)
  )


;;; "Lisp"
;; built-in

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
  (mini-defk "d" 'delete-pair mode-specific-map))


;;; Logos

(mini-pkgif logos
  (mini-set logos-outlines-are-pages t)
  (defvar logos--page-delimiter)
  (mini-eval outline
    (mini-set logos-outline-regexp-alist
      `((emacs-lisp-mode . "^;;;+ ")
	(org-mode . "^\\*+ +")
	(markdown-mode . "^\\#+ +")
	(t . ,(or outline-regexp logos--page-delimiter)))))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (mini-set logos-hide-mode-line t)
  (mini-set logos-hide-buffer-boundaries t)
  (mini-set logos-hide-fringe t)
  (mini-set logos-variable-pitch nil)
  (mini-set logos-buffer-read-only nil)
  (mini-set logos-scroll-lock nil)
  (mini-set logos-olivetti nil)

  ;; Also check this manual for `logos-focus-mode-extra-functions'.  It is
  ;; a hook that lets you extend `logos-focus-mode'.

  (mini-defk [remap narrow-to-region] 'logos-narrow-dwim)
  (mini-defk [remap forward-page] 'logos-forward-page-dwim)
  (mini-defk [remap backward-page] 'logos-backward-page-dwim)
  (mini-defk "<f8>" 'logos-focus-mode))

;; Also consider adding keys to `logos-focus-mode-map'.  They will take
;; effect when `logos-focus-mode' is enabled.


;;; Lsp-bridge

(mini-pkgif lsp-bridge
  ;; Doesn't work.
  ;; :install-file "lsp-bridge"
  (add-to-list 'load-path (expand-file-name "lisp/lsp-bridge" user-emacs-directory))
  (add-to-list 'exec-path (expand-file-name ".cache/lsp/eclipse.jdt.ls/bin" user-emacs-directory))
  (mini-pkgif yasnippet
    (require 'yasnippet)
    (declare-function yas-global-mode nil)
    (yas-global-mode 1))
  (declare-function global-lsp-bridge-mode nil)
  (global-lsp-bridge-mode))


;;; Lsp-java

(mini-pkgif lsp-java
  (when (package-installed-p 'lsp-mode)
    (mini-set lsp-java-configuration-runtimes
      '[(:name "OpenJDK-1.8"
	       :path "/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.332.b09-1.fc36.x86_64/jre/")
	(:name "OpenJDK-11"
	       :path "/usr/lib/jvm/java-11-openjdk-11.0.15.0.10-1.fc36.x86_64/"
	       :default t)
	(:name "OpenJDK-17"
               :path "/usr/lib/jvm/java-17-openjdk-17.0.3.0.7-1.fc36.x86_64/")])
    (add-hook 'java-mode-hook 'lsp-deferred)))


;;; Lsp-mode

(mini-pkgif lsp-mode
  (dolist (hook '(c-mode c++-mode))
    (add-hook (intern (concat (symbol-name hook) "-hook")) 'lsp-deferred))
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
  (mini-eval lsp-mode
    (dolist (pkg4lsp '(lsp-completion lsp-diagnostics lsp-modeline))
      (require pkg4lsp))))


;;; Macrostep

(mini-pkgif macrostep
  (mini-defk "C-<tab>" 'macrostep-expand emacs-lisp-mode-map)
  (mini-defk "C-<tab>" 'macrostep-expand lisp-interaction-mode-map))


;;; Macrostep-geiser

(mini-pkgif macrostep-geiser
  nil) ;; placeholder

;;; Magit

(mini-pkgif magit
  ;; :system-deps "git"
  (mini-eval magit
    (mini-set magit-define-global-key-bindings nil))
  (mini-defk "v S" 'magit-status ctl-x-map)
  (mini-defk "M-g" 'magit-dispatch ctl-x-map)
  (mini-defk "M-g" 'magit-file-dispatch mode-specific-map))


;;; Marginalia

(mini-pkgif marginalia
  (unless (seq-filter 'package-installed-p '(helm ivy))
    (add-hook 'emacs-startup-hook 'marginalia-mode)
    (if (package-installed-p 'selectrum)
	(mini-eval selectrum
	  (defvar selectrum-minibuffer-map)
	  (mini-defk "M-m" 'marginalia-cycle selectrum-minibuffer-map))
      (mini-defk "M-m" 'marginalia-cycle minibuffer-local-map))
    (add-hook 'minibuffer-setup-hook 'marginalia-mode)
    (mini-eval marginalia
      (mini-defk "M-m" 'marginalia-cycle minibuffer-local-map))))


;;; Markdown-mode

(mini-pkgif markdown-mode
  nil) ;; placeholder


;;; Mct

(mini-pkgif mct
  (add-hook 'minibuffer-setup-hook 'mct-minibuffer-mode)
  (add-hook 'completion-in-region-mode-hook 'mct-region-mode))


;;; Minimap

(mini-pkgif minimap
  (mini-defk "<f9>" 'minimap-mode)
  (mini-set minimap-window-location 'right)
  (mini-set minimap-mode nil))


;;; Misc
;; built-in

(mini-bltin misc
  (when (display-graphic-p)
    (mini-defk "C-z"	'zap-up-to-char));; replace 'suspend-frame in GUI

  ;; These bindings prevent use of shift-selection with word
  ;; movement.  But they make it easy to move precisely where you
  ;; want.
  (when mini-to-word-bindings
    (mini-defk "M-B" 'backward-to-word)
    (mini-defk "M-F" 'forward-to-word)))


;;; Mixed-pitch

(mini-pkgif mixed-pitch
  (add-hook 'text-mode-hook 'mixed-pitch-mode))


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


;;; Mood-line

(mini-pkgif mood-line
  (add-hook 'after-init-hook 'mood-line-mode)
  (add-hook 'vc-mode-line-hook 'mood-line--update-vc-segment))


;;; Moody

(mini-pkgif moody
  (mini-set x-underline-at-descent-line t)
  (declare-function moody-replace-mode-line-buffer-identification nil)
  (declare-function moody-replace-vc-mode nil)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


;;; Mu4e

(mini-pkgif mu4e
  ;; :system-deps ("mu" "mbsync") ;; packages are "maildir-utils" and "isync"
  (mini-addmenu "tools"
    '(["Mu4e: Email client" mu4e])
    '("Other Apps*" "Communication"))
  (mini-set mu4e-get-mail-command "mbsync -c ~/.config/mbsync/mbsyncrc -a")
  (mini-set mu4e-headers-unread-mark    '("u" . "📩 "))
  (mini-set mu4e-headers-draft-mark     '("D" . "🚧 "))
  (mini-set mu4e-headers-flagged-mark   '("F" . "🚩 "))
  (mini-set mu4e-headers-new-mark       '("N" . "✨ "))
  (mini-set mu4e-headers-passed-mark    '("P" . "↪ "))
  (mini-set mu4e-headers-replied-mark   '("R" . "↩ "))
  (mini-set mu4e-headers-seen-mark      '("S" . " "))
  (mini-set mu4e-headers-trashed-mark   '("T" . "🗑️"))
  (mini-set mu4e-headers-attach-mark    '("a" . "📎 "))
  (mini-set mu4e-headers-encrypted-mark '("x" . "🔑 "))
  (mini-set mu4e-headers-signed-mark    '("s" . "🖊 "))) ;; This may need to be customized.


;;; Mu4e-alert

(mini-pkgif mu4e-alert
  ;;:system-deps "notify-send"
  ;;(run-at-time 6 nil 'mu4e-alert-enable-mode-line-display)
  (declare-function mu4e-alert-set-default-style nil)
  (mu4e-alert-set-default-style 'libnotify)
  (declare-function mu4e-alert-enable-notifications nil)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  ;; (mini-set mu4e-alert-interesting-mail-query
  ;;   "flag:unread maildir:/gmail/inbox")
  (declare-function mu4e-alert-enable-mode-line-display nil)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))


;;; Mu4e-marker-icons

(mini-pkgif mu4e-marker-icons
  ;; mini-eval (mu4e all-the-icons)
  (declare-function mu4e-marker-icons-mode nil)
  (mu4e-marker-icons-mode 1))


;;; Mwim

(mini-pkgif mwim
  (mini-defk "C-a" 'mwim-beginning)
  (mini-defk "C-e" 'mwim-end))

;; I'll use C-e more, because that finger is stronger.  I don't want
;; to press C-a very much because my pinky gets sore easily.
;; Configure the movements by customizing the variables
;; `mwim-beginning-position-functions' and
;; `mwim-end-position-functions'.


;;; Newcomment
;; built-in

(mini-bltin newcomment
  (mini-defk "M-;" 'comment-line)) ; to replace 'comment-dwim


;;; Nov

(mini-pkgif nov
  ;; Currently, this is the only entry-point (opening a file with
  ;; the ".epub" extension).  This can be done from `dired' of
  ;; course, but...  IDEA It would be nice to have an interface
  ;; for browsing ebooks, kind of like Calibre.  This may already
  ;; exist, as there are already some apps which work with Calibre
  ;; databases.  Such a package might also open pds with
  ;; `pdf-tools'.
  (add-to-list 'auto-mode-alist "\\.epub\\'" 'nov-mode))


;;; Olivetti

(mini-pkgif olivetti
  (mini-addmenu "options"
    '(["Olivetti Mode" olivetti-mode])))


;;; Orderless

(mini-pkgif orderless
  (mini-set completion-styles '(orderless basic))
  (mini-set completion-category-defaults nil)
  (mini-set completion-category-overrides
    '((file (styles partial-completion)))))


;;; Org
;; built-in

(mini-bltin org
  ;; To get latest version of org, use mini-ensure instead.
  ;; needed for export to pdf: wrapfig.sty ulem.sty capt-of.sty
  ;; :system-deps ("/usr/share/texlive/texmf-dist/tex/generic/ulem/ulem.sty"      ;; texlive-capt-of
  ;;               "/usr/share/texlive/texmf-dist/tex/latex/capt-of/capt-of.sty") ;; texlive-ulem
  (mini-defk ?a 'org-agenda     mode-specific-map)
  (mini-defk ?c 'org-capture    mode-specific-map)
  (mini-defk ?l 'org-store-link mode-specific-map)
  (mini-eval org-agenda
    (mini-set org-agenda-block-separator ?─)
    (mini-set org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")
    (mini-set org-agenda-time-grid
      '((daily today require-timed)
	(800 1000 1200 1400 1600 1800 2000)
	" ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")))
  (mini-set org-auto-align-tags nil)
  (mini-set org-catch-invisible-edits 'show-and-error)
  (mini-set org-directory "~/org")
  (mini-set org-ellipsis "↲")
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
       (python . t)))))


;;; Org-contacts

(mini-pkgif org-contacts
  (require 'org-contacts)
  (defvar org-directory)
  (mini-set org-contacts-files (list (expand-file-name "contacts.org" org-directory)))
  (defvar org-capture-templates)
  (defvar mini-pkg-org-contacts-template
    "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:" "Template for org-contacts.")
  (mini-eval org-contacts
    (add-to-list 'org-capture-templates
		 `(("c" "Contact" entry (file+headline "~/org/contacts.org" "Friends"),
		    mini-pkg-org-contacts-template
		    :empty-lines 1)))))


;;; Org-contrib

(mini-pkgif org-contrib
  nil) ;; placeholder


;;; Org-modern

(mini-pkgif org-modern
  (add-hook 'org-mode-hook 'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook 'org-modern-agenda)
  (mini-set org-modern-hide-stars nil))


;;; Paragraphs
;; built-in

(mini-bltin paragraphs
  (mini-defk ?R 'repunctuate-sentences search-map))


;;; Paren
;; built-in

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


;;; Persistent-scratch

(mini-pkgif persistent-scratch
  (add-hook 'after-init-hook
            'persistent-scratch-setup-default))


;;; Pixel-scroll
;; built-in

(mini-bltin pixel-scroll
  (when (version< "29" emacs-version)
    (pixel-scroll-precision-mode)))


;;; Prog-mode
;; built-in

(mini-bltin prog-mode
  (mini-eval prog-mode
    (global-prettify-symbols-mode)))


;;; Pulsar

(mini-pkgif pulsar
  (mini-addmenu "options"
    '(["Pulsar* (cursor pulse on big moves)" pulsar-mode]))
  (add-hook 'after-init-hook 'pulsar-global-mode)
  (mini-set pulsar-face 'pulsar-generic)
  (mini-set pulsar-iterations 20)
  (mini-set pulsar-delay 0.03)
  (mini-defk [?s] 'pulsar-pulse-line mode-specific-map))


;;; Python
;; built-in

(mini-bltin python
  ;; Show problematic whitespace in Python code.
  (add-hook 'python-mode-hook
	    (lambda ()
	      (setq-local whitespace-line-column nil)
	      (setq-local fill-column 79)
	      (setq-local whitespace-style
		'(face trailing lines-tail empty indentation::tab big-indent
		       space-after-tab::tab space-before-tab::tab))
	      (setq-local tab-width 4)
	      (whitespace-mode)))
  (mini-eval python
    (mini-set python-shell-completion-native-disabled-interpreters
      '("jupyter" "pypy")) ; try to autoload python
    (mini-set python-shell-interpreter "jupyter")
    (mini-set python-shell-interpreter-args "console --simple-prompt")
    (mini-set python-shell-prompt-detect-failure-warning nil)))


;;; Python-black

(mini-pkgif python-black
  (mini-eval python
    (defvar python-mode-map)
    (mini-defk "C-c p b" 'python-black-buffer python-mode-map)
    ;; (add-hook 'python-mode-hook 'python-black-on-save-mode-enable-dwim)
    ))


;;; Pyvenv

(mini-pkgif pyvenv
  (mini-eval pyvenv
    (declare-function pyvenv-mode nil)
    ;; (pyvenv-activate "~/pyvenv-test/")
    (pyvenv-mode 1)))


;;; Quickrun

(mini-pkgif quickrun
  (mini-eval python
    (defvar python-mode-map) ;; Get flycheck to shut up.
    (mini-defk [?\C-c ?\C-c] 'quickrun-shell python-mode-map)))


;;; Recentf
;; built-in

(mini-bltin recentf
  (mini-set recentf-mode t))


;;; Repeat
;; built-in
 
(mini-bltin repeat
  (when (version< "28" emacs-version)
    (add-hook 'after-init-hook 'repeat-mode)))


;;; Restart-emacs

(mini-pkgif restart-emacs
  ;; Emacs 29 provides a built-in command of the same name.
  (mini-defk ?C 'restart-emacs ctl-x-map)
  (mini-addmenu "file"
    '(["Restart Emacs" restart-emacs])))


;;; Rg

(mini-pkgif rg
  ;; :system-deps "rg" ;; ripgrep
  ;; (mini-set rg-executable "toolbox -c ripgrep run rg")
  (mini-addmenu "tools"
    '(["Ripgrep*" rg])))


;;; Savehist
;; built-in

(mini-bltin savehist
  (add-hook 'minibuffer-mode-hook 'savehist-mode))


;;; Saveplace
;; built-in

(mini-bltin saveplace
  (add-hook 'after-init-hook 'save-place-mode))


;;; Shx

(mini-pkgif shx
  (declare-function shx-mode nil)
  (add-hook 'shell-mode-hook #'shx-mode))


;;; Simple
;; built-in

(mini-bltin simple
  (mini-set column-number-mode t)
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
  (mini-defk ?k 'kill-current-buffer ctl-x-map)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'emacs-startup-hook 'turn-on-auto-fill))


;;; Smtpmail
;; built-in

(mini-bltin smtpmail
  (mini-set send-mail-function 'smtpmail-send-it)
  (mini-set smtpmail-stream-type 'ssl))


;;; Somafm

(mini-pkgif somafm
  (autoload 'somafm "somafm" "Refresh channels and display the channels buffer.
If we don't have the list already, or if the refresh interval
has passed, otherwise show the channel buffer." t)
  (mini-addmenu "tools"
    '(["Soma.fm radio" somafm])
    '("Other Apps*" "Media")))


;;; Startup
;; built-in

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


;;; Su

(mini-pkgif su
  nil) ;; placeholder


;;; Subword
;; built-in

(mini-bltin subword
  (autoload 'subword-mark "subword" "Do the same as `mark-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `mark-word'." t))


;;; Tab-bar
;; built-in

(mini-bltin tab-bar
  (add-hook 'after-init-hook 'tab-bar-mode)
  (mini-set tab-bar-format
    '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
  (mini-set tab-bar-select-tab-modifiers '(meta))
  (mini-set tab-bar-tab-hints t)
  (mini-set tab-bar-close-button-show nil))


;;; Term
;; built-in

(mini-bltin term
  (defun mini-termsh ()
    "Invoke `term' without having to press Enter first."
    (interactive)
    (term "/bin/bash")))


;;; Time
;; built-in

(mini-bltin time
  (mini-set display-time-default-load-average nil)
  (mini-set display-time-format "%l:%M%#p ")
  (mini-set display-time-mode t))


;;; Tmm
;; built-in

(mini-bltin tmm
   ;; Global-map
  (mini-defk "<f10>"	'tmm-menubar)
  ;; Isearch-mode-map
  (mini-eval isearch
    (mini-defk "<f10>"     'isearch-tmm-menubar  isearch-mode-map))
  ;; Minibuffer-local-map
  (mini-eval minibuffer
    (mini-defk "<f10>"     'keyboard-escape-quit minibuffer-local-map)))


;;; Transpose-frame

(mini-pkgif transpose-frame
  (defvar mini-transpose-frame-prefix-map)
  (define-prefix-command 'mini-transpose-frame-prefix-command 'mini-transpose-frame-prefix-map)
  (mini-defk ?f 'mini-transpose-frame-prefix-command mini-transpose-prefix-map)
  (mini-defk ?t 'transpose-frame                   mini-transpose-frame-prefix-map)
  (mini-defk ?f 'flip-frame                        mini-transpose-frame-prefix-map)
  (mini-defk ?l 'flop-frame                        mini-transpose-frame-prefix-map)
  (mini-defk ?r 'rotate-frame                      mini-transpose-frame-prefix-map)
  (mini-defk ?c 'rotate-frame-clockwise            mini-transpose-frame-prefix-map)
  (mini-defk ?a 'rotate-frame-anticlockwise        mini-transpose-frame-prefix-map))


;;; Treemacs

(mini-pkgif treemacs
  (mini-defk "<f8>" 'treemacs))


;;; Treemacs-tab-bar

(mini-pkgif treemacs-tab-bar
  (mini-eval treemacs
    (require 'treemacs-tab-bar)
    (declare-function treemacs-set-scope-type nil)
    (treemacs-set-scope-type 'Tabs)))


;;; Tree-sitter

(mini-pkgif tree-sitter
  (mini-ensure tree-sitter-langs)
  (mini-eval tree-sitter
    (require 'tree-sitter-langs)))


;;; Vertico

(mini-pkgif vertico
  (add-hook 'after-init-hook 'vertico-mode)
  (defvar vertico-indexed-mode)
  (mini-set vertico-indexed-mode t)
  (mini-set vertico-cycle t)
  (declare-function vertico-indexed-mode "vertico-indexed")
  (defun mini-tmm-with-vertico ()
    "A wrapper around `tmm-menubar' to ensure all items are shown."
    (interactive)
    (let ((vertico-count 40)
	  (vertico-count-format nil)
	  (vindexstatus vertico-indexed-mode))
      (when vindexstatus
	;; Turn off vertico-indexed-mode...
	(vertico-indexed-mode 0)
	;; But turn it back on after this finishes.  (Turning it on
	;; after the call to `tmm-menubar' usually won't work, because
	;; if you quit from the menu, the command aborts.)
	(run-with-timer 0 nil 'vertico-indexed-mode))
      (ignore vertico-count
	      vertico-count-format)
      (call-interactively 'tmm-menubar)))

  (mini-defk [f10] 'mini-tmm-with-vertico)

  (mini-eval vertico
    (defvar vertico-map)
    ;; Use C-s and C-r to navigate between results (esp. for consult-line)
    (mini-defk "C-s" 'vertico-next     vertico-map)
    (mini-defk "C-r" 'vertico-previous vertico-map)

    ;; Avoid duplicate menu when using `tmm-menubar'.
    (declare-function tmm-add-prompt "tmm")
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


;;; Vertigo

;; Not to be confused with the "Vertico".

(mini-pkgif vertigo
  (mini-set vertigo-home-row (if (eq mini-keyboard-layout 'programmer-dvorak)
				 '(?u ?h ?e ?t ?o ?n ?a ?s ?i ?d)
                               (append mini-keys-homerow-10 nil)))
  (mini-set vertigo-cut-off 9)
  (mini-set vertigo-max-digits 2)
  (mini-defk [?\M-g ?\M--] 'vertigo-set-negative-digit-argument)
  (mini-defk [?\M-g ?-] 'vertigo-set-negative-digit-argument)
  (mini-defk [?\M-g ?\M-r] 'vertigo-set-digit-argument)
  (mini-defk [?\M-p] 'vertigo-jump-up)
  (mini-defk [?\M-n] 'vertigo-jump-down)
  (defun mini-vertigo-jump-up-column ()
    ""
    (interactive)
    (declare-function vertigo--run nil)
    (vertigo--run #'previous-line "Jump up: "))
  (defun mini-vertigo-jump-down-column ()
    ""
    (interactive)
    (vertigo--run #'next-line "Jump down: "))
  ;; FIXME Make it go to end of line if column number exceeds length of line.
  (defun mini-vertigo-jump-to-column ()
    "Jump to a specific absolute column."
    (interactive)
    (vertigo--run (lambda (arg)
                    (interactive "p")
                    (move-beginning-of-line 1)
                    (forward-char arg))
                  "Jump to column: "))
  (defun mini-vertigo-go-up ()
    ""
    (interactive)
    (declare-function vertigo-jump-up nil)
    (vertigo-jump-up)
    (declare-function mini-vertigo-jump-to-column nil)
    (mini-vertigo-jump-to-column))
  (defun mini-vertigo-go-down ()
    ""
    (interactive)
    (declare-function vertigo-jump-down nil)
    (vertigo-jump-down)
    (mini-vertigo-jump-to-column)))


;;; View
;; built-in

(mini-bltin view
  (mini-defk ?v 'view-mode mode-specific-map)
  (mini-addmenu "options"
    '(["View-Mode" view-mode])
    '("View Enhancements*")))


;;; Vterm

(mini-pkgif vterm
  ;; Needs: cmake libtool
  ;; :system-deps ("cmake" "libtool")
  (mini-addmenu "tools"
    '(["Vterm" vterm])
    '("Other Apps*" "System" "Command Lines")))


;;; Vundo

(mini-pkgif vundo
  (mini-addmenu "tools"
    '(["Visualize Undos*" vundo]))
  (mini-defk "v" 'vundo mode-specific-map))


;;; Warnings
;; built-in

(mini-bltin warnings
  (mini-set warning-minimum-level :error))


;;; Which-func
;; built-in

(mini-bltin which-func
  (add-hook 'prog-mode-hook 'which-function-mode)
  (add-hook 'org-mode-hook 'which-function-mode)
  (mini-set which-function-mode nil))


;;; Which-key

(mini-pkgif which-key
  (mini-addmenu "options"
    '(["Which-Key* (pop-up keybindings)" which-key-mode])
    '("Show/Hide"))
  (add-hook 'window-setup-hook
            'which-key-setup-side-window-right-bottom)
  (mini-eval which-key
    (declare-function which-key-mode nil)
    (which-key-mode)
    (dolist (kb '((?t . which-key-show-top-level)
                  (?M . which-key-show-major-mode)))
      (mini-defk (car kb) (cdr kb) help-map))
    (defvar which-key-replacement-alist)
    (push (cons '(nil . "\\([[:alnum:]-]+\\)-mode$")
		(lambda (kb)
                  (cons (car kb)
			(let* ((kbstr (cdr kb))
                               (cmdsym (intern kbstr))
                               (vblsym (cond
					((boundp cmdsym) cmdsym)
					;; mode command with variable named differently
					((eq cmdsym 'read-only-mode) 'buffer-read-only))))
                          ;; TODO Fix checkbox for edebug
                          ;; modes ("C-x X" and "C-x C-a")?
                          ;; These several modes set the
                          ;; value of a couple variables to
                          ;; indicate which one is active or
                          ;; will be active.
                          (cond
                           ;; commands ending in "mode" that aren't modes.
                           ((member kbstr '("which-key-show-major-mode" "describe-mode")) kbstr)
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
    ;; TEMP Remove the following if and when PR #333 is
    ;; accepted.  The rest of this block is a fix for
    ;; side-window-right popup, which without this allows the
    ;; list of bindings to overlap the bottom of the screen
    ;; when `line-spacing' is non-nil.
    (defvar which-key-side-window-location)
    (declare-function
     which-key--height-or-percentage-to-height "which-key")
    (defvar which-key-side-window-max-height)
    (declare-function
     which-key--total-width-to-text "which-key")
    (declare-function
     which-key--width-or-percentage-to-width "which-key")
    (defvar which-key-side-window-max-width)
    (defvar which-key-unicode-correction)
    (defun which-key--side-window-max-dimensions ()
      "Return max-dimensions of the side-window popup (height . width)
in lines and characters respectively."
      (cons
       ;; height
       (if (member which-key-side-window-location
                   '(left right))
           ;; 1 is a kludge to make sure there is no overlap
           (- (/ (- (frame-inner-height)
                    (window-pixel-height
                     (minibuffer-window)))
		 (default-line-height))
              1)
	 (which-key--height-or-percentage-to-height
          which-key-side-window-max-height))
       ;; width
       (max 0
            (- (if (member which-key-side-window-location
                           '(left right))
                   (which-key--total-width-to-text
                    (which-key--width-or-percentage-to-width
                     which-key-side-window-max-width))
		 (which-key--total-width-to-text
                  (which-key--width-or-percentage-to-width
                   1.0)))
               which-key-unicode-correction))))))


;;; Whitespace

(mini-bltin whitespace
  (mini-set show-trailing-whitespace t)
  (mini-set whitespace-style
    '(face trailing lines-tail empty indentation::tab big-indent
	   space-after-tab::tab space-before-tab::tab)))


;;; Windmove
;; built-in

(mini-bltin windmove
  (mini-windmove-defk
   (left right up down)
   (("windmove"         (?\C-c ?w) control)
    ("windmove-display" (?\C-c ?w) meta)
    ("windmove-delete"  (?\C-c ?w) control shift)
    ("windmove-swap"    (?\C-c ?w) control meta)))

  ;; Additional commands for creating a frame, creating a tab, or
  ;; ensuring the next buffer appears in the current window.
  (mini-defk ?\M-N 'windmove-display-new-frame) ;; M-F interferes with shift-selection
  (mini-defk ?\M-T 'windmove-display-new-tab)
  (mini-defk ?\M-S 'windmove-display-same-window))


;;; Winner
;; built-in

(mini-bltin winner
  ;; This automatically binds "C-c <left>" to `winner-undo' and
  ;; "C-c <right>" to `winner-redo'.  To prevent that, you can set
  ;; `winner-dont-bind-my-keys' to a non-nil value beforehand.
  (add-hook 'window-setup-hook 'winner-mode)
  (mini-addmenu "options"
    '(["Winner Mode (undo window arrangements)" winner-mode])))


;;; Window
;; built-in

(mini-bltin window
  (mini-defk "M-o" 'other-window))


;;; Yasnippet

(mini-pkgif yasnippet
  ;; Is this not duplicative with abbrev, auto-insert, skeletons, etc?
  (add-hook 'find-file-hook 'yas-global-mode)
  (mini-ensure yasnippet-snippets))


(provide 'mini-packages)
;;; mini-packages.el ends here
