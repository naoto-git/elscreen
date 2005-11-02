;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen.el 
;;
(defconst elscreen-version "1.3.2 (August 23, 2004)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;;              Nara Institute of Science and Technology, Japan
;; Based on: screens.el
;;              by Heikki T. Suopanki <suopanki@stekt1.oulu.fi>
;; Created:  June 22, 1996
;; Revised:  August 23, 2004

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'elscreen)
(require 'alist)

(defconst elscreen-on-xemacs (featurep 'xemacs))
(defconst elscreen-on-emacs (and (not elscreen-on-xemacs)
                                 (>= emacs-major-version 21)))


;;; User Customizable Variables:

(defgroup elscreen nil
  "ElScreen -- Screen Manager for Emacssen"
  :tag "ElScreen"
  :group 'environment)

(defcustom elscreen-prefix-key "\C-z"
  "*Command prefix for ElScreen commands."
  :type '(string :size 10)
  :tag "Prefix-key"
  :set (lambda (symbol value)
	 (when (boundp 'elscreen-map)
	   (global-set-key value elscreen-map)
	   (global-unset-key elscreen-prefix-key))
	 (custom-set-default symbol value))
  :group 'elscreen)

(defcustom elscreen-display-screen-number t
  "*If non-nil, show the number of the current screen in the mode line."
  :type 'boolean
  :tag "Show screen number"
  :group 'elscreen)

(defcustom elscreen-default-buffer-name "*scratch*"
  "*Name of a buffer in new screen."
  :type '(string :size 24)
  :tag "Default buffer name"
  :group 'elscreen)

(defcustom elscreen-mode-to-nickname-alist
  '(("^wl-draft-mode$" . (lambda (buf) (format "WL(%s)" (buffer-name buf))))
    ("^wl-" . "Wanderlust")
    ("^mew-draft-mode$" . (lambda (buf) (format "Mew(%s)" (buffer-name buf))))
    ("^mew-" . "Mew")
    ("^w3m-mode" . (lambda (buf) (w3m-current-title)))
    ("^irchat-" . "IRChat")
    ("^liece-" . "Liece")
    ("^dired-mode$" . (lambda (buf)
			(set-buffer buf)
			(format "Dired(%s)" dired-directory)))
    ("^Info-mode$" . "Info")
    ("^lookup-" . "Lookup"))
  "*Alist composed of the pair of mode-name and corresponding screen-name."
  :type 'sexp
  :tag "Major-mode to nickname alist"
  :group 'elscreen)

(defcustom elscreen-buffer-to-nickname-alist
  '(("[Ss]hell" . "shell")
    ("compilation" . "compile")
    ("-telnet" . "telnet")
    ("dict" . "OnlineDict")
    ("*WL:Message*" . "Wanderlust"))
  "*Alist composed of the pair of regular expression of
buffer-name and corresponding screen-name."
  :type 'sexp
  :tag "Buffer-name to nickname alist"
  :group 'elscreen)

(defvar elscreen-display-tab-set-nil-hook nil)
(when elscreen-on-emacs ; GNU Emacs 21
  (defcustom elscreen-display-tab t
    "*If non-nil, display the tabs at the top of screen."
    :type 'boolean
    :tag "Display screen tab"
    :set (lambda (symbol value)
	   (custom-set-default symbol value)
	   (when (fboundp 'elscreen-e21-tab-update)
	     (elscreen-e21-tab-update t)
	     (run-hooks 'elscreen-display-tab-set-nil-hook)))
    :group 'elscreen)

  (defcustom elscreen-tab-width 16
    "*Tab width (should be equal or greater than 6)."
    :type '(integer :size 4)
    :tag "Tab width"
    :set (lambda (symbol value)
	   (when (and (numberp value)
		      (>= value 6))
	     (custom-set-default symbol value)
	     (when (fboundp 'elscreen-e21-tab-update)
	       (elscreen-e21-tab-update t)))))

  (defcustom elscreen-tab-display-create-screen t
    "*If non-nil, display the tab to create new screen at the most left side."
    :type 'boolean
    :tag "Display tab to create new screen"
    :set (lambda (symbol value)
	   (custom-set-default symbol value)
	   (when (fboundp 'elscreen-e21-tab-update)
	     (elscreen-e21-tab-update t)))
    :group 'elscreen)

  (defcustom elscreen-tab-display-kill-screen t
    "*If non-nil, display the icons to kill a screen at left side of each tab."
    :type 'boolean
    :tag "Display icons to kill each screen"
    :set (lambda (symbol value)
	   (custom-set-default symbol value)
	   (when (fboundp 'elscreen-e21-tab-update)
	     (elscreen-e21-tab-update t)))
    :group 'elscreen)

  (defface elscreen-tab-background-face
    '((((type x w32 mac) (class color))
       :background "Gray50")
      (((class color))
       (:background "black")))
    "*Face to fontify background of tab line."
    :group 'elscreen)

  (defface elscreen-tab-current-screen-face
    '((((class color))
       (:background "white" :foreground "black"))
      (t (:underline t)))
    "*Face for current screen tab."
    :group 'elscreen)

  (defface elscreen-tab-other-screen-face
    '((((type x w32 mac) (class color))
       :background "Gray85" :foreground "Gray50")
      (((class color))
       (:background "blue" :foreground "black")))
    "*Face for tabs other than current screen one."
    :group 'elscreen)
  )

;;; Key & Menu bindings:

(defvar elscreen-map (make-sparse-keymap)
  "*Keymap for ElScreen.")
(global-set-key elscreen-prefix-key elscreen-map)

(define-key elscreen-map "\C-c" 'elscreen-create)
(define-key elscreen-map "c"    'elscreen-create)
(define-key elscreen-map "\C-k" 'elscreen-kill)
(define-key elscreen-map "k"    'elscreen-kill)
(define-key elscreen-map "\C-p" 'elscreen-previous)
(define-key elscreen-map "p"    'elscreen-previous)
(define-key elscreen-map "\C-n" 'elscreen-next)
(define-key elscreen-map "n"    'elscreen-next)
(define-key elscreen-map "\C-a" 'elscreen-toggle)
(define-key elscreen-map "a"    'elscreen-toggle)
(define-key elscreen-map "g"    'elscreen-goto)
(define-key elscreen-map "0"    'elscreen-jump)
(define-key elscreen-map "1"    'elscreen-jump)
(define-key elscreen-map "2"    'elscreen-jump)
(define-key elscreen-map "3"    'elscreen-jump)
(define-key elscreen-map "4"    'elscreen-jump)
(define-key elscreen-map "5"    'elscreen-jump)
(define-key elscreen-map "6"    'elscreen-jump)
(define-key elscreen-map "7"    'elscreen-jump)
(define-key elscreen-map "8"    'elscreen-jump)
(define-key elscreen-map "9"    'elscreen-jump)
(define-key elscreen-map "?"    'elscreen-help)
(define-key elscreen-map "b"    'elscreen-goto-screen-with-buffer)
(define-key elscreen-map "\C-s" 'elscreen-swap)
(define-key elscreen-map "\C-w" 'elscreen-display-screen-name-list)
(define-key elscreen-map "w"    'elscreen-display-screen-name-list)
(define-key elscreen-map "\C-m" 'elscreen-display-last-message)
(define-key elscreen-map "m"    'elscreen-display-last-message)
(define-key elscreen-map "t"    'elscreen-display-time)
(define-key elscreen-map "A"    'elscreen-screen-nickname)
(define-key elscreen-map "v"    'elscreen-display-version)
(define-key elscreen-map "i"    'elscreen-display-screen-number-toggle)
(define-key elscreen-map "j"    'elscreen-link)
(define-key elscreen-map "s"    'elscreen-split)
(define-key elscreen-map "\C-f" 'elscreen-find-file)
(define-key elscreen-map "\M-x" 'elscreen-execute-extended-command)

(define-key minibuffer-local-map elscreen-prefix-key 'undefined)

(defvar elscreen-help "ElScreen keys:
       \\[elscreen-create]    Create a new screen and switch to it
       \\[elscreen-kill]    Kill the current screen
       \\[elscreen-next]    Switch to the \"next\" screen in a cyclic order
       \\[elscreen-previous]    Switch to the \"previous\" screen in a cyclic order
       \\[elscreen-toggle]    Toggle to the screen selected previously
       \\[elscreen-goto]    Jump to the specified screen
       \\[elscreen-jump-0]  
         :      Jump to the screen #
       \\[elscreen-jump-9]      
       \\[elscreen-display-screen-name-list]    Show list of screens
       \\[elscreen-screen-nickname]    Name the current screen
       \\[elscreen-display-last-message]    Show last message
       \\[elscreen-display-time]    Show time
       \\[elscreen-display-version]    Show ElScreen version
       \\[elscreen-find-file]    Create new screen and open file
       \\[elscreen-display-screen-number-toggle]    Show/hide the screen number in the mode line
       \\[elscreen-help]    Show this help"
  "*Help shown by elscreen-help-mode")


(cond
 (elscreen-on-emacs ; GNU Emacs 21
  (define-key-after (lookup-key global-map [menu-bar]) [elscreen]
    (cons "ElScreen" (make-sparse-keymap "ElScreen")) 'buffer)

  (defvar elscreen-menu-bar-command-entries
    (list (list 'elscreen-command-separator
		'menu-item
		"--")
	  (list 'elscreen-create
		'menu-item
		"Create Screen"
		'elscreen-create
		:help "Create a new screen and switch to it")
	  (list 'elscreen-kill
		'menu-item
		"Kill Screen"
		'elscreen-kill
		:help "Kill the current screen")
	  (list 'elscreen-next
		'menu-item
		"Next Screen"
		'elscreen-next
		:help "Switch to the \"next\" screen in a cyclic order")
	  (list 'elscreen-previous
		'menu-item
		"Previous Screen"
		'elscreen-previous
		:help "Switch to the \"previous\" screen in a cyclic order")
	  (list 'elscreen-toggle
		'menu-item
		"Toggle Screen"
		'elscreen-toggle
		:help "Toggle to the screen selected previously")))

  (defvar elscreen-tab-separator
    (propertize
     " "
     'face 'elscreen-tab-background-face
     'display '(space :width 0.5))
    "String used to separate tabs.")
  )
 (elscreen-on-xemacs ; XEmacs
  (defvar elscreen-menu-bar-command-entries
    '("%_ElScreen"
      :filter elscreen-xmas-menu-bar-filter
      "----"  
      ["%_Create Screen" elscreen-create]
      ["%_Kill Screen" elscreen-kill]  
      ["%_Next Screen" elscreen-next]
      ["%_Previous Screen" elscreen-previous]
      ["%_Toggle Screen" elscreen-toggle]
      ))

  (defconst elscreen-menubar (copy-sequence default-menubar))
  (let ((menubar elscreen-menubar))
    (catch 'buffers-menu-search
      (while (car menubar)
	(when (equal (car (car menubar)) "%_Buffers")
	  (throw 'buffers-menu-search menubar))
	(setq menubar (cdr menubar))))
    (setcdr menubar (cons elscreen-menu-bar-command-entries (cdr menubar))))

  (set-menubar elscreen-menubar)
  (set-menubar-dirty-flag)
  ))


;;; Code:

; Window configuration handling

(defvar elscreen-frame-confs nil
  "*Alist that contains the information about screen configurations.")

(defun get-alist (key alist)
  (cdr (assoc key alist)))

(defsubst elscreen-get-frame-confs (frame)
  (get-alist frame elscreen-frame-confs))

(defun elscreen-make-frame-confs (frame)
  (if (null (elscreen-get-frame-confs frame))
      (set-alist 'elscreen-frame-confs
		 frame (list
			(cons 'status
			      (list (cons 'current-screen 0)
				    (cons 'previous-screen nil)
				    (cons 'modified-inquirer nil)))
			(cons 'window-configuration
			      (list (cons 0 (current-window-configuration))))
			(cons 'screen-nickname nil)))))

(defun elscreen-delete-frame-confs (frame)
  (remove-alist 'elscreen-frame-confs frame))

(if (boundp 'after-make-frame-functions)
    ; GNU Emacs 21
    (add-hook 'after-make-frame-functions 'elscreen-make-frame-confs)
  ; XEmacs
  (add-hook 'create-frame-hook 'elscreen-make-frame-confs))
(add-hook 'delete-frame-hook 'elscreen-delete-frame-confs)
(elscreen-make-frame-confs (selected-frame))

(defsubst elscreen-get-conf-list (frame type)
  (get-alist type (elscreen-get-frame-confs frame)))

(defsubst elscreen-set-conf-list (frame type conf-list)
  (let ((frame-conf (elscreen-get-frame-confs frame)))
    (set-alist 'frame-conf type conf-list)))

(defun elscreen-get-status (status &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (status-list (elscreen-get-conf-list frame 'status)))
    (get-alist status status-list)))

(defun elscreen-set-status (status value &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (status-list (elscreen-get-conf-list frame 'status)))
    (set-alist 'status-list status value)
    (elscreen-set-conf-list frame 'status status-list)))

(defun elscreen-get-current-screen (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-get-status 'current-screen frame)))

(defun elscreen-set-current-screen (value &optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-set-status 'current-screen value frame)))

(defun elscreen-get-previous-screen (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-get-status 'previous-screen frame)))

(defun elscreen-set-previous-screen (value &optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-set-status 'previous-screen value frame)))

(defvar elscreen-screen-update-hook nil)
(add-hook 'post-command-hook (lambda ()
			       (run-hooks 'elscreen-screen-update-hook)))

(defun elscreen-screen-modified-p (inquirer &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (inquirer-list (elscreen-get-status 'modified-inquirer frame))
	 (modified (null (memq inquirer inquirer-list))))
    (add-to-list 'inquirer-list inquirer)
    (elscreen-set-status 'modified-inquirer inquirer-list frame)
    modified))

(defun elscreen-screen-modified (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-set-status 'modified-inquirer nil frame)))

(defvar elscreen-screen-modified-hook-suppress nil)
(defvar elscreen-screen-modified-hook-pwc nil)
(defun elscreen-screen-modified-hook (&optional mode) ; hook for GNU Emacs
  (when (and (not (window-minibuffer-p))
	     (fboundp 'compare-window-configurations)
	     (not elscreen-screen-modified-hook-suppress)
	     (or (eq mode 'force)
		 (eq mode 'force-immediately)
		 (null elscreen-screen-modified-hook-pwc)
		 (not (compare-window-configurations
		       (current-window-configuration)
		       elscreen-screen-modified-hook-pwc))))
    (setq elscreen-screen-modified-hook-pwc
	  (current-window-configuration))
    (elscreen-screen-modified)))

(defmacro elscreen-screen-modified-set-hook (&rest hooks-and-functions)
  (cons
   'progn
   (mapcar
    (lambda (hook-or-function)
      (let ((mode 'normal))
	(when (listp hook-or-function)
	  (setq mode (nth 1 hook-or-function))
	  (setq hook-or-function (nth 0 hook-or-function)))
	(cond
	 ((functionp hook-or-function)
	  ( `(defadvice (, hook-or-function) (around
					      elscreen-screen-modified-advice
					      activate)
	       ad-do-it
	       (elscreen-screen-modified-hook '(, mode))
	       (when (eq '(, mode) 'force-immediately)
		 (run-hooks 'elscreen-screen-update-hook)))
	    ))
	 ((boundp hook-or-function)
	  ( `(add-hook '(, hook-or-function)
		       (lambda (&rest ignore)
			 (elscreen-screen-modified-hook '(, mode))
			 (when (eq '(, mode) 'force-immediately)
			   (run-hooks 'elscreen-screen-update-hook)))))))))
      hooks-and-functions)))

(elscreen-screen-modified-set-hook
 (recenter 'force) (change-major-mode-hook 'force)
 window-configuration-change-hook window-size-change-functions
 (pre-command-hook 'force)
 (handle-switch-frame 'force) ; GNU Emacs 21
 (select-frame-hook 'force) ; XEmacs
 )

(defun elscreen-get-window-configuration (screen &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (winconf-list (elscreen-get-conf-list frame 'window-configuration)))
    (get-alist screen winconf-list)))

(defun elscreen-set-window-configuration (screen winconf &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (winconf-list (elscreen-get-conf-list frame 'window-configuration)))
    (set-alist 'winconf-list screen winconf)
    (elscreen-set-conf-list frame 'window-configuration winconf-list)))

(defun elscreen-delete-window-configuration (screen &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (winconf-list (elscreen-get-conf-list frame 'window-configuration)))
    (remove-alist 'winconf-list screen)
    (elscreen-set-conf-list frame 'window-configuration winconf-list)))

(defun elscreen-get-screen-nickname (screen &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (screen-nickname-list (elscreen-get-conf-list frame 'screen-nickname)))
    (get-alist screen screen-nickname-list)))

(defun elscreen-set-screen-nickname (screen screen-nickname &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (screen-nickname-list (elscreen-get-conf-list frame 'screen-nickname)))
    (set-alist 'screen-nickname-list screen screen-nickname)
    (elscreen-set-conf-list frame 'screen-nickname screen-nickname-list)))

(defun elscreen-delete-screen-nickname (screen &optional frame)
  (let* ((frame (or frame (selected-frame)))
	 (screen-nickname-list (elscreen-get-conf-list frame 'screen-nickname)))
    (remove-alist 'screen-nickname-list screen)
    (elscreen-set-conf-list frame 'screen-nickname screen-nickname-list)))

(defsubst elscreen-get-number-of-screens (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (length (elscreen-get-screen-list frame))))

(defsubst elscreen-get-screen-list (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (mapcar 'car (elscreen-get-conf-list frame 'window-configuration))))

(defun elscreen-screen-live-p (screen &optional frame)
  (let* ((frame (or frame (selected-frame))))
    (not (null (elscreen-get-window-configuration screen frame)))))

(defun elscreen-copy-tree (tree)
  (when (consp tree)
    (let ((original-tree tree))
      (setq tree (list (elscreen-copy-tree (car original-tree))))
      (nconc tree (elscreen-copy-tree (cdr original-tree)))))
  tree)

(defmacro elscreen-save-screen-excursion (&rest body)
  (` (let ((original-buffer-list (buffer-list))
	   (original-frame-confs (elscreen-copy-tree elscreen-frame-confs)))
       (unwind-protect
	   (progn
	     (,@ body))
	 (setq elscreen-frame-confs original-frame-confs)
	 (elscreen-goto-internal (elscreen-get-current-screen))
	 (elscreen-screen-number-string-update)
	 (while original-buffer-list
	   (bury-buffer (car original-buffer-list))
	   (setq original-buffer-list (cdr original-buffer-list)))))))


; Display message in minibuffers

(defun elscreen-message (message &optional sec)
  (when message
    (setq elscreen-last-message message)
    (message "%s" message)
    (sit-for (or sec 3)))
  (message nil))


; Display screen number in modelines

(defvar elscreen-screen-number-string "[0]")
(let ((mode-line (or
		  ; GNU Emacs 21.3.50 or later
		  (memq 'mode-line-position mode-line-format)
		  ; GNU Emacs 21.3.1
		  (memq 'mode-line-buffer-identification mode-line-format)
		  ; XEmacs
		  (memq 'global-mode-string mode-line-format)))
      (elscreen-mode-line-elm
       '(elscreen-display-screen-number elscreen-screen-number-string)))
  (if (null (member elscreen-mode-line-elm mode-line))
      (setcdr mode-line (cons elscreen-mode-line-elm (cdr mode-line)))))

(defsubst elscreen-screen-number-string-update ()
  (setq elscreen-screen-number-string
	(format "[%d]" (elscreen-get-current-screen))))

(defun elscreen-display-screen-number-toggle ()
  "Toggle the screen number in the mode line."
  (interactive)
  (setq elscreen-display-screen-number (null elscreen-display-screen-number))
  (redraw-frame (selected-frame)))


; Create/Kill a screen

(defvar elscreen-create-hook nil)
(defvar elscreen-kill-hook nil)

(defun elscreen-create () 
  "Create a new screen."
  (interactive)
  (cond
   ((>= (elscreen-get-number-of-screens) 10)
    (elscreen-message "Can't create any more screen")
    nil)
   (t
    (elscreen-set-window-configuration (elscreen-get-current-screen)
				       (current-window-configuration))
    (elscreen-set-previous-screen (elscreen-get-current-screen))
    (let ((screen-list (sort (elscreen-get-screen-list) '<))
	  (i 0))
      (while (eq (nth i screen-list) i)
	(setq i (+ i 1)))
      (elscreen-set-current-screen i))
    (delete-other-windows) 
    (switch-to-buffer elscreen-default-buffer-name)
    (elscreen-screen-number-string-update)
    (elscreen-set-window-configuration (elscreen-get-current-screen)
				       (current-window-configuration))
    (elscreen-screen-modified)
    (run-hooks 'elscreen-create-hook)
    (elscreen-get-current-screen))))

(defun elscreen-kill (&optional screen) 
  "Kill the current screen."
  (interactive "P")
  (let ((screen (or (and (numberp screen) screen)
		    (elscreen-get-current-screen))))
    (cond
     ((not (elscreen-screen-live-p screen))
      (elscreen-message "There is no such screen, cannot kill")
      nil)
     ((= (elscreen-get-number-of-screens) 1)
      (elscreen-message "There is only one screen, cannot kill")
      nil)
     (t 
      (elscreen-delete-window-configuration screen)
      (elscreen-delete-screen-nickname screen)
      (cond
       ((eq screen (elscreen-get-current-screen))
	(let* ((screen-list (sort (elscreen-get-screen-list) '<))
	       (next-screen
		(or (elscreen-get-previous-screen)
		    (nth 1 (memq (elscreen-get-current-screen) screen-list))
		    (car screen-list))))
	  (elscreen-set-current-screen nil)
	  (elscreen-goto next-screen)))
       ((eq screen (elscreen-get-previous-screen))
	(elscreen-set-previous-screen nil)))
      (elscreen-screen-modified)
      (run-hooks 'elscreen-kill-hook)
      screen))))


; Switch the screen

(defvar elscreen-goto-hook nil)

(defsubst elscreen-goto-internal (screen)
  (set-window-configuration (elscreen-get-window-configuration screen)))

(defun elscreen-goto (screen) 
  "Jump to the specified screen."
  (interactive "NGoto screen number: ")
  (cond
   ((eq (elscreen-get-current-screen) screen))
   ((elscreen-screen-live-p screen)
    (if (elscreen-get-current-screen)
	(elscreen-set-window-configuration (elscreen-get-current-screen)
					   (current-window-configuration)))
    (elscreen-set-previous-screen (elscreen-get-current-screen))
    (elscreen-set-current-screen screen)
    (elscreen-goto-internal screen)
    (elscreen-screen-number-string-update)
    (redraw-frame (selected-frame))
    (elscreen-screen-modified)
    (run-hooks 'elscreen-goto-hook))
   (t
    (elscreen-message (format "No screen %d" screen)))))

(defun elscreen-next () 
  "Switch to the next screen."
  (interactive)
  (cond
   ((= (elscreen-get-number-of-screens) 1)
    (elscreen-message
     (format "You cannot escape from screen %d!"
	     (elscreen-get-current-screen))))
   (t
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
	   (next-screen
	    (or (nth 1 (memq (elscreen-get-current-screen) screen-list))
		(car screen-list))))
      (elscreen-goto next-screen)))))

(defun elscreen-previous () 
  "Switch to the previous screen."
  (interactive)
  (cond
   ((= (elscreen-get-number-of-screens) 1)
    (elscreen-message
     (format "You cannot escape from screen %d!"
	     (elscreen-get-current-screen))))
   (t
    (let* ((screen-list (sort (elscreen-get-screen-list) '>))
	   (next-screen
	    (or (nth 1 (memq (elscreen-get-current-screen) screen-list))
		(car screen-list))))
      (elscreen-goto next-screen)))))

(defun elscreen-toggle ()
  "Toggle to last screen."
  (interactive)
  (cond
   ((= (elscreen-get-number-of-screens) 1)
    (elscreen-message
     (format "You cannot escape from screen %d!"
	     (elscreen-get-current-screen))))
   ((elscreen-get-previous-screen)
    (elscreen-goto (elscreen-get-previous-screen)))
   (t
    (elscreen-previous))))
      
(defun elscreen-jump ()
  "Switch to specified screen."
  (interactive)
  (let ((next-screen (string-to-number (string last-command-char))))
    (if (and (<= 0 next-screen) (<= next-screen 9))
	(elscreen-goto next-screen))))


; Swap current screen number with previous one

(defun elscreen-swap ()	
  (interactive)	
  (let* ((current-screen (elscreen-get-current-screen))
	 (previous-screen (elscreen-get-previous-screen))
	 (current-window-configuration
	  (elscreen-get-window-configuration current-screen))
	 (previous-window-configuration
	  (elscreen-get-window-configuration previous-screen)))
    (elscreen-set-window-configuration current-screen
				       previous-window-configuration)
    (elscreen-set-window-configuration previous-screen
				       current-window-configuration)
    (elscreen-goto-internal current-screen)))


; Get screen with specified buffer, or create new screen

(defun elscreen-get-screen-create (buffer)
  (elscreen-set-window-configuration (elscreen-get-current-screen)
				     (current-window-configuration))
  (setq elscreen-screen-modified-hook-suppress t)
  (let* ((screen-list (sort (elscreen-get-screen-list) '<))
	 (target-screen 
	  (catch 'find-screen-with-buffer
	    (elscreen-save-screen-excursion
	     (mapcar
	      (lambda (screen)
		(elscreen-goto-internal screen)
		(if (member buffer
			    (mapcar (lambda (window)
				      (window-buffer window))
				    (window-list)))
		    (throw 'find-screen-with-buffer screen)))
	      screen-list)
	     nil))))
    (setq elscreen-screen-modified-hook-suppress nil)
    (if target-screen
 	(progn
 	  (elscreen-goto target-screen)
 	  (select-window (get-buffer-window buffer)))
      (if (setq target-screen (elscreen-create))
	  (switch-to-buffer buffer)
 	(split-window)
 	(switch-to-buffer-other-window buffer)))
    target-screen))

(defun elscreen-screen-nickname (screen-nickname)
  "Specify screen name."
  (interactive "sSet window title to: ")
  (cond
   ((eq (length screen-nickname) 0)
    (elscreen-delete-screen-nickname (elscreen-get-current-screen)))
   (t
    (elscreen-set-screen-nickname (elscreen-get-current-screen)
				  screen-nickname)))
  (elscreen-screen-modified))

(defmacro elscreen-get-alist-to-nickname (alist op cond)
  ( `(catch '(, alist)
       (progn
	 (mapcar
	  (lambda (map)
	    (let ((nickname nil))
	      (when ((, op) (car map) (, cond))
		(cond
		 ((functionp (cdr map))
		  (setq nickname (funcall (cdr map) (window-buffer window))))
		 (t
		  (setq nickname (cdr map))))
		(throw '(, alist) (cons 'nickname nickname)))))
	  (, alist))
	 nil))))

(defun elscreen-get-screen-to-name-alist (&optional truncate-length padding)
  (elscreen-set-window-configuration (elscreen-get-current-screen)
				     (current-window-configuration))
  (setq elscreen-screen-modified-hook-suppress t)
  (let* ((screen-list (sort (elscreen-get-screen-list) '<))
	 (screen-name nil)
	 (screen-to-name-alist nil)
	 (nickname-list nil))
    (elscreen-save-screen-excursion
     (mapcar
      (lambda (screen)
	(elscreen-goto-internal screen)
        ; if nickname exists, use it.
	(setq screen-name (elscreen-get-screen-nickname screen))
        ; nickname does not exist, so examine major-mode and buffer-name
	(when (null screen-name)
	  (walk-windows
	   (lambda (window)
	     (set-buffer (window-buffer window))
	     (setq nickname-list
		   (cons
		    (or
		     (elscreen-get-alist-to-nickname
		      elscreen-mode-to-nickname-alist
		      string-match (symbol-name major-mode))
		     (elscreen-get-alist-to-nickname
		      elscreen-buffer-to-nickname-alist
		      string-match (buffer-name))
		     (cons 'buffer-name (buffer-name)))
		    nickname-list)))
	   'other 'other)
	     
	  (setq screen-name (cdr (car nickname-list)))
	  (if (eq (car (car nickname-list)) 'nickname)
	      (setq nickname-list
		    (delete (car nickname-list) nickname-list))
	    (setq nickname-list (cdr nickname-list)))
	  (while (> (length nickname-list) 0)
	    (setq screen-name
		  (concat screen-name ":" (cdr (car nickname-list))))
	    (if (eq (car (car nickname-list)) 'nickname)
		(setq nickname-list
		      (delete (car nickname-list) nickname-list))
	      (setq nickname-list (cdr nickname-list)))))

	(if (and (integerp truncate-length)
		 (> truncate-length 3)
		 (> (string-width screen-name) truncate-length))
	    (setq screen-name
		  (truncate-string-to-width
		   (concat (truncate-string-to-width screen-name
						     (- truncate-length 3))
			   "...")
		   truncate-length nil ?.))
	  (when padding
	    (setq screen-name
		  (truncate-string-to-width screen-name
					    truncate-length nil ?\ ))))

	(set-alist 'screen-to-name-alist screen screen-name))
      screen-list))

    (setq elscreen-screen-modified-hook-suppress nil)
    screen-to-name-alist))

(defun elscreen-display-screen-name-list ()
  "Show list of screens."
  (interactive)
  (let ((screen-list (sort (elscreen-get-screen-list) '<))
	(screen-to-name-alist (elscreen-get-screen-to-name-alist))
	(current-screen (elscreen-get-current-screen))
	(previous-screen (elscreen-get-previous-screen)))
    (elscreen-message
     (mapconcat
      (lambda (screen)
	(let ((screen-name (get-alist screen screen-to-name-alist)))
	  (format "%d%s %s" screen
		  (or (and (eq screen current-screen) "+")
		      (and (eq screen previous-screen) "-")
		      "")
		  screen-name)))
      screen-list "  "))))


; Menu & Tab for GNU Emacs 21

(defun elscreen-e21-menu-bar-update (&optional force)
  (when (and (lookup-key (current-global-map) [menu-bar elscreen])
	     (or force
		 (and (elscreen-screen-modified-p
		       'elscreen-menu-bar-update))))
    (let ((screen-list (sort (elscreen-get-screen-list) '<))
	  (screen-to-name-alist (elscreen-get-screen-to-name-alist 25))
	  (elscreen-menu nil)
	  (current-screen (elscreen-get-current-screen))
	  (previous-screen (elscreen-get-previous-screen)))
      (setq elscreen-menu
	    (mapcar
	     (lambda (screen)
	       (let ((screen-name (get-alist screen screen-to-name-alist)))
		 (setq screen-name
		       (format "%d%s %s" screen
			       (or (and (eq screen current-screen) "+")
				   (and (eq screen previous-screen) "-")
				   " ")
			       screen-name))
		 (list (string-to-char (number-to-string screen))
		       'menu-item
		       screen-name
		       'elscreen-jump
		       :key-sequence (format "%s%d"
					     elscreen-prefix-key screen))))
	     screen-list))
      (setq elscreen-menu
	    (nconc elscreen-menu elscreen-menu-bar-command-entries))
      (setq elscreen-menu
	    (cons 'keymap (cons "Select Screen" elscreen-menu)))
      (define-key (current-global-map) [menu-bar elscreen]
	(cons (copy-sequence "ElScreen") elscreen-menu)))))

(defsubst elscreen-e21-tab-create-keymap (&optional function screen)
  (let ((keymap (make-sparse-keymap))
	(action (if (functionp function)
		    (if screen
			`(lambda (e)
			   (interactive "e")
			   (funcall ',function ,screen))
		      `(lambda (e)
			 (interactive "e")
			 (funcall ',function)))
		  'ignore)))
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line drag-mouse-1] 'ignore)
    (define-key keymap [header-line drag-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-1] action)
    (define-key keymap [header-line mouse-2] action)
    keymap))

(defmacro elscreen-e21-tab-append (property)
  ( `(setq elscreen-tab-format (append elscreen-tab-format
				       (list (, property))))))

(defun elscreen-e21-tab-update (&optional force)
  (when (and (not (window-minibuffer-p))
	     (or force
		 (elscreen-screen-modified-p 'elscreen-tab-update)))
    (let ((screen-list (sort (elscreen-get-screen-list) '<))
	  (screen-to-name-alist (elscreen-get-screen-to-name-alist
				 elscreen-tab-width t))
	  (current-screen (elscreen-get-current-screen))
	  (previous-screen (elscreen-get-previous-screen))
	  (top-window nil))
      (let (x y edges)
	(walk-windows
	 (lambda (window)
	   (setq edges (window-edges window))
	   (or (and x (< x (nth 0 edges))) (setq x (nth 0 edges)))
	   (or (and y (< y (nth 1 edges))) (setq y (nth 1 edges)))
	   (set-buffer (window-buffer window))
	   (when (and (boundp 'elscreen-tab-format)
		      (equal header-line-format elscreen-tab-format))
	     (setq elscreen-tab-format nil)
	     (setq header-line-format nil))
	   )
	 'other 'other)
	(setq top-window (window-at x y)))

      (when elscreen-display-tab
	(set-buffer (window-buffer top-window))
	(set (make-local-variable 'elscreen-tab-format) nil)
	(when elscreen-tab-display-create-screen
	  (elscreen-e21-tab-append
	   (propertize
	    "[!]"
	    'face 'elscreen-tab-current-screen-face
	    'local-map (elscreen-e21-tab-create-keymap
			'elscreen-create)))
	  (elscreen-e21-tab-append elscreen-tab-separator))

	  (mapcar
	   (lambda (screen)
	     (let ((face (if (eq current-screen screen)
			     'elscreen-tab-current-screen-face
			   'elscreen-tab-other-screen-face)))
	       (when elscreen-tab-display-kill-screen
		 (elscreen-e21-tab-append
		  (propertize
		   "[X]"
		   'face face
		   'local-map (elscreen-e21-tab-create-keymap
			       'elscreen-kill screen)))
		 (elscreen-e21-tab-append
		  (propertize
		   " "
		   'face face
		   'local-map (elscreen-e21-tab-create-keymap))))
	       
	       (elscreen-e21-tab-append
		(propertize
		 (format "%d%s %s" screen
			 (or (and (eq screen current-screen) "+")
			     (and (eq screen previous-screen) "-")
			     " ")
			 (get-alist screen screen-to-name-alist))
		 'face face
		 'local-map (elscreen-e21-tab-create-keymap
			     'elscreen-goto screen)))
	       (elscreen-e21-tab-append elscreen-tab-separator)))
	   screen-list)

	  (elscreen-e21-tab-append 
	   (propertize
	    (make-string (window-width) ?\ )
	    'face 'elscreen-tab-background-face
	    'local-map (elscreen-e21-tab-create-keymap)))

          (unless (equal elscreen-tab-format header-line-format)
	    (setq header-line-format elscreen-tab-format))))))

(when elscreen-on-emacs
  (elscreen-e21-menu-bar-update t)
  (elscreen-e21-tab-update t)
  (add-hook 'elscreen-screen-update-hook 'elscreen-e21-menu-bar-update)
  (add-hook 'elscreen-screen-update-hook 'elscreen-e21-tab-update))

; Menu for XEmacs

(defun elscreen-xmas-menu-bar-filter (menu)
  (let ((screen-list (sort (elscreen-get-screen-list) '<))
	(screen-to-name-alist (elscreen-get-screen-to-name-alist 25))
	(elscreen-menu nil)
	(current-screen (elscreen-get-current-screen))
	(previous-screen (elscreen-get-previous-screen)))
    (setq elscreen-menu
	  (mapcar
	   (lambda (screen)
	     (let ((screen-name (get-alist screen screen-to-name-alist)))
	       (setq screen-name
		     (format "%d%s %s" screen
			     (or (and (eq screen current-screen) "+")
				 (and (eq screen previous-screen) "-")
				 " ")
			     screen-name))
	       (vector screen-name
		       `(elscreen-goto ,screen)
		       :active t
		       ; :keys (format "%s %d" elscreen-prefix-key screen)
		       :keys (format "%s %d" "C-z" screen)
		       )))
	   screen-list))
    (append elscreen-menu menu)))


; Help

(defun elscreen-help () 
  "Help about screen functions."
  (interactive)
  (with-output-to-temp-buffer "*ElScreen Help*"
    (princ (documentation 'elscreen-help-mode))
    (print-help-return-message)))

(defun elscreen-add-help (&optional additional-help)
  (if additional-help 
      (setq elscreen-help (concat elscreen-help
				  (format "\n\n")
				  additional-help)))
  (let ((help-function
	 (list 'defun 'elscreen-help-mode '()
	       elscreen-help)))
    (eval help-function)))

(elscreen-add-help)


; Version

(defun elscreen-display-version ()
  "Display ElScreen version."
  (interactive)
  (elscreen-message (concat "ElScreen version " elscreen-version)))


; Utility Functions

(defvar elscreen-last-message "Welcome to ElScreen!"
  "*Last shown message.")

(defun elscreen-display-last-message ()
  "Display the last message."
  (interactive)
  (elscreen-message elscreen-last-message 5))

(defun elscreen-display-time ()
  "Display time."
  (interactive)
  (elscreen-message
   (concat (current-time-string) " " (nth 1 (current-time-zone))) 3))

(defun elscreen-goto-screen-with-buffer (buffer)
  "Go to the screen that has the window with buffer BUFNAME,
creating one if none already exists."
  (interactive "BGo to the screen with specified buffer: ")
  (elscreen-get-screen-create (get-buffer-create buffer)))

(defun elscreen-find-file (filename)
  "Edit file FILENAME.
Switch to a screen visiting file FILENAME,
creating one if none already exists."
  (interactive "FFind file in new screen: ")
  (elscreen-get-screen-create (find-file-noselect filename)))

(defun elscreen-execute-extended-command (prefix-arg)
  (interactive "P")
  (let ((prefix-arg prefix-arg))
    (setq this-command (read-command
                        ;; Note: this has the hard-wired
                        ;;  "C-u" and "M-x" string bug in common
                        ;;  with all Emacs's.
                        ;; (i.e. it prints C-u and M-x regardless of
                        ;; whether some other keys were actually bound
                        ;; to `execute-extended-command' and 
                        ;; `universal-argument'.
                        (cond ((eq prefix-arg '-)
                               "- M-x ")
                              ((equal prefix-arg '(4))
                               "C-u M-x ")
                              ((integerp prefix-arg)
                               (format "%d M-x " prefix-arg))
                              ((and (consp prefix-arg)
                                    (integerp (car prefix-arg)))
                               (format "%d M-x " (car prefix-arg)))
                              (t
                               "M-x ")))))
  (unless (elscreen-create)
    (split-window)
    (other-window 1))
  (command-execute this-command t))


; Unsupported Functions...

(defun elscreen-link ()
  (interactive)
  (cond
   ((null (one-window-p))
    (elscreen-message "current screen must not have two or more window!"))
   ((or (null (elscreen-get-previous-screen))
	(= (elscreen-get-number-of-screens) 1))
    (elscreen-message "must specify previous screen!"))
   ((and (elscreen-goto (elscreen-get-previous-screen))
	 (null (one-window-p)))
    (elscreen-goto (elscreen-get-previous-screen))
    (elscreen-message "previous screen must not have two or more window!"))
   (t
    (let ((elscreen-link-buffer (current-buffer)))
      (elscreen-kill)
      (switch-to-buffer-other-window elscreen-link-buffer)))))
   
(defun elscreen-split ()
  (interactive)
  (if (and (null (one-window-p))
	   (< (elscreen-get-number-of-screens) 10))
      (let ((elscreen-split-buffer (current-buffer)))
	(delete-window)
	(elscreen-create)
	(switch-to-buffer elscreen-split-buffer)
	(elscreen-goto (elscreen-get-previous-screen)))
    (elscreen-message "cannot split screen!")))
