;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen.el 
;;
(defconst elscreen-version "1.02 (January 12, 1997)")
;;
;; Author:   Naoto Morishima <naoto-m@is.aist-nara.ac.jp>
;;              Nara Institute of Science and Technology, Japan
;; Based on: screens.el 
;;              by Heikki T. Suopanki <suopanki@stekt1.oulu.fi>
;; Created:  June 22, 1996
;; Revised:  January 12, 1997

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

;
; user custamizable options
;
(defvar elscreen-prefix-key "\C-z"
  "*ElScreen command prefix-key.")

(defvar elscreen-show-screen-number t
  "*If non-nil, show the number of the current screen in the mode line.")

(defvar elscreen-buffer-to-screen-alist
  '(("inbox\\|reply to\\|\\+draft\\|Mew" . "Mew")
    ("Article\\|Subject\\|Newsgroup\\|Group\\|Summary" . "Gnus")
    ("[Ss]hell" . "shell")
    ("compilation" . "compile")
    ("-telnet" . "telnet")
    ("IRC" . "IRC")
    ("dict" . "OnlineDict"))
  "*Alist composed of the pair of regular expression of buffer-name and corresponding screen-name.")

(defvar elscreen-mode-to-screen-alist
  '(("Dired" . "Dired")
    ("Info" . "Info"))
  "*Alist composed of the pair of mode-name and corresponding screen-name.")

;
; variables
;
(defvar elscreen-mode-line (and elscreen-show-screen-number "[0] ")
  "*Shows the screen number in the mode line.") 

(setq global-mode-string
      (cond
       ((not (listp global-mode-string))
	(list "" 'elscreen-mode-line global-mode-string))
       ((not (memq 'elscreen-mode-line global-mode-string))
	(append '("" elscreen-mode-line) global-mode-string))
       (t
	global-mode-string)))

(defvar elscreen-confs-alist (list (cons 0 (current-window-configuration)))
  "*Alist that contains the information about screen configurations.")

(defvar elscreen-name-alist nil
  "*List of window name specified by user.")

(defvar elscreen-open-screens 1
  "*How many screens are used.")

(defvar elscreen-scratch-buffer "*scratch*"
  "*Buffer name of scratch-buffer.")

(defvar elscreen-last-message "Welcome to ElScreen!"
  "*Last shown message.")

(defvar elscreen-this-screen 0
  "*Number of the current screen.")

(defvar elscreen-previous-screen nil
  "*Number of the previous screen.")

(defvar elscreen-help "ElScreen keys:
       \\[elscreen-create]    Create a new screen         
       \\[elscreen-kill]    Kill the current screen
       \\[elscreen-previous]    Switch to the previous screen
       \\[elscreen-next]    Switch to the next screen
       \\[elscreen-toggle]    Toggle screen
       \\[elscreen-goto]    Jump to the specified screen
       \\[elscreen-jump-0]  
         :      Jump to the screen #
       \\[elscreen-jump-9]      
       \\[elscreen-show-list]    Show list of screens
       \\[elscreen-name]    Name the current screen
       \\[elscreen-show-last-message]    Show last message
       \\[elscreen-show-time]    Show time
       \\[elscreen-show-version]    Show ElScreen version
       \\[elscreen-find-file]    Create new screen and open file
       \\[elscreen-number-mode-line]    Show/hide the screen number in the mode line
       \\[elscreen-help]    Show this help"
  "*Help shown by elscreen-help-mode")

;
; key definition
;
(defvar elscreen-map (make-sparse-keymap)
  "*Keymap for ElScreen.")
(global-set-key elscreen-prefix-key elscreen-map)

(define-key elscreen-map  "\C-c" 'elscreen-create)
(define-key elscreen-map  "c"    'elscreen-create)
(define-key elscreen-map  "k"    'elscreen-kill)
(define-key elscreen-map  "\C-p" 'elscreen-previous)
(define-key elscreen-map  "p"    'elscreen-previous)
(define-key elscreen-map  "\C-n" 'elscreen-next)
(define-key elscreen-map  "n"    'elscreen-next)
(define-key elscreen-map  "\C-a" 'elscreen-toggle)
(define-key elscreen-map  "a"    'elscreen-toggle)
(define-key elscreen-map  "g"    'elscreen-goto)
(define-key elscreen-map  "0"    'elscreen-jump-0)
(define-key elscreen-map  "1"    'elscreen-jump-1)
(define-key elscreen-map  "2"    'elscreen-jump-2)
(define-key elscreen-map  "3"    'elscreen-jump-3)
(define-key elscreen-map  "4"    'elscreen-jump-4)
(define-key elscreen-map  "5"    'elscreen-jump-5)
(define-key elscreen-map  "6"    'elscreen-jump-6)
(define-key elscreen-map  "7"    'elscreen-jump-7)
(define-key elscreen-map  "8"    'elscreen-jump-8)
(define-key elscreen-map  "9"    'elscreen-jump-9)
(define-key elscreen-map  "?"    'elscreen-help)
(define-key elscreen-map  "\C-f" 'elscreen-find-file)
(define-key elscreen-map  "w"    'elscreen-show-list)
(define-key elscreen-map  "m"    'elscreen-show-last-message)
(define-key elscreen-map  "t"    'elscreen-show-time)
(define-key elscreen-map  "A"    'elscreen-name)
(define-key elscreen-map  "v"    'elscreen-show-version)
(define-key elscreen-map  "i"    'elscreen-number-mode-line)
(define-key elscreen-map  "l"    'elscreen-link)
(define-key elscreen-map  "s"    'elscreen-split)

(add-hook 'minibuffer-setup-hook 'elscreen-minibuffer-setup)
(add-hook 'minibuffer-exit-hook 'elscreen-minibuffer-exit)

(defun elscreen-minibuffer-setup ()
  "Disable elscreen-prefix-key when minibuffer become active."
  (global-set-key elscreen-prefix-key 'elscreen-minibuffer-message))

(defun elscreen-minibuffer-exit ()
  "Enable elscreen-prefix-key when minibuffer become inactive."
  (global-set-key elscreen-prefix-key elscreen-map))

;
; code
;
(defun elscreen-minibuffer-message ()
  "Show message when minibuffer is active."
  (interactive)
  (elscreen-message "Sorry, minibuffer is active..."))

(defun elscreen-create () 
  "Create a new screen."
  (interactive)
      (cond
       ((>= elscreen-open-screens 10)
	(elscreen-message "Can't create any more screen"))
       (t
	(asput 'elscreen-confs-alist
	      elscreen-this-screen
	      (current-window-configuration))
	(setq elscreen-open-screens (+ elscreen-open-screens 1))
	(let ((i 0)
	      (flag t))
	  (while (and flag (< i 9))
	    (if (not (asget elscreen-confs-alist i))
		(progn
		  (setq elscreen-previous-screen elscreen-this-screen)
		  (setq flag nil))
	      (setq i (+ i 1))))
	  (setq elscreen-this-screen i))
	(delete-other-windows) 
	(switch-to-buffer elscreen-scratch-buffer)
	(lisp-interaction-mode)
	(elscreen-mode-line-update)
	(asput 'elscreen-confs-alist
	      elscreen-this-screen
	      (current-window-configuration)))))

(defun elscreen-kill () 
  "Kill the current screen."
  (interactive)
  (cond
   ((= elscreen-open-screens 1)
    (elscreen-message "There is only one screen, can't kill"))
   (t 
    (setq elscreen-open-screens (- elscreen-open-screens 1))
    (asdelete 'elscreen-confs-alist elscreen-this-screen)
    (if (asget elscreen-name-alist elscreen-this-screen)
	(asdelete 'elscreen-name-alist elscreen-this-screen))
    (let ((i 0)
	  (flag t)
	  (elscreen-next-screen))
      (if elscreen-previous-screen
	  (setq elscreen-next-screen elscreen-previous-screen)
	(setq elscreen-next-screen elscreen-this-screen)
	(while (and flag (< i 9))
	  (setq elscreen-next-screen (+ elscreen-next-screen 1))
	  (if (< 9 elscreen-next-screen)
	      (setq elscreen-next-screen 0))
	  (if (asget elscreen-confs-alist elscreen-next-screen)
	      (setq flag nil)
	    (setq i (+ i 1)))))
      (setq elscreen-this-screen nil)
      (elscreen-goto elscreen-next-screen)))))

(defun elscreen-goto (elscreen-next-screen) 
  "Jump to the specified screen."
  (interactive "nGoto screen number:")
  (if (asget elscreen-confs-alist elscreen-next-screen)
      (progn
	(if elscreen-this-screen
	    (asput 'elscreen-confs-alist
		  elscreen-this-screen
		  (current-window-configuration)))
	(setq elscreen-previous-screen elscreen-this-screen)
	(setq elscreen-this-screen elscreen-next-screen)
	(delete-other-windows)
	(switch-to-buffer elscreen-scratch-buffer)
	(set-window-configuration
	 (asget elscreen-confs-alist elscreen-this-screen))
	(elscreen-mode-line-update))
    (elscreen-message (concat "No screen " elscreen-next-screen))))

(defun elscreen-next () 
  "Switch to the next screen."
  (interactive)
  (asput 'elscreen-confs-alist
	elscreen-this-screen
	(current-window-configuration))
  (let ((i 0)
	(flag t)
	(elscreen-next-screen elscreen-this-screen))
    (while (and flag (< i 10))
      (setq elscreen-next-screen (+ elscreen-next-screen 1))
      (if (< 9 elscreen-next-screen)
	  (setq elscreen-next-screen 0))
      (if (asget elscreen-confs-alist elscreen-next-screen)
	  (setq flag nil))
      (setq i (+ i 1)))
    (cond
     ((eq elscreen-next-screen elscreen-this-screen)
      (elscreen-message 
       (concat "You connot escape from screen " elscreen-this-screen "!")))
     (t
      (elscreen-goto elscreen-next-screen)))))

(defun elscreen-previous () 
  "Switch to the previous screen."
  (interactive)
  (asput 'elscreen-confs-alist
	elscreen-this-screen
	(current-window-configuration))
  (let ((i 0)
	(flag t)
	(elscreen-next-screen elscreen-this-screen))
    (while (and flag (< i 10))
      (setq i (+ i 1))
      (setq elscreen-next-screen (- elscreen-next-screen 1))
      (if (< elscreen-next-screen 0)
	  (setq elscreen-next-screen 9))
      (if (asget elscreen-confs-alist elscreen-next-screen)
	  (setq flag nil)))
    (cond
     ((eq elscreen-next-screen elscreen-this-screen)
      (elscreen-message 
       (concat "You connot escape from screen " elscreen-this-screen "!")))
     (t
      (elscreen-goto elscreen-next-screen)))))

(defun elscreen-toggle ()
  "Toggle to last screen."
    (interactive)
    (cond
     ((= elscreen-open-screens 1)
      (elscreen-message 
       (concat "You connot escape from screen " elscreen-this-screen "!")))
     (elscreen-previous-screen
      (elscreen-goto elscreen-previous-screen))
     (t
      (let ((i 0)
	    (flag t)
	    (elscreen-next-screen elscreen-this-screen))
	(while (and flag (< i 10))
	  (setq elscreen-next-screen (+ elscreen-next-screen 1))
	  (if (< 9 elscreen-next-screen)
	      (setq elscreen-next-screen 0))
	  (if (asget elscreen-confs-alist elscreen-next-screen)
	      (setq flag nil))
	  (setq i (+ i 1)))
	(elscreen-goto elscreen-next-screen)))))
      
(defun elscreen-jump-0 ()
  "Switch to the screen 0."
  (interactive)
  (elscreen-goto 0))

(defun elscreen-jump-1 ()
  "Switch to the screen 1."
  (interactive)
  (elscreen-goto 1))

(defun elscreen-jump-2 ()
  "Switch to the screen 2."
  (interactive)
  (elscreen-goto 2))

(defun elscreen-jump-3 ()
  "Switch to the screen 3."
  (interactive)
  (elscreen-goto 3))

(defun elscreen-jump-4 ()
  "Switch to the screen 4."
  (interactive)
  (elscreen-goto 4))

(defun elscreen-jump-5 ()
  "Switch to the screen 5."
  (interactive)
  (elscreen-goto 5))

(defun elscreen-jump-6 ()
  "Switch to the screen 6."
  (interactive)
  (elscreen-goto 6))

(defun elscreen-jump-7 ()
  "Switch to the screen 7."
  (interactive)
  (elscreen-goto 7))

(defun elscreen-jump-8 ()
  "Switch to the screen 8."
  (interactive)
  (elscreen-goto 8))

(defun elscreen-jump-9 ()
  "Switch to the screen 9."
  (interactive)
  (elscreen-goto 9))


(defun elscreen-number-mode-line ()
  "Toggle the screen number in the mode line."
  (interactive)
  (setq elscreen-show-screen-number (null elscreen-show-screen-number))
  (elscreen-mode-line-update)
  (switch-to-buffer (current-buffer)))


(defun elscreen-show-list ()
  "Show list of screens."
  (interactive)
  (let ((elscreen-tmp-this-screen elscreen-this-screen)
	(elscreen-tmp-previous-screen elscreen-previous-screen)
	(elscreen-list-message nil)
	(elscreen-tmp-name-alist nil)
	(elscreen-buffer-names nil)
	(elscreen-mode-names nil)
	(i 0)
	(j)
	(flag))
    (while (< i 10)
      (if (asget elscreen-confs-alist i)
	  (progn
	    (elscreen-goto i)
	    (setq elscreen-buffer-names (elscreen-get-buffer-list))
	    (setq elscreen-mode-names (elscreen-get-mode-list))
	    (setq flag t)
	    (or
	     (progn ; examine buffer name
	       (setq j 0)
	       (while (and flag (< j (length elscreen-buffer-to-screen-alist)))
		 (if (string-match
		      (car (nth j elscreen-buffer-to-screen-alist))
		      elscreen-buffer-names)
		     (progn
		       (asput 'elscreen-tmp-name-alist i
			     (cdr (nth j elscreen-buffer-to-screen-alist)))
		       (setq flag nil)))
		 (setq j (+ j 1)))
	       (null flag))
	     (progn ; buffer-name didn't give it, so let's try mode-name
	       (setq j 0)
	       (while (and flag (< j (length elscreen-mode-to-screen-alist)))
		 (if (string-match
		      (car (nth j elscreen-mode-to-screen-alist))
		      elscreen-mode-names)
		     (progn
		       (asput 'elscreen-tmp-name-alist i
			     (cdr (nth j elscreen-mode-to-screen-alist)))
		       (setq flag nil)))
		 (setq j (+ j 1)))
	       (null flag))
	     ; neither buffer-name and mode-name didn't give it...
	     (asput 'elscreen-tmp-name-alist i elscreen-buffer-names))))
      (setq i (+ i 1)))
	     ; making elscreen-list-message is completed.
    (elscreen-goto elscreen-tmp-this-screen)
    (setq elscreen-previous-screen elscreen-tmp-previous-screen)
    (setq i 0)
    (while (< i 10)
      (if (asget elscreen-confs-alist i)
	  (setq elscreen-list-message
		(concat elscreen-list-message i 
			(or (and (eq i elscreen-this-screen) "+")
			    (and (eq i elscreen-previous-screen) "-"))
			" "
			(or (asget elscreen-name-alist i)
			    (asget elscreen-tmp-name-alist i))
			"  ")))
      (setq i (+ i 1)))
    (elscreen-message elscreen-list-message)))

(defun elscreen-get-buffer-list ()
  (let ((org-window (selected-window))
	(buffer-list (buffer-name)))
    (while (progn (other-window 1)
		  (not (eq org-window (selected-window))))
      (setq buffer-list (concat buffer-list ":" (buffer-name))))
    buffer-list))

(defun elscreen-get-mode-list ()
  (let ((org-window (selected-window))
	(mode-list mode-name))
    (while (progn (other-window 1)
		  (not (eq org-window (selected-window))))
      (setq mode-list (concat mode-list ":" mode-name)))
    mode-list))


(defun elscreen-name (new-name)
  "Specify screen name."
  (interactive "sSet window's title to: ")
  (asput 'elscreen-name-alist elscreen-this-screen new-name)
  (if (eq (length new-name) 0)
      (asdelete 'elscreen-name-alist elscreen-this-screen)))


(defun elscreen-find-file (filename)
  "Edit file FILENAME.
Switch to a screen visiting file FILENAME,
creating one if none already exists."
  (interactive "FFind file in new screen: ")
  (let ((elscreen-tmp-this-screen elscreen-this-screen)
	(elscreen-tmp-previous-screen elscreen-previous-screen)
	(elscreen-target-screen nil)
	(flag t)
	(i 0))
    (while (and flag (< i 10))
      (if (and (asget elscreen-confs-alist i)
	       (elscreen-goto i))
	  (if (member
	       (file-truename (expand-file-name filename))
	       (elscreen-get-ab-file-list))
	      (progn
		(setq elscreen-target-screen i)
		(setq flag nil))))
      (setq i (+ i 1)))
    (if elscreen-target-screen
	(progn
	  (elscreen-goto elscreen-target-screen)
	  (select-window
	   (get-buffer-window (file-name-nondirectory filename))))
      (if (< elscreen-open-screens 10)
	  (progn
	    (elscreen-create)
	    (find-file filename))
	(elscreen-goto elscreen-tmp-this-screen)
	(find-file-other-window filename)
	(setq elscreen-tmp-this-screen elscreen-tmp-previous-screen)))
    (setq elscreen-previous-screen elscreen-tmp-this-screen)))

(defun elscreen-get-ab-file-list ()
  (let ((org-window (selected-window))
	(ab-file-list (list (and (buffer-file-name)
				 (file-truename (buffer-file-name))))))
    (while (progn (other-window 1)
		  (not (eq org-window (selected-window))))
      (setq ab-file-list (nconc ab-file-list
				(list (and (buffer-file-name)
					   (file-truename
					    (buffer-file-name)))))))
    ab-file-list))


(defun elscreen-get-screen-create (name)
  (let ((elscreen-tmp-this-screen elscreen-this-screen)
	(elscreen-tmp-previous-screen elscreen-previous-screen)
	(elscreen-target-screen nil)
	(flag t)
	(i 0))
    (while (and flag (< i 10))
      (if (and (asget elscreen-confs-alist i)
	       (elscreen-goto i))
	  (if (string-match
	       name
	       (elscreen-get-buffer-list))
	      (progn
		(setq elscreen-target-screen i)
		(setq flag nil))))
      (setq i (+ i 1)))
    (if elscreen-target-screen
	(progn
	  (elscreen-goto elscreen-target-screen)
	  (set-buffer (get-buffer name)))
      (if (< elscreen-open-screens 10)
	  (progn
	    (elscreen-create)
	    (switch-to-buffer name))
	(elscreen-goto elscreen-tmp-this-screen)
	(setq elscreen-tmp-this-screen elscreen-tmp-previous-screen)
	(switch-to-buffer-other-window name)))
    (setq elscreen-previous-screen elscreen-tmp-this-screen)))


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


(defun elscreen-show-last-message ()
  "Show last message."
  (interactive)
  (elscreen-message elscreen-last-message 5))

(defun elscreen-show-time ()
  "Show time."
  (interactive)
  (elscreen-message
   (concat (current-time-string) " " (nth 1 (current-time-zone))) 3))

(defun elscreen-show-version ()
  "Show ElScreen version."
  (interactive)
  (elscreen-message (concat "ElScreen version " elscreen-version)))


(defun elscreen-mode-line-update () 
  (setq elscreen-mode-line (and elscreen-show-screen-number
				(concat "[" elscreen-this-screen "] "))))

(defun elscreen-message (message &optional sec)
  (setq elscreen-last-message message)
  (message message)
  (sit-for (or sec 3))
  (message nil))

;
; not supported
;
(defun elscreen-link ()
  (interactive)
  (cond
   ((null (one-window-p))
    (elscreen-message "current screen must not have two or more window!"))
   ((or (null elscreen-previous-screen)
	(= elscreen-open-screens 1))
    (elscreen-message "must specify previous screen!"))
   ((and (elscreen-goto elscreen-previous-screen)
	 (null (one-window-p)))
    (elscreen-goto elscreen-previous-screen)
    (elscreen-message "previous screen must not have two or more window!"))
   (t
    (let ((elscreen-link-buffer (current-buffer)))
      (elscreen-kill)
      (switch-to-buffer-other-window elscreen-link-buffer)))))
   
(defun elscreen-split ()
  (interactive)
  (if (and (null (one-window-p))
	   (< elscreen-open-screens 10))
      (let ((elscreen-split-buffer (current-buffer)))
	(delete-window)
	(elscreen-create)
	(switch-to-buffer elscreen-split-buffer)
	(elscreen-goto elscreen-previous-screen))
    (elscreen-message "cannot split screen!")))
