;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-gf.el
;;
(defconst elscreen-gf-version "1.0 (7 January, 1997)")
;;
;; Author:   Naoto Morishima <naoto-m@is.aist-nara.ac.jp>
;;              Nara Institute of Science and Technology, Japan
;; Based on: grep-family.el
;;              by Youki Kadobayashi <youki-k@is.aist-nara.ac.jp>
;; Created:  23 June, 1996
;; Revised:  7 January, 1997

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

(provide 'elscreen-gf)

(require 'elscreen)
(require 'around-point)
(require 'alist)
(require 'directory-complete)

;
; user custamizable options
;
(defvar elscreen-gf-use-underline t
  "*If non-nil, underline is drawn on the selected entry in ElScreen-GF mode.")

(defvar elscreen-gf-mkid-always-recursive nil
  "*If non-nil, mkid always runs recursively.")

(defvar elscreen-gf-entry-overlay-property 'face
  "*After you jump to an entry, ElScreen-GF applies the overlay which this property specifies.")

(defvar elscreen-gf-entry-overlay-value 'highlight
  "*After you jump to an entry, ElScreen-GF applies the overlay which this value specifies.")

(defvar elscreen-gf-major-mode-alist
  '((c-mode . "*.[ch]")
    (emacs-lisp-mode . "*.el")
    (perl-mode . "*.pl"))
  "*Alist composed of the pairs of major-mode and regular expression of corresponding files.")

;
; variables
;
(defvar elscreen-gf-mode-name "ElScreen-GF"
  "*Mode name for ElScreen-GF.")

(defvar elscreen-gf-buffer-name "grep-family"
  "*Buffer name for grep-family.")

(defvar elscreen-gf-underline-overlay nil
  "*Overlay for selected entry.")

(defvar elscreen-gf-entry-overlay nil
  "*Overlay which is applied with corresponding line.")

(defvar elscreen-gf-truncate-lines t
  "*Lines of entries are truncated or not?")

(defvar elscreen-gf-running-process nil
  "*Running process.")

(defvar elscreen-gf-buffer-command nil
  "*Command assigned to the buffer.")

;
; key definition
;
(defvar elscreen-gf-map (make-sparse-keymap)
  "*Keymap for elscreen-gf.")

(define-key elscreen-map  "\C-g" 'elscreen-gf-command)
(fset 'elscreen-gf-command elscreen-gf-map)

(define-key elscreen-gf-map  "G" 'elscreen-gf-grep)
(define-key elscreen-gf-map  "g" 'elscreen-gf-gid)
(define-key elscreen-gf-map  "a" 'elscreen-gf-aid)
(define-key elscreen-gf-map  "m" 'elscreen-gf-mkid)
(define-key elscreen-gf-map  "v" 'elscreen-gf-show-version)

(defvar elscreen-gf-mode-map (make-sparse-keymap)
  "keymap used in elscreen-gf mode.")

(define-key elscreen-gf-mode-map "n"    'elscreen-gf-next-line)
(define-key elscreen-gf-mode-map "p"    'elscreen-gf-previous-line)
(define-key elscreen-gf-mode-map " "    'elscreen-gf-scroll-up)
(define-key elscreen-gf-mode-map "\177" 'elscreen-gf-scroll-down)
(define-key elscreen-gf-mode-map "<"    'elscreen-gf-beginning-of-buffer)
(define-key elscreen-gf-mode-map ">"    'elscreen-gf-end-of-buffer)
(define-key elscreen-gf-mode-map "t"    'elscreen-gf-toggle-truncate-lines)
(define-key elscreen-gf-mode-map "o"    'elscreen-gf-jump-to-entry)
(define-key elscreen-gf-mode-map "v"    'elscreen-gf-show-version)

;
; code
;
(defun elscreen-gf-mode (curdir)
  "Major mode for jumping to the entries.

Key bindings:
\\{elscreen-gf-mode-map}"
  (elscreen-get-screen-create elscreen-gf-buffer-name)

  (setq major-mode 'elscreen-gf-mode)
  (use-local-map elscreen-gf-mode-map)
  (auto-fill-mode nil)

  (setq buffer-read-only nil)
  (erase-buffer)
  (setq buffer-read-only t)

  (setq mode-name elscreen-gf-mode-name)
  (setq default-directory curdir)
  (setq truncate-lines elscreen-gf-truncate-lines))


(defun elscreen-gf-exclusive-p ()
  (if (processp elscreen-gf-running-process)
      (progn
	(message "Sorry, running %s now. Try again later."
		 (process-name elscreen-gf-running-process))
	nil)
    t))

(defun elscreen-gf-run-command (command)
  (message "running %s ..." elscreen-gf-buffer-command)
  (setq buffer-read-only nil)
  (insert shell-file-name "% cd " default-directory "\n"
	  shell-file-name "% " command "\n\n")
  (setq buffer-read-only t)
  (setq elscreen-gf-buffer-lines nil)
  (setq elscreen-gf-running-process
	(start-process elscreen-gf-buffer-command
		       (current-buffer)
		       "sh" "-c" command))
  (set-process-filter elscreen-gf-running-process
		      'elscreen-gf-filter)
  (set-process-sentinel elscreen-gf-running-process
			'elscreen-gf-sentinel))

(defun elscreen-gf-filter (process string)
  (save-excursion
    (set-buffer (process-buffer process))
    (save-excursion
      (let ((buffer-read-only nil))
	(goto-char (point-max))
	(insert string)))))

(defun elscreen-gf-sentinel (process event)
  (set-buffer (process-buffer process))
  (cond
   ((string-match "finished" event)
    (message "running %s ... done"
	     (process-name elscreen-gf-running-process)))
   (t
    (message "running %s ... error"
	     (process-name elscreen-gf-running-process))))
  (setq elscreen-gf-running-process nil)
  (setq elscreen-gf-buffer-lines (count-lines (point-min) (point-max)))
  (setq elscreen-gf-underline-overlay
	(elscreen-gf-overlay elscreen-gf-use-underline
			     elscreen-gf-underline-overlay
			     'face
			     'underline)))


(defun elscreen-gf-grep ()
  "Run grep, with user-specified args, and collect output in the ElScreen-GF
buffer."
  (interactive)
  (if (elscreen-gf-exclusive-p)
      (let ((grep-args
	     (concat (read-string
		      "run grep (pattern): "
		      (cons (symbol-around-point) 1))
		     " "
		     (read-string
		      "run grep (files): "
		      (cons (elscreen-gf-interesting-files-regexp) 1)))))
  (elscreen-gf-mode default-directory)
  (setq elscreen-gf-buffer-command "grep")
  (elscreen-gf-run-command (concat "grep -n " grep-args " /dev/null")))))


(defun elscreen-gf-gid ()
  "Run gid, with user-specified args, and collect output in the ElScreen-GF
buffer."
  (interactive)
  (if (elscreen-gf-exclusive-p)
      (let ((gid-args
	     (read-string "run gid (pattern): "
			  (cons (symbol-around-point) 1))))
	(elscreen-gf-mode default-directory)
	(setq elscreen-gf-buffer-command "gid")
	(elscreen-gf-run-command (concat "gid " gid-args)))))


(defun elscreen-gf-aid ()
  "Run aid, with user-specified args, and collect output in the ElScreen-GF
buffer."
  (interactive)
  (if (elscreen-gf-exclusive-p)
      (let ((aid-args
	     (read-string "run aid (pattern): "
			  (cons (symbol-around-point) 1))))
	(elscreen-gf-mode default-directory)
	(setq truncate-lines nil)
	(setq elscreen-gf-buffer-command "aid")
	(elscreen-gf-run-command (concat "aid " aid-args)))))


(defun elscreen-gf-mkid ()
  "Run mkid, with user-specified args. If elscree-gf-mkid-always-recursive
is non-nil, mkid always runs recursively."
  (interactive)
  (if (elscreen-gf-exclusive-p)
      (let ((mkid-command)
	    (default-directory
	      (read-directory-name "run mkid (target dir) : "
				   t
				   (cons default-directory 0)))
	    (target-files
	     (read-string "run mkid (target files) : "
			  (cons (elscreen-gf-interesting-files-regexp) 1))))
	(if (or elscreen-gf-mkid-always-recursive
		(y-or-n-p "run mkid : recursive? "))
	    (setq mkid-command
		  (concat "find " default-directory " -name '"
			  target-files
			  "' -print | mkid"))
	  (setq mkid-command
		(concat "mkid "
			target-files)))
	(message "making ID ...")
	(setq elscreen-gf-running-process
	      (start-process "mkid" nil "sh" "-c" mkid-command))
	(set-process-sentinel elscreen-gf-running-process
			      'elscreen-gf-mkid-sentinel))))

(defun elscreen-gf-mkid-sentinel (process event)
  (cond
   ((string-match "finished" event)
    (message "making ID ... done"))
   (t
    (message "making ID ... error")))
  (setq elscreen-gf-running-process nil))


(defun elscreen-gf-next-line ()
  "Move the current entry vertically down."
  (interactive)
  (let ((current-line))
    (save-excursion
      (end-of-line)
      (setq current-line (count-lines (point-min) (point))))
    (cond
     ((< current-line 4)
      (goto-line 4))
     ((null elscreen-gf-buffer-lines)
      (next-line 1))
     ((< current-line elscreen-gf-buffer-lines)
      (next-line 1)))
    (setq elscreen-gf-underline-overlay
	  (elscreen-gf-overlay elscreen-gf-use-underline
			       elscreen-gf-underline-overlay
			       'face
			       'underline))))

(defun elscreen-gf-previous-line ()
  "Move the current entry vertically up."
  (interactive)
  (let ((current-line))
    (save-excursion
      (end-of-line)
      (setq current-line (count-lines (point-min) (point))))
    (cond
     ((< 4 current-line)
      (previous-line 1))
     (t
      (goto-line 4)))
    (setq elscreen-gf-underline-overlay
	  (elscreen-gf-overlay elscreen-gf-use-underline
			       elscreen-gf-underline-overlay
			       'face
			       'underline))))


(defun elscreen-gf-scroll-up ()
  "Scroll entries upward full screen."
  (interactive)
  (scroll-up)
  (setq elscreen-gf-underline-overlay
	(elscreen-gf-overlay elscreen-gf-use-underline
			   elscreen-gf-underline-overlay
			   'face
			   'underline)))


(defun elscreen-gf-scroll-down ()
  "Scroll entries downward full screen."
  (interactive)
  (scroll-down)
  (setq elscreen-gf-underline-overlay
	(elscreen-gf-overlay elscreen-gf-use-underline
			   elscreen-gf-underline-overlay
			   'face
			   'underline)))

(defun elscreen-gf-beginning-of-buffer ()
  "Move the current entry to the beginning of the entries."
  (interactive)
  (goto-line 4)
  (setq elscreen-gf-underline-overlay
	(elscreen-gf-overlay elscreen-gf-use-underline
			   elscreen-gf-underline-overlay
			   'face
			   'underline)))

(defun elscreen-gf-end-of-buffer ()
  "Move the current entry to the end of the entries."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (setq elscreen-gf-underline-overlay
	(elscreen-gf-overlay elscreen-gf-use-underline
			   elscreen-gf-underline-overlay
			   'face
			   'underline)))


(defun elscreen-gf-toggle-truncate-lines ()
  "Toggle truncated lines."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter))


(defun elscreen-gf-jump-to-entry ()
  "Jump to the current entry."
  (interactive)
  (let ((current-line)
	(filename)
	(line-number))
    (save-excursion
      (end-of-line)
      (setq current-line (count-lines (point-min) (point))))
    (cond
     ((< current-line 4)
      (elscreen-gf-beginning-of-buffer))
     ((string-match "grep\\|gid" elscreen-gf-buffer-command)
      (save-excursion
	(beginning-of-line)
	(setq filename
	      (file-truename
	       (expand-file-name
		(buffer-substring (point) (- (search-forward ":") 1)))))
	(setq line-number
	      (string-to-number
	       (buffer-substring (point) (- (search-forward ":") 1)))))
      (elscreen-find-file filename)
      (goto-line line-number)
      (setq elscreen-gf-entry-overlay
	    (elscreen-gf-overlay t
				 elscreen-gf-entry-overlay
				 elscreen-gf-entry-overlay-property
				 elscreen-gf-entry-overlay-value))))))


(defun elscreen-gf-interesting-files-regexp ()
  (or (asget elscreen-gf-major-mode-alist major-mode)
      (concat "*" (and buffer-file-name
		       (string-match "\.[^.]+$" buffer-file-name)
		       (substring buffer-file-name
				  (match-beginning 0)
				  (match-end 0))))))

(defun elscreen-gf-overlay (flag overlay &optional prop value)
  (if flag
      (progn
	(if (overlayp overlay)
	    (move-overlay overlay
			  (save-excursion (beginning-of-line) (point))
			  (save-excursion (end-of-line) (point))
			  (current-buffer))
	  (setq overlay
		(make-overlay 
		 (save-excursion (beginning-of-line) (point))
		 (save-excursion (end-of-line) (point)))))
	(overlay-put overlay prop value)
	overlay)))


(elscreen-add-help "ElScreen-GF keys:
       \\[elscreen-gf-grep]    run grep
       \\[elscreen-gf-gid]    run gid
       \\[elscreen-gf-aid]    run aid
       \\[elscreen-gf-mkid]    run mkid
       \\[elscreen-gf-show-version]    show ElScreen-GF version")

(defun elscreen-gf-show-version ()
  "Show ElScreen-GF version."
  (interactive)
  (elscreen-message (concat "ElScreen-GF version " elscreen-gf-version)))
