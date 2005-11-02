;; -*- Mode: Emacs-Lisp -*-
;;
;; directory-complete.el
;;
;; Author:   Naoto Morishima <naoto-m@is.aist-nara.ac.jp>
;;              Nara Institute of Science and Technology, Japan
;; Created:  17 November, 1996
;; Version:  0.1

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

(provide 'directory-complete)

;
; code
;
(defun read-directory-name (prompt &optional existing initial)
  (completing-read prompt
		   (function directory-name-completion)
		   nil
		   existing
		   initial))

(defun directory-name-completion (string predicate flag)
  (let* ((directory-alist nil)
	 (directory (or (file-name-directory string) ""))
	 (input (or (file-name-nondirectory string) ""))
	 (files)
	 (completed))
    (cond
     ((null (file-directory-p directory))
      nil)
     (t
      (setq files (directory-files directory))
      (mapcar (function
	       (lambda (s)
		 (if (file-directory-p (concat directory s))
		     (setq directory-alist
			   (nconc directory-alist
				  (list (cons (file-name-as-directory s)
					      (file-name-as-directory s))))))))
	      files)
      (cond
       (flag
	(all-completions input directory-alist predicate))
       (t
	(setq completed (try-completion input directory-alist predicate))
	(cond
	 ((eq (length directory-alist) 2)
	  t)
	 ((stringp completed)
	  (concat directory completed))
	 (t
	  completed))))))))
