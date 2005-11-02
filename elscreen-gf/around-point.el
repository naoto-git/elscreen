;;; around-point --- handy functions when asking user for function name etc.
;; Maintainer: Youki Kadobayashi <youki-k@is.aist-nara.ac.jp>
;;	Osaka University / Nara Institute of Science and Technology, Japan
;; Created: August, 1994
;; Version: 1.0
;; Keywords: minibuffer
;; Mostly stolen from:
;;	symfunc.el (by the author of mkid?)
;;	~/as-is/unix.el by Wolfgang Rupprecht <wolfgang@mgm.mit.edu>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'around-point)

(defun word-around-point ()
  "Return the word around the point as a string."
  (save-excursion
    (let (beg)
      (if (not (looking-at "\\<"))
	  (forward-word -1))
      (setq beg (point))
      (forward-word 1)
      (buffer-substring beg (point)))))

(defun symbol-around-point ()
  "Return the symbol around the point as a string."
  (save-excursion
    (let ((match-data (match-data)))
      (unwind-protect
	  (if (not (looking-at "\\sw\\|\\s_")) ; if not in a symbol
	      (re-search-backward "\\sw\\|\\s_" nil t)) ; go into prev. one
	(store-match-data match-data)))
    (buffer-substring (progn (forward-sexp 1) (point))
		      (progn (backward-sexp 1) (point)))))
