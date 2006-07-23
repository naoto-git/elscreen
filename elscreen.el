;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen.el
;;
(defconst elscreen-version "1.4.3.7 (July 12, 2006)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;; Based on: screens.el
;;              by Heikki T. Suopanki <suopanki@stekt1.oulu.fi>
;; Created:  June 22, 1996
;; Revised:  July 12, 2006

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
(eval-when-compile
  (require 'static))

(static-defconst elscreen-on-xemacs (featurep 'xemacs))
(static-defconst elscreen-on-emacs (and (not elscreen-on-xemacs)
                                        (>= emacs-major-version 21)))


;;; User Customizable Variables:

(defgroup elscreen nil
  "ElScreen -- Screen Manager for Emacsen"
  :tag "ElScreen"
  :group 'environment)

(defcustom elscreen-prefix-key "\C-z"
  "Prefix key for ElScreen commands."
  :type '(string :size 10)
  :tag "Prefix-key"
  :set (lambda (symbol value)
         (when (boundp 'elscreen-map)
           (elscreen-set-prefix-key value))
         (custom-set-default symbol value))
  :group 'elscreen)

(defcustom elscreen-display-screen-number t
  "Non-nil to display the number of current screen in the mode line."
  :type 'boolean
  :tag "Show screen number"
  :group 'elscreen)

(defcustom elscreen-default-buffer-name "*scratch*"
  "Name of a buffer in new screen."
  :type '(string :size 24)
  :tag "Default buffer name"
  :group 'elscreen)

(defcustom elscreen-default-buffer-initial-major-mode initial-major-mode
  "Major mode command symbol to use for the default buffer."
  :type 'function
  :group 'elscreen)

(defcustom elscreen-default-buffer-initial-message initial-scratch-message
  "Initial message displayed in default buffer.
If this is nil, no message will be displayed."
  :type '(choice (text :tag "Message")
                 (const :tag "none" nil))
  :group 'elscreen)

(defcustom elscreen-mode-to-nickname-alist
  '(("^dired-mode$" .
     (lambda ()
       (format "Dired(%s)" dired-directory)))
    ("^Info-mode$" .
     (lambda ()
       (format "Info(%s)" (file-name-nondirectory Info-current-file))))
    ("^mew-draft-mode$" .
     (lambda ()
       (format "Mew(%s)" (buffer-name (current-buffer)))))
    ("^mew-" . "Mew")
    ("^irchat-" . "IRChat")
    ("^liece-" . "Liece")
    ("^lookup-" . "Lookup"))
  "Alist composed of the pair of mode-name and corresponding screen-name."
  :type '(alist :key-type string :value-type (choice string function))
  :set (lambda (symbol value)
         (custom-set-default symbol value)
         (when (fboundp 'elscreen-rebuild-mode-to-nickname-alist)
           (elscreen-rebuild-mode-to-nickname-alist)))
  :group 'elscreen)

(defcustom elscreen-buffer-to-nickname-alist
  '(("[Ss]hell" . "shell")
    ("compilation" . "compile")
    ("-telnet" . "telnet")
    ("dict" . "OnlineDict")
    ("*WL:Message*" . "Wanderlust"))
  "Alist composed of the pair of regular expression of
buffer-name and corresponding screen-name."
  :type '(alist :key-type string :value-type (choice string function))
  :set (lambda (symbol value)
         (custom-set-default symbol value)
         (when (fboundp 'elscreen-rebuild-buffer-to-nickname-alist)
           (elscreen-rebuild-buffer-to-nickname-alist)))
  :group 'elscreen)

(defcustom elscreen-startup-command-line-processing t
  "If non-nil, ElScreen processes command line when Emacsen
starts up, and opens files with new screen if needed."
  :type 'boolean
  :group 'elscreen)

(static-when elscreen-on-emacs ;; GNU Emacs 21
  (defvar elscreen-display-tab-set-nil-hook nil)
  (defcustom elscreen-display-tab t
    "Non-nil to display the tabs at the top of screen."
    :type 'boolean
    :set (lambda (symbol value)
           (custom-set-default symbol value)
           (when (fboundp 'elscreen-e21-tab-update)
             (elscreen-e21-tab-update t)
             (run-hooks 'elscreen-display-tab-set-nil-hook)))
    :group 'elscreen)

  (defcustom elscreen-tab-width 16
    "Tab width (should be equal or greater than 6)."
    :type '(integer :size 4)
    :set (lambda (symbol value)
           (when (and (numberp value)
                      (>= value 6))
             (custom-set-default symbol value)
             (when (fboundp 'elscreen-e21-tab-update)
               (elscreen-e21-tab-update t))))
    :group 'elscreen)

  (make-obsolete-variable 'elscreen-tab-display-create-screen
                          'elscreen-tab-display-control)
  (defcustom elscreen-tab-display-control t
    "Non-nil to display control tab at the most left side."
    :type 'boolean
    :set (lambda (symbol value)
           (custom-set-default symbol value)
           (when (fboundp 'elscreen-e21-tab-update)
             (elscreen-e21-tab-update t)))
    :group 'elscreen)

  (defcustom elscreen-tab-display-kill-screen t
    "Non-nil to display the icons to kill a screen at left side of each tab."
    :type 'boolean
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
    "Face to fontify background of tab line."
    :group 'elscreen)

  (defface elscreen-tab-control-face
    '((((type x w32 mac) (class color))
       (:background "white" :foreground "black" :underline "Gray50"))
      (((class color))
       (:background "white" :foreground "black" :underline t))
      (t (:underline t)))
    "Face for control tab."
    :group 'elscreen)

  (defface elscreen-tab-current-screen-face
    '((((class color))
       (:background "white" :foreground "black"))
      (t (:underline t)))
    "Face for current screen tab."
    :group 'elscreen)

  (defface elscreen-tab-other-screen-face
    '((((type x w32 mac) (class color))
       :background "Gray85" :foreground "Gray50")
      (((class color))
       (:background "blue" :foreground "black" :underline t)))
    "Face for tabs other than current screen one."
    :group 'elscreen)
  )

;;; Key & Menu bindings:

(defvar elscreen-map (make-sparse-keymap)
  "*Keymap for ElScreen.")
(define-key elscreen-map "\C-c" 'elscreen-create)
(define-key elscreen-map "c"    'elscreen-create)
(define-key elscreen-map "C"    'elscreen-clone)
(define-key elscreen-map "\C-k" 'elscreen-kill)
(define-key elscreen-map "k"    'elscreen-kill)
(define-key elscreen-map "\M-k" 'elscreen-kill-screen-and-buffers)
(define-key elscreen-map "K"    'elscreen-kill-others)
(define-key elscreen-map "\C-p" 'elscreen-previous)
(define-key elscreen-map "p"    'elscreen-previous)
(define-key elscreen-map "\C-n" 'elscreen-next)
(define-key elscreen-map "n"    'elscreen-next)
(define-key elscreen-map "\C-a" 'elscreen-toggle)
(define-key elscreen-map "a"    'elscreen-toggle)
(define-key elscreen-map "'"    'elscreen-goto)
(define-key elscreen-map "\""   'elscreen-select-and-goto)
(define-key elscreen-map "0"    'elscreen-jump-0)
(define-key elscreen-map "1"    'elscreen-jump)
(define-key elscreen-map "2"    'elscreen-jump)
(define-key elscreen-map "3"    'elscreen-jump)
(define-key elscreen-map "4"    'elscreen-jump)
(define-key elscreen-map "5"    'elscreen-jump)
(define-key elscreen-map "6"    'elscreen-jump)
(define-key elscreen-map "7"    'elscreen-jump)
(define-key elscreen-map "8"    'elscreen-jump)
(define-key elscreen-map "9"    'elscreen-jump-9)
(define-key elscreen-map "\C-s" 'elscreen-swap)
(define-key elscreen-map "\C-w" 'elscreen-display-screen-name-list)
(define-key elscreen-map "w"    'elscreen-display-screen-name-list)
(define-key elscreen-map "\C-m" 'elscreen-display-last-message)
(define-key elscreen-map "m"    'elscreen-display-last-message)
(define-key elscreen-map "\C-t" 'elscreen-display-time)
(define-key elscreen-map "t"    'elscreen-display-time)
(define-key elscreen-map "A"    'elscreen-screen-nickname)
(define-key elscreen-map "b"    'elscreen-find-and-goto-by-buffer)
(define-key elscreen-map "\C-f" 'elscreen-find-file)
(define-key elscreen-map "\C-r" 'elscreen-find-file-read-only)
(define-key elscreen-map "d"    'elscreen-dired)
(define-key elscreen-map "\M-x" 'elscreen-execute-extended-command)
(define-key elscreen-map "i"    'elscreen-display-screen-number-toggle)
(define-key elscreen-map "?"    'elscreen-help)
(define-key elscreen-map "v"    'elscreen-display-version)
(define-key elscreen-map "j"    'elscreen-link)
(define-key elscreen-map "s"    'elscreen-split)

(defun elscreen-set-prefix-key (prefix-key)
  (when (not (eq elscreen-prefix-key prefix-key))
    (when elscreen-prefix-key
      (global-set-key elscreen-prefix-key
                      (get 'elscreen-prefix-key
                           'global-map-original-definition))
      (define-key minibuffer-local-map elscreen-prefix-key
        (get 'elscreen-prefix-key 'minibuffer-local-map-original-definition)))
    (put 'elscreen-prefix-key 'global-map-original-definition
         (lookup-key global-map prefix-key))
    (put 'elscreen-prefix-key 'minibuffer-local-map-original-definition
         (lookup-key minibuffer-local-map prefix-key)))
  (global-set-key prefix-key elscreen-map)
  (define-key minibuffer-local-map prefix-key 'undefined)
  (setq elscreen-prefix-key prefix-key))

(defvar elscreen-help "ElScreen keys:
  \\[elscreen-create]    Create a new screen and switch to it
  \\[elscreen-clone]    Create a new screen with the window-configuration of current screen
  \\[elscreen-kill]    Kill current screen
  \\[elscreen-kill-screen-and-buffers]  Kill current screen and buffers
  \\[elscreen-kill-others]    Kill other screens
  \\[elscreen-next]    Switch to the \"next\" screen in a cyclic order
  \\[elscreen-previous]    Switch to the \"previous\" screen in a cyclic order
  \\[elscreen-toggle]    Toggle to the screen selected previously
  \\[elscreen-select-and-goto]    Jump to the specified screen
  \\[elscreen-jump-0]
    :      Jump to the screen #
  \\[elscreen-jump-9]
  \\[elscreen-swap]  Swap current screen with previous one
  \\[elscreen-display-screen-name-list]    Show list of screens
  \\[elscreen-screen-nickname]    Name current screen
  \\[elscreen-display-last-message]    Show last message
  \\[elscreen-display-time]    Show time
  \\[elscreen-find-and-goto-by-buffer]    Switch to the screen in which specified buffer is displayed
  \\[elscreen-find-file]  Create new screen and open file
  \\[elscreen-find-file-read-only]  Create new screen and open file but don't allow changes
  \\[elscreen-dired]    Create new screen and run dired
  \\[elscreen-execute-extended-command]  Read function name, then call it with new screen
  \\[elscreen-display-screen-number-toggle]    Show/hide the screen number in the mode line
  \\[elscreen-display-version]    Show ElScreen version
  \\[elscreen-help]    Show this help"
  "*Help shown by elscreen-help-mode")


;;; Code:

(defvar elscreen-frame-confs nil
  "*Alist that contains the information about screen configurations.")

(defun elscreen-current-window-configuration ()
  (list (current-window-configuration) (point-marker)))

(defun elscreen-apply-window-configuration (elscreen-window-configuration)
  (let ((window-configuration (car elscreen-window-configuration))
        (marker (cadr elscreen-window-configuration)))
    (set-window-configuration window-configuration)
    (and (marker-buffer marker) (goto-char marker))))

(defun elscreen-default-window-configuration ()
  (let ((default-buffer (get-buffer elscreen-default-buffer-name)))
    (save-window-excursion
      (delete-other-windows)
      (if default-buffer
          (switch-to-buffer default-buffer)
        (switch-to-buffer (get-buffer-create elscreen-default-buffer-name))
        (funcall elscreen-default-buffer-initial-major-mode)
        (insert elscreen-default-buffer-initial-message)
        (set-buffer-modified-p nil))
      (elscreen-current-window-configuration))))

(defun get-alist (key alist)
  (cdr (assoc key alist)))

(defsubst elscreen-get-frame-confs (frame)
  (get-alist frame elscreen-frame-confs))

(defun elscreen-make-frame-confs (frame &optional keep-window-configuration)
  (when (null (elscreen-get-frame-confs frame))
    (let ((selected-frame (selected-frame))
          elscreen-window-configuration)
      (save-current-buffer
        (select-frame frame)
        (setq elscreen-window-configuration
              (if keep-window-configuration
                  (elscreen-current-window-configuration)
                (elscreen-default-window-configuration)))
        (set-alist 'elscreen-frame-confs frame
                   (list
                    (cons 'modified-inquirer nil)
                    (cons 'screen-property
                          (list
                           (cons 0 (list
                                    (cons 'window-configuration
                                          elscreen-window-configuration)))))
                    (cons 'screen-history (list 0))
                    (cons 'screen-to-name-alist-cache nil)))
        (elscreen-apply-window-configuration elscreen-window-configuration)
        (elscreen-notify-screen-modification 'force-immediately)
        (select-frame selected-frame)))))

(defun elscreen-delete-frame-confs (frame)
  (remove-alist 'elscreen-frame-confs frame)
  (when (eq frame (selected-frame))
    (select-frame (previous-frame))
    (elscreen-notify-screen-modification 'force-immediately)))

(defun elscreen-start ()
  (mapcar
   (lambda (frame)
     (elscreen-make-frame-confs frame 'keep))
   (frame-list))
  (let ((prefix-key elscreen-prefix-key)
        (elscreen-prefix-key nil))
    (elscreen-set-prefix-key prefix-key)))

(static-cond
 ((boundp 'after-make-frame-functions) ;; GNU Emacs 21
  (add-hook 'after-make-frame-functions 'elscreen-make-frame-confs))
 (t ;; XEmacs
  (add-hook 'create-frame-hook 'elscreen-make-frame-confs)))
(static-cond
 ((boundp 'delete-frame-functions) ;; GNU Emacs 22?
  (add-hook 'delete-frame-functions 'elscreen-delete-frame-confs))
 (t ;; XEmacs
  (add-hook 'delete-frame-hook 'elscreen-delete-frame-confs)))

(defsubst elscreen-get-conf-list (frame type)
  (get-alist type (elscreen-get-frame-confs frame)))

(defsubst elscreen-set-conf-list (frame type conf-list)
  (let ((frame-conf (elscreen-get-frame-confs frame)))
    (set-alist 'frame-conf type conf-list)))

(defun elscreen-set-current-screen (value &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-history (elscreen-get-conf-list frame 'screen-history)))
    (setq screen-history (cons value (delq value screen-history)))
    (elscreen-set-conf-list frame 'screen-history screen-history)))

(defun elscreen-get-current-screen (&optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-history (elscreen-get-conf-list frame 'screen-history)))
    (or (car screen-history)
        (car (sort (elscreen-get-screen-list) '<)))))

(defun elscreen-get-previous-screen (&optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-history (elscreen-get-conf-list frame 'screen-history)))
    (or (cadr screen-history)
        (car (sort (elscreen-get-screen-list) '<)))))

(defun elscreen-delete-screen-from-history (value &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-history (elscreen-get-conf-list frame 'screen-history)))
    (setq screen-history (delq value screen-history))
    (elscreen-set-conf-list frame 'screen-history screen-history)))

(defun elscreen-status-label (screen &optional default frame)
  (let* ((default (or default " "))
         (frame (or frame (selected-frame)))
         (current-screen (elscreen-get-current-screen frame))
         (previous-screen (elscreen-get-previous-screen frame)))
    (cond
     ((eq screen current-screen) "+")
     ((eq screen previous-screen) "-")
     (t default))))

(defvar elscreen-notify-screen-modification-suppress-flag nil)
(defmacro elscreen-notify-screen-modification-suppress (&rest body)
  (` (let ((elscreen-notify-screen-modification-suppress-flag t))
       (,@ body))))

(defvar elscreen-screen-update-hook nil)
(defun elscreen-run-screen-update-hook ()
  (elscreen-notify-screen-modification-suppress
   (run-hooks 'elscreen-screen-update-hook))
  (remove-hook 'post-command-hook 'elscreen-run-screen-update-hook))

(defun elscreen-screen-modified-p (inquirer &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (inquirer-list (elscreen-get-conf-list frame 'modified-inquirer))
         (modified (null (memq inquirer inquirer-list))))
    (add-to-list 'inquirer-list inquirer)
    (elscreen-set-conf-list frame 'modified-inquirer inquirer-list)
    modified))

(defun elscreen-set-screen-modified (&optional frame)
  (let* ((frame (or frame (selected-frame))))
    (elscreen-set-conf-list frame 'modified-inquirer nil))
  (add-hook 'post-command-hook 'elscreen-run-screen-update-hook))

(cond
 ((fboundp 'compare-window-configurations)) ;; GNU Emacs
 ((fboundp 'window-configuration-equal) ;; XEmacs 21.5
  (defalias 'compare-window-configurations 'window-configuration-equal)))
(defvar elscreen-screen-modified-hook-pwc nil)
(defun elscreen-notify-screen-modification (&optional mode)
  (when (and (not (window-minibuffer-p))
             (not elscreen-notify-screen-modification-suppress-flag)
             (or (eq mode 'force)
                 (eq mode 'force-immediately)
                 (null elscreen-screen-modified-hook-pwc)
                 (and (fboundp 'compare-window-configurations)
                      (not (compare-window-configurations
                            (current-window-configuration)
                            elscreen-screen-modified-hook-pwc)))))
    (setq elscreen-screen-modified-hook-pwc
          (current-window-configuration))
    (elscreen-set-screen-modified)
    (when (eq mode 'force-immediately)
      (elscreen-run-screen-update-hook))))

(defmacro elscreen-screen-modified-hook-setup (&rest hooks-and-functions)
  (cons
   'progn
   (mapcar
    (lambda (hook-or-function)
      (let ((mode ''normal))
        (when (listp hook-or-function)
          (setq mode (nth 1 hook-or-function))
          (setq hook-or-function (nth 0 hook-or-function)))
        (cond
         ((string-match "-\\(hooks?\\|functions\\)$"
                        (symbol-name hook-or-function))
          ( `(add-hook '(, hook-or-function)
                       (lambda (&rest ignore)
                         (elscreen-notify-screen-modification (, mode))))))
         (t ;; Assume hook-or-function is function
          ( `(defadvice (, hook-or-function) (around
                                              elscreen-screen-modified-advice
                                              activate)
               ad-do-it
               (elscreen-notify-screen-modification (, mode))))))))
    hooks-and-functions)))

(elscreen-screen-modified-hook-setup
 (recenter 'force) (change-major-mode-hook 'force)
 other-window
 window-configuration-change-hook window-size-change-functions
 (handle-switch-frame 'force) ;; GNU Emacs 21
 (select-frame-hook 'force) ;; XEmacs
 (Info-find-node-2 'force))

(defun elscreen-get-screen-property (screen &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-property-list (elscreen-get-conf-list frame 'screen-property)))
    (get-alist screen screen-property-list)))

(defun elscreen-set-screen-property (screen property &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-property-list (elscreen-get-conf-list frame 'screen-property)))
    (set-alist 'screen-property-list screen property)
    (elscreen-set-conf-list frame 'screen-property screen-property-list)))

(defun elscreen-delete-screen-property (screen &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-property-list (elscreen-get-conf-list frame 'screen-property)))
    (remove-alist 'screen-property-list screen)
    (elscreen-set-conf-list frame 'screen-property screen-property-list)))

(defun elscreen-get-window-configuration (screen &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-property (elscreen-get-screen-property screen frame)))
    (get-alist 'window-configuration screen-property)))

(defun elscreen-set-window-configuration (screen winconf &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-property (elscreen-get-screen-property screen frame)))
    (set-alist 'screen-property 'window-configuration winconf)
    (elscreen-set-screen-property screen screen-property frame)))

(defun elscreen-get-screen-nickname (screen &optional frame)
  "Return nickname of SCREEN in FRAME from `elscreen-frame-confs', a string.
If FRAME is omitted, selected-frame is used."
  (let* ((frame (or frame (selected-frame)))
         (screen-property (elscreen-get-screen-property screen frame)))
    (get-alist 'nickname screen-property)))

(defun elscreen-set-screen-nickname (screen nickname &optional frame)
  "Set SCREEN's nickname to SCREEN-NICKNAME."
  (let* ((frame (or frame (selected-frame)))
         (screen-property (elscreen-get-screen-property screen frame)))
    (set-alist 'screen-property 'nickname nickname)
    (elscreen-set-screen-property screen screen-property frame)))

(defun elscreen-delete-screen-nickname (screen &optional frame)
  "Remove SCREEN's nickname from `elscreen-frame-confs'."
  (let* ((frame (or frame (selected-frame)))
         (screen-property (elscreen-get-screen-property screen frame)))
    (remove-alist 'screen-property 'nickname)
    (elscreen-set-screen-property screen screen-property frame)))

(defun elscreen-get-screen-to-name-alist-cache (&optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-get-conf-list frame 'screen-to-name-alist-cache)))

(defun elscreen-set-screen-to-name-alist-cache (alist &optional frame)
  (let ((frame (or frame (selected-frame))))
    (elscreen-set-conf-list frame 'screen-to-name-alist-cache alist)))

(defun elscreen-get-number-of-screens (&optional frame)
  "Return total number of screens in FRAME.
If FRAME is omitted, selected-frame is used."
  (let ((frame (or frame (selected-frame))))
    (length (elscreen-get-screen-list frame))))

(defun elscreen-one-screen-p (&optional frame)
  "Return t if FRAME has only one screen.
If FRAME is omitted, selected-frame is used."
  (let ((frame (or frame (selected-frame))))
    (= (elscreen-get-number-of-screens frame) 1)))

(defun elscreen-get-screen-list (&optional frame)
  "Return a list of screen numbers.
If FRAME is omitted, selected-frame is used."
  (let ((frame (or frame (selected-frame))))
    (mapcar 'car (elscreen-get-conf-list frame 'screen-property))))

(defun elscreen-screen-live-p (screen &optional frame)
  "Return t when SCREEN on FRAME is alive.
If FRAME is omitted, selected-frame is used."
  (let* ((frame (or frame (selected-frame))))
    (not (null (elscreen-get-window-configuration screen frame)))))

(defun elscreen-copy-tree (tree)
  (if (fboundp 'copy-tree)
      (copy-tree tree)
    (let (clone)
      (while (consp tree)
        (setq clone (cons (or (and (consp (car tree))
                                   (elscreen-copy-tree (car tree)))
                              (car tree))
                          clone))
        (setq tree (cdr tree)))
      (nconc (nreverse clone) tree))))

(defmacro elscreen-save-screen-excursion (&rest body)
  (` (let ((original-buffer-list (buffer-list))
           (original-buffer-live-p nil)
           (original-elscreen-window-configuration
            (elscreen-current-window-configuration))
           (original-frame-confs (elscreen-copy-tree elscreen-frame-confs)))
       (unwind-protect
           (save-window-excursion
             (,@ body))
         (setq elscreen-frame-confs original-frame-confs)
         (elscreen-apply-window-configuration
          original-elscreen-window-configuration)
         (mapcar
          (lambda (buffer)
            (when (buffer-live-p buffer)
              (bury-buffer buffer)
              (setq original-buffer-live-p t)))
          original-buffer-list)
         (when original-buffer-live-p
           (while (not (memq (car (buffer-list)) original-buffer-list))
             (bury-buffer (car (buffer-list)))))))))

(defun elscreen-goto-internal (screen)
  (let ((elscreen-window-configuration
         (elscreen-get-window-configuration screen)))
    (elscreen-apply-window-configuration elscreen-window-configuration)))

(defvar elscreen-create-hook nil)
(defun elscreen-create-internal (&optional noerror)
  "Create a new screen."
  (cond
   ((>= (elscreen-get-number-of-screens) 10)
    (unless noerror
      (elscreen-message "No more screens."))
    nil)
   (t
    (let ((screen-list (sort (elscreen-get-screen-list) '<))
          (screen 0))
      (elscreen-set-window-configuration
       (elscreen-get-current-screen)
       (elscreen-current-window-configuration))
      (while (eq (nth screen screen-list) screen)
        (setq screen (+ screen 1)))
      (elscreen-set-window-configuration
       screen (elscreen-default-window-configuration))
      (elscreen-notify-screen-modification 'force)
      (run-hooks 'elscreen-create-hook)
      screen))))

(defun elscreen-find-screen (condition &optional frame)
  (let* ((frame (or frame (selected-frame)))
         (screen-list (sort (elscreen-get-screen-list frame) '<)))
    (elscreen-set-window-configuration
     (elscreen-get-current-screen)
     (elscreen-current-window-configuration))
    (elscreen-notify-screen-modification-suppress
     (catch 'found
       (elscreen-save-screen-excursion
        (mapc
         (lambda (screen)
           (when (funcall condition screen)
             (throw 'found screen)))
         screen-list)
        nil)))))

(defun elscreen-find-screen-by-buffer (buffer &optional create)
  (let* ((buffer (if (bufferp buffer) buffer (get-buffer buffer)))
         (target-screen (elscreen-find-screen
                         (lambda (screen)
                           (elscreen-goto-internal screen)
                           (member buffer
                                   (mapcar (lambda (window)
                                             (window-buffer window))
                                           (window-list)))))))
    (when (and (null target-screen) create)
      (cond
       ((null buffer))
       ((setq target-screen (elscreen-create-internal 'noerror))
        (save-window-excursion
          (elscreen-goto-internal target-screen)
          (switch-to-buffer buffer t)
          (elscreen-set-window-configuration
           target-screen
           (elscreen-current-window-configuration))))
       (t
        (setq target-screen (elscreen-get-current-screen))
        (elscreen-goto-internal target-screen)
        (save-selected-window
          (select-window (split-window))
          (switch-to-buffer buffer t)))))
    target-screen))

(defun elscreen-find-screen-by-major-mode (major-mode-or-re)
  (let ((major-mode-re (cond
                        ((stringp major-mode-or-re)
                         major-mode-or-re)
                        ((symbolp major-mode-or-re)
                         (format "^%s$" (regexp-quote
                                         (symbol-name major-mode-or-re))))
                        (t nil))))
    (when major-mode-re
      (elscreen-find-screen
       (lambda (screen)
         (elscreen-goto-internal screen)
         (save-selected-window
           (catch 'find
             (mapcar
              (lambda (window)
                (select-window window)
                (when (string-match major-mode-re (symbol-name major-mode))
                  (throw 'find t)))
              (window-list))
             nil)))))))

(defvar elscreen-last-message "Welcome to ElScreen!"
  "*Last shown message.")
(defun elscreen-message (message &optional sec)
  "Display MESSAGE in mini-buffer for SEC seconds.
Default value for SEC is 3."
  (when message
    (setq elscreen-last-message message)
    (message "%s" message)
    (sit-for (or sec 3)))
  (message nil))

;;; Create & Kill & Goto

(defun elscreen-create ()
  "Create a new screen and switch to it."
  (interactive)
  (let ((screen (elscreen-create-internal)))
    (if screen
        (elscreen-goto screen))))

(defun elscreen-clone (&optional target-screen)
  "Create a new screen with the window-configuration of TARGET-SCREEN.
If TARGET-SCREEN is ommitted, current-screen is used."
  (interactive)
  (let ((target-screen (or target-screen (elscreen-get-current-screen)))
        (screen (elscreen-create-internal))
        elscreen-window-configuration)
    (when screen
      (save-window-excursion
        (elscreen-goto-internal target-screen)
        (setq elscreen-window-configuration
              (elscreen-current-window-configuration)))
      (elscreen-set-window-configuration screen elscreen-window-configuration)
      (elscreen-goto screen))))

(defvar elscreen-kill-hook nil)
(defun elscreen-kill (&optional screen)
  "Kill SCREEN.  If optional argument SCREEN is
ommitted, current-screen is killed."
  (interactive "P")
  (let ((screen (or (and (numberp screen) screen)
                    (elscreen-get-current-screen))))
    (cond
     ((not (elscreen-screen-live-p screen))
      (elscreen-message "There is no such screen, cannot kill")
      nil)
     ((elscreen-one-screen-p)
      (elscreen-message "There is only one screen, cannot kill")
      nil)
     (t
      (elscreen-delete-screen-property screen)
      (elscreen-delete-screen-from-history screen)
      (elscreen-goto-internal (elscreen-get-current-screen))
      (elscreen-notify-screen-modification 'force)
      (run-hooks 'elscreen-kill-hook)
      screen))))

(defun elscreen-kill-screen-and-buffers (&optional screen)
  "Kill buffers on SCREEN and SCREEN itself.  If optional
argument SCREEN is omitted, current screen is killed."
  (interactive)
  (let* ((screen (or screen (elscreen-get-current-screen)))
         (elscreen-window-configuration
          (elscreen-get-window-configuration screen)))
    (when (elscreen-kill screen)
      (save-window-excursion
        (elscreen-apply-window-configuration elscreen-window-configuration)
        (mapc
         (lambda (buffer)
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))
         (mapcar 'window-buffer (window-list))))
      screen)))

(defun elscreen-kill-others (&optional screen force)
  "Kill screens other than SCREEN.  If optional argument SCREEN
is ommitted, current screen will survive."
  (interactive)
  (let* ((current-screen (elscreen-get-current-screen))
         (screen (or screen current-screen))
         (screen-list (delq screen (sort (elscreen-get-screen-list) '<)))
         screen-list-string)
    (cond
     ((and (null screen-list))
      (when (interactive-p)
        (elscreen-message "There is only one screen, cannot kill")))
     ((or
       (not (interactive-p))
       (yes-or-no-p (format "Really kill screens other than %d? " screen)))
      (setq screen-list-string (mapconcat
                                (lambda (screen)
                                  (elscreen-delete-screen-property screen)
                                  (elscreen-delete-screen-from-history screen)
                                  (number-to-string screen))
                                screen-list ","))
      (unless (eq current-screen screen)
        (elscreen-goto-internal screen))
      (elscreen-notify-screen-modification 'force-immediately)
      (when (interactive-p)
        (elscreen-message (format "screen %s killed" screen-list-string)))))
    screen-list))

(defvar elscreen-goto-hook nil)
(defun elscreen-goto (screen)
  "Switch to screen SCREEN."
  (interactive "NGoto screen number: ")
  (cond
   ((eq (elscreen-get-current-screen) screen))
   ((elscreen-screen-live-p screen)
    (when (elscreen-get-current-screen)
      (elscreen-set-window-configuration
       (elscreen-get-current-screen)
       (elscreen-current-window-configuration)))
    (elscreen-set-current-screen screen)
    (elscreen-goto-internal screen)
    (static-when (and elscreen-on-emacs (= emacs-major-version 21))
      (when window-system
        (redraw-frame (selected-frame))))
    (elscreen-notify-screen-modification 'force)
    (run-hooks 'elscreen-goto-hook)
    screen)
   (t
    (elscreen-message (format "No screen %d" screen))
    nil)))

(defun elscreen-next ()
  "Switch to the next screen."
  (interactive)
  (cond
   ((elscreen-one-screen-p)
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
   ((elscreen-one-screen-p)
    (elscreen-message
     (format "You cannot escape from screen %d!"
             (elscreen-get-current-screen))))
   (t
    (let* ((screen-list (sort (elscreen-get-screen-list) '>))
           (previous-screen
            (or (nth 1 (memq (elscreen-get-current-screen) screen-list))
                (car screen-list))))
      (elscreen-goto previous-screen)))))

(defun elscreen-toggle ()
  "Toggle to the screen selected previously."
  (interactive)
  (cond
   ((elscreen-one-screen-p)
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
(defalias 'elscreen-jump-0 'elscreen-jump)
(defalias 'elscreen-jump-9 'elscreen-jump)

(defun elscreen-swap ()
  "Interchange screens selected currently and previously."
  (interactive)
  (let* ((current-screen (elscreen-get-current-screen))
         (previous-screen (elscreen-get-previous-screen))
         (current-screen-property
          (elscreen-get-screen-property current-screen))
         (previous-screen-property
          (elscreen-get-screen-property previous-screen)))
    (elscreen-set-screen-property current-screen previous-screen-property)
    (elscreen-set-screen-property previous-screen current-screen-property)
    (elscreen-goto-internal (elscreen-get-current-screen))))

(defun elscreen-screen-nickname (screen-nickname)
  "Set nickname for current screen to SCREEN-NICKNAME."
  (interactive "sSet window title to: ")
  (cond
   ((eq (length screen-nickname) 0)
    (elscreen-delete-screen-nickname (elscreen-get-current-screen)))
   (t
    (elscreen-set-screen-nickname (elscreen-get-current-screen)
                                  screen-nickname)))
  (elscreen-notify-screen-modification 'force))

(defvar elscreen-mode-to-nickname-alist-symbol-list nil)
(defvar elscreen-mode-to-nickname-alist-internal nil)
(defun elscreen-rebuild-mode-to-nickname-alist ()
  (setq elscreen-mode-to-nickname-alist-internal
        (apply 'append
               (mapcar 'symbol-value
                       elscreen-mode-to-nickname-alist-symbol-list)))
  (elscreen-notify-screen-modification 'force-immediately))
(defun elscreen-set-mode-to-nickname-alist (mode-to-nickname-alist-symbol)
  (add-to-list 'elscreen-mode-to-nickname-alist-symbol-list
               mode-to-nickname-alist-symbol)
  (elscreen-rebuild-mode-to-nickname-alist))
(elscreen-set-mode-to-nickname-alist 'elscreen-mode-to-nickname-alist)

(defvar elscreen-buffer-to-nickname-alist-symbol-list nil)
(defvar elscreen-buffer-to-nickname-alist-internal nil)
(defun elscreen-rebuild-buffer-to-nickname-alist ()
  (setq elscreen-buffer-to-nickname-alist-internal
        (apply 'append
               (mapcar 'symbol-value
                       elscreen-buffer-to-nickname-alist-symbol-list)))
  (elscreen-notify-screen-modification 'force-immediately))
(defun elscreen-set-buffer-to-nickname-alist (buffer-to-nickname-alist-symbol)
  (add-to-list 'elscreen-buffer-to-nickname-alist-symbol-list
               buffer-to-nickname-alist-symbol)
  (elscreen-rebuild-buffer-to-nickname-alist))
(elscreen-set-buffer-to-nickname-alist 'elscreen-buffer-to-nickname-alist)

(defmacro elscreen-get-alist-to-nickname (alist op mode-or-buffer-name)
  ( `(catch '(, alist)
       (progn
         (mapcar
          (lambda (map)
            (let ((nickname nil)
                  (condition-data (car map))
                  (string-or-function (cdr map)))
              (when ((, op) condition-data (, mode-or-buffer-name))
                (cond
                 ((functionp string-or-function)
                  (setq nickname
                        (condition-case nil
                            (funcall string-or-function)
                          (wrong-number-of-arguments
                           (funcall string-or-function (current-buffer))))))
                 (t
                  (setq nickname string-or-function)))
                (throw '(, alist) (cons 'nickname nickname)))))
          (, alist))
         nil))))

(defun elscreen-get-screen-to-name-alist ()
  (elscreen-notify-screen-modification-suppress
   (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
     (elscreen-set-window-configuration (elscreen-get-current-screen)
                                        (elscreen-current-window-configuration))
     (let* ((screen-list (sort (elscreen-get-screen-list) '<))
            screen-name screen-to-name-alist nickname-type-map)
       (elscreen-save-screen-excursion
        (mapcar
         (lambda (screen)
           ;; If nickname exists, use it.
           (setq screen-name (elscreen-get-screen-nickname screen))
           ;; Nickname does not exist, so examine major-mode and buffer-name.
           (when (null screen-name)
             (elscreen-goto-internal screen)

             (let* ((start-window (selected-window))
                    (window start-window))
               (while window
                 (with-current-buffer (window-buffer window)
                   (setq nickname-type-map
                         (cons (or (elscreen-get-alist-to-nickname
                                    elscreen-mode-to-nickname-alist-internal
                                    string-match (symbol-name major-mode))
                                   (elscreen-get-alist-to-nickname
                                    elscreen-buffer-to-nickname-alist-internal
                                    string-match (buffer-name))
                                   (cons 'buffer-name (buffer-name)))
                               nickname-type-map))
                   (setq window (when (not (eq (next-window window)
                                               start-window))
                                  (next-window window))))))

             (let (nickname-list)
               (while (> (length nickname-type-map) 0)
                 (setq nickname-list (cons (cdar nickname-type-map)
                                           nickname-list))
                 (if (eq (caar nickname-type-map) 'nickname)
                     (setq nickname-type-map
                           (delete (car nickname-type-map) nickname-type-map))
                   (setq nickname-type-map (cdr nickname-type-map))))
               (setq screen-name (mapconcat (lambda (v) v) nickname-list ":"))))

           (set-alist 'screen-to-name-alist screen screen-name))
         screen-list))

       (elscreen-set-screen-to-name-alist-cache screen-to-name-alist))))
  (elscreen-get-screen-to-name-alist-cache))

(defun elscreen-truncate-screen-name (screen-name truncate-length &optional padding)
  (if (and (> truncate-length 3)
           (> (string-width screen-name) truncate-length))
      (setq screen-name
            (truncate-string-to-width
             (concat (truncate-string-to-width
                      screen-name (- truncate-length 3))
                     "...")
             truncate-length nil ?.))
    (when padding
      (setq screen-name (truncate-string-to-width
                         screen-name truncate-length nil ?\ ))))
  screen-name)

(defun elscreen-display-screen-name-list ()
  "Display the list of screens in mini-buffer."
  (interactive)
  (let ((screen-list (sort (elscreen-get-screen-list) '<))
        (screen-to-name-alist (elscreen-get-screen-to-name-alist)))
    (elscreen-message
     (mapconcat
      (lambda (screen)
        (let ((screen-name (get-alist screen screen-to-name-alist)))
          (format "%d%s %s" screen
                  (elscreen-status-label screen "")
                  screen-name)))
      screen-list "  "))))


;;; Mode Line & Menu & Tab

;; GNU Emacs
(static-when elscreen-on-emacs
  ;; Mode Line
  (defvar elscreen-e21-mode-line-string "[0]")
  (defun elscreen-e21-mode-line-update ()
    (let (screen)
      (when (and (elscreen-screen-modified-p 'elscreen-e21-mode-line-update)
                 (setq screen (elscreen-get-current-screen)))
        (setq elscreen-e21-mode-line-string (format "[%d]" screen))
        (force-mode-line-update))))

  (let ((point (or
                ;; GNU Emacs 21.3.50 or later
                (memq 'mode-line-position mode-line-format)
                ;; GNU Emacs 21.3.1
                (memq 'mode-line-buffer-identification mode-line-format)))
        (elscreen-mode-line-elm '(elscreen-display-screen-number
                                  (" " elscreen-e21-mode-line-string))))
    (when (null (member elscreen-mode-line-elm mode-line-format))
      (setcdr point (cons elscreen-mode-line-elm (cdr point)))))

  (add-hook 'elscreen-screen-update-hook 'elscreen-e21-mode-line-update)

  ;; Menu
  (define-key-after (lookup-key global-map [menu-bar]) [elscreen]
    (cons "ElScreen" (make-sparse-keymap "ElScreen")) 'buffer)

  (defvar elscreen-e21-menu-bar-command-entries
    (list (list 'elscreen-command-separator
                'menu-item
                "--")
          (list 'elscreen-create
                'menu-item
                "Create Screen"
                'elscreen-create
                :help "Create a new screen and switch to it")
          (list 'elscreen-clone
                'menu-item
                "Clone Screen"
                'elscreen-clone
                :help "Create a new screen with the window-configuration of current screen")
          (list 'elscreen-kill
                'menu-item
                "Kill Screen"
                'elscreen-kill
                :help "Kill current screen")
          (list 'elscreen-kill-screen-and-buffers
                'menu-item
                "Kill Screen and Buffers"
                'elscreen-kill-screen-and-buffers
                :help "Kill current screen and buffers")
          (list 'elscreen-kill-others
                'menu-item
                "Kill Other Screens"
                'elscreen-kill-others
                :help "Kill other screens")
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

  (defun elscreen-e21-menu-bar-update (&optional force)
    (when (and (lookup-key (current-global-map) [menu-bar elscreen])
               (or force
                   (elscreen-screen-modified-p 'elscreen-menu-bar-update)))
      (let ((screen-list (sort (elscreen-get-screen-list) '<))
            (screen-to-name-alist (elscreen-get-screen-to-name-alist))
            (elscreen-menu nil))
        (setq elscreen-menu
              (mapcar
               (lambda (screen)
                 (list (string-to-char (number-to-string screen))
                       'menu-item
                       (format "%d%s %s" screen
                               (elscreen-status-label screen)
                               (elscreen-truncate-screen-name
                                (get-alist screen screen-to-name-alist) 25))
                       'elscreen-jump
                       :keys (format "%s %d"
                                     (key-description elscreen-prefix-key)
                                     screen)))
              screen-list))
        (setq elscreen-menu
              (nconc elscreen-menu elscreen-e21-menu-bar-command-entries))
        (setq elscreen-menu
              (cons 'keymap (cons "Select Screen" elscreen-menu)))
        (define-key (current-global-map) [menu-bar elscreen]
          (cons (copy-sequence "ElScreen") elscreen-menu)))))

  (add-hook 'elscreen-screen-update-hook 'elscreen-e21-menu-bar-update)

  ;; Tab
  (defvar elscreen-e21-tab-format nil)
  (make-variable-buffer-local 'elscreen-e21-tab-format)

  (defsubst elscreen-e21-tab-create-keymap (&optional mouse-1 mouse-2 mouse-3)
    (let ((keymap (make-sparse-keymap))
          (mouse-1 (or mouse-1 'ignore))
          (mouse-2 (or mouse-2 'ignore))
          (mouse-3 (or mouse-3 'ignore)))
      (define-key keymap [header-line down-mouse-1] 'ignore)
      (define-key keymap [header-line down-mouse-2] 'ignore)
      (define-key keymap [header-line down-mouse-3] 'ignore)
      (define-key keymap [header-line drag-mouse-1] 'ignore)
      (define-key keymap [header-line drag-mouse-2] 'ignore)
      (define-key keymap [header-line drag-mouse-3] 'ignore)
      (define-key keymap [header-line mouse-1] mouse-1)
      (define-key keymap [header-line mouse-2] mouse-2)
      (define-key keymap [header-line mouse-3] mouse-3)
      keymap))

  (defun elscreen-e21-tab-escape-% (string)
    (if (string-match "%" string)
        (let ((retval "")
              start (end 0) substring)
          (while (setq start end)
            (setq end (next-property-change start string))
            (setq substring (replace-regexp-in-string
                             "%" "%%" (substring string start end)))
            (set-text-properties 0 (length substring)
                                 (text-properties-at start string) substring)
            (setq retval (concat retval substring)))
          retval)
      string))

  (defun elscreen-e21-tab-update (&optional force)
    (when (and (not (window-minibuffer-p))
               (or (elscreen-screen-modified-p 'elscreen-tab-update) force))
      (walk-windows
       (lambda (window)
         (with-current-buffer (window-buffer window)
           (when (and (boundp 'elscreen-e21-tab-format)
                      (equal header-line-format elscreen-e21-tab-format)
                      (or (not (eq (window-buffer window)
                                   (window-buffer (frame-first-window))))
                          (not elscreen-display-tab)))
             (kill-local-variable 'elscreen-e21-tab-format)
             (setq header-line-format nil))))
       'other 'other)

      (when elscreen-display-tab
        (let ((screen-list (sort (elscreen-get-screen-list) '<))
              (screen-to-name-alist (elscreen-get-screen-to-name-alist))
              (current-screen (elscreen-get-current-screen))
              (half-space (eval-when-compile
                            (propertize
                             " "
                             'display '(space :width 0.5))))
              (tab-separator (eval-when-compile
                               (propertize
                                " "
                                'face 'elscreen-tab-background-face
                                'display '(space :width 0.5))))
              (control-tab (eval-when-compile
                             (propertize
                              "<->"
                              'face 'elscreen-tab-control-face
                              'local-map (elscreen-e21-tab-create-keymap
                                          'elscreen-previous
                                          'elscreen-create
                                          'elscreen-next)
                              'help-echo "mouse-1: previous screen, mouse-2: create new screen, mouse-3: next screen"))))
          (with-current-buffer (window-buffer (frame-first-window))
            (kill-local-variable 'elscreen-e21-tab-format)
            (when elscreen-tab-display-control
              (setq elscreen-e21-tab-format
                    (nconc
                     elscreen-e21-tab-format
                     (list
                      control-tab
                      tab-separator))))

            (mapcar
             (lambda (screen)
               (setq elscreen-e21-tab-format
                     (nconc
                      elscreen-e21-tab-format
                      (list
                       (propertize
                        (concat
                         (when elscreen-tab-display-kill-screen
                           (propertize
                            (concat "[X]" half-space)
                            'local-map (elscreen-e21-tab-create-keymap
                                        `(lambda (e)
                                           (interactive "e")
                                           (elscreen-kill ,screen)))))
                         (propertize
                          (format "%d%s%s%s"
                                  screen
                                  (elscreen-status-label screen)
                                  half-space
                                  (elscreen-e21-tab-escape-%
                                   (elscreen-truncate-screen-name
                                    (get-alist screen screen-to-name-alist)
                                    elscreen-tab-width t)))
                          'help-echo (get-alist screen screen-to-name-alist)
                          'local-map (elscreen-e21-tab-create-keymap
                                      `(lambda (e)
                                         (interactive "e")
                                         (elscreen-goto ,screen)))))
                        'face (if (eq current-screen screen)
                                  'elscreen-tab-current-screen-face
                                'elscreen-tab-other-screen-face))
                       tab-separator))))
             screen-list)

            (setq elscreen-e21-tab-format
                  (nconc
                   elscreen-e21-tab-format
                   (list
                    (propertize
                     (make-string (window-width) ?\ )
                     'face 'elscreen-tab-background-face
                     'local-map (elscreen-e21-tab-create-keymap)))))

            (setq header-line-format elscreen-e21-tab-format))))))

  (add-hook 'elscreen-screen-update-hook 'elscreen-e21-tab-update))

;; XEmacs
(static-when elscreen-on-xemacs
  ;; Mode Line
  (defvar elscreen-xmas-mode-line-string "[0]")
  (defun elscreen-xmas-mode-line-update ()
    (let (screen)
      (when (and (elscreen-screen-modified-p 'elscreen-xmas-mode-line-update)
                 (setq screen (elscreen-get-current-screen)))
        (setq elscreen-xmas-mode-line-string (format "[%d]" screen))
        (force-mode-line-update))))

  (let ((point (memq 'global-mode-string mode-line-format))
        (elscreen-mode-line-elm '(elscreen-display-screen-number
                                  (" " elscreen-xmas-mode-line-string))))
    (if (null (member elscreen-mode-line-elm mode-line-format))
        (setcdr point (cons elscreen-mode-line-elm (cdr point)))))

  (add-hook 'elscreen-screen-update-hook 'elscreen-xmas-mode-line-update)

  ;; Menu
  (defvar elscreen-xmas-menu-bar-command-entries
    '("%_ElScreen"
      :filter elscreen-xmas-menu-bar-filter
      "----"
      ["%_Create Screen" elscreen-create]
      ["%_Clone Screen" elscreen-clone]
      ["%_Kill Screen" elscreen-kill]
      ["%_Kill Screen and Buffers" elscreen-kill-screen-and-buffers]
      ["%_Kill Other Screens" elscreen-kill-others]
      ["%_Next Screen" elscreen-next]
      ["%_Previous Screen" elscreen-previous]
      ["%_Toggle Screen" elscreen-toggle]))

  (defconst elscreen-menubar (copy-sequence default-menubar))
  (let ((menubar elscreen-menubar))
    (catch 'buffers-menu-search
      (while (car menubar)
        (when (equal (car (car menubar)) "%_Buffers")
          (throw 'buffers-menu-search menubar))
        (setq menubar (cdr menubar))))
    (setcdr menubar
            (cons elscreen-xmas-menu-bar-command-entries (cdr menubar))))

  (set-menubar elscreen-menubar)
  (set-menubar-dirty-flag)

  (defun elscreen-xmas-menu-bar-filter (menu)
    (let ((screen-list (sort (elscreen-get-screen-list) '<))
          (screen-to-name-alist (elscreen-get-screen-to-name-alist))
          (elscreen-menu nil))
      (setq elscreen-menu
            (mapcar
             (lambda (screen)
               (vector (format "%d%s %s" screen
                               (elscreen-status-label screen)
                               (elscreen-truncate-screen-name
                                (get-alist screen screen-to-name-alist) 25))
                       `(elscreen-goto ,screen)
                       :active t
                       :keys (format "%s %d"
                                     (key-description elscreen-prefix-key)
                                     screen)))
             screen-list))
      (append elscreen-menu menu))))

;;; Help

(defvar elscreen-help-symbol-list nil)
(defun elscreen-set-help (help-symbol)
  (add-to-list 'elscreen-help-symbol-list help-symbol t))
(elscreen-set-help 'elscreen-help)

(defun elscreen-help ()
  "Show key bindings of ElScreen and Add-On softwares."
  (interactive)
  (with-output-to-temp-buffer "*ElScreen Help*"
    (princ (substitute-command-keys
            (mapconcat 'symbol-value
                       elscreen-help-symbol-list "\n\n")))
    (print-help-return-message)))

;;; Utility Functions

(defun elscreen-display-version ()
  "Display ElScreen version."
  (interactive)
  (elscreen-message (concat "ElScreen version " elscreen-version)))

(defun elscreen-display-screen-number-toggle ()
  "Toggle the screen number in the mode line."
  (interactive)
  (setq elscreen-display-screen-number (null elscreen-display-screen-number))
  (redraw-frame (selected-frame)))

(defun elscreen-display-last-message ()
  "Repeat the last message displayed in the mini-buffer."
  (interactive)
  (elscreen-message elscreen-last-message 5))

(defun elscreen-display-time ()
  "Show system information."
  (interactive)
  (elscreen-message
   (format "%s %s / %s"
           (current-time-string)
           (nth 1 (current-time-zone))
           (mapconcat
            (lambda (load)
              (format "%.2f" (/ load 100.0)))
            (load-average) " "))
   3))

(defun elscreen-select-and-goto ()
  (interactive)
  (let* ((screen-list (sort (elscreen-get-screen-list) '<))
         (screen-to-name-alist (elscreen-get-screen-to-name-alist))
         (command-list '(("c" . elscreen-create)
                         ("n" . elscreen-next)
                         ("p" . elscreen-previous)
                         ("t" .  elscreen-toggle)))
         (truncate-length (- (frame-width) 6))
         (candidate-window-height (max (+ (elscreen-get-number-of-screens) 4)
                                       window-min-height))
         (candidate-buffer (get-buffer-create
                            (format " *ElScreen-select:%s*"
                                    (prin1-to-string (selected-frame)))))
         (candidate (concat
                     "Current screen list: \n\n"
                     (mapconcat
                      (lambda (screen)
                        (format "  %d%s %s\n" screen
                                (elscreen-status-label screen)
                                (elscreen-truncate-screen-name
                                 (get-alist screen screen-to-name-alist)
                                 truncate-length)))
                      screen-list nil)))
         (prompt "Select screen or (c)reate, (n)ext, (p)revious, (t)oggle: ")
         (minibuffer-map (copy-keymap minibuffer-local-map))
         window frame-last-window command-or-target-screen mini-hist)
    ;; prepare window to show candidates
    (save-window-excursion
      (setq frame-last-window (previous-window (static-if elscreen-on-xemacs
                                                   (frame-highest-window)
                                                 (frame-first-window))))
      (while (minibuffer-window-active-p frame-last-window)
        (setq frame-last-window (previous-window frame-last-window)))
      (while (and (not (one-window-p))
                  (or (< (window-width frame-last-window)
                         (frame-width))
                      (< (window-height frame-last-window)
                         (+ candidate-window-height window-min-height))))
        (setq window frame-last-window)
        (setq frame-last-window (previous-window window))
        (delete-window window))
      (select-window (split-window frame-last-window))
      (shrink-window (- (window-height) candidate-window-height))
      ;; now switch to the buffer for candidates and fill it
      (switch-to-buffer candidate-buffer)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert candidate)
        (goto-char (point-min))
        (save-excursion
          (while (not (eobp))
            (when (looking-at "^  \\([0-9]\\)\\(.\\) \\(.*\\)$")
              (put-text-property
               (match-beginning 1) (match-end 1) 'face 'bold)
              (cond
               ((string= (match-string 2) "+")
                (put-text-property
                 (match-beginning 3) (match-end 3) 'face 'bold))))
            (forward-line 1)))
        (setq buffer-read-only t)
        (set-buffer-modified-p nil))
      ;; make keymap for minibuffer
      (suppress-keymap minibuffer-map t)
      (define-key minibuffer-map "\C-g" 'abort-recursive-edit)
      (define-key minibuffer-map "\C-m" 'exit-recursive-edit)
      (define-key minibuffer-map "q" 'exit-recursive-edit)
      (define-key minibuffer-map " " 'exit-recursive-edit)
      (mapcar
       (lambda (command)
         (define-key minibuffer-map (car command) 'self-insert-and-exit))
       command-list)
      (mapcar
       (lambda (screen)
         (define-key minibuffer-map (number-to-string screen)
           'self-insert-and-exit))
       screen-list)
      ;; read key from minibuffer
      (unwind-protect
          (setq command-or-target-screen
                (read-from-minibuffer prompt nil minibuffer-map
                                      nil 'mini-hist))
        (kill-buffer candidate-buffer)))
    (cond
     ((string= command-or-target-screen ""))
     ((get-alist command-or-target-screen command-list)
      (funcall (get-alist command-or-target-screen command-list)))
     (t
      (elscreen-goto (string-to-number command-or-target-screen))))))

(defun elscreen-find-and-goto-by-buffer (&optional buffer create noselect)
  "Go to the screen that has the window with buffer BUFFER,
creating one if none already exists."
  (interactive)
  (let* ((prompt "Go to the screen with specified buffer: ")
         (create (or create (interactive-p)))
         (buffer-name (or (and (bufferp buffer) (buffer-name buffer))
                          (and (stringp buffer) buffer)
                          (and (featurep 'iswitchb)
                               (iswitchb-read-buffer prompt))
                          (read-buffer prompt)))
         (target-screen (elscreen-find-screen-by-buffer
                         (get-buffer-create buffer-name) create)))
    (when target-screen
      (elscreen-goto target-screen)
      (unless noselect
        (select-window
         (get-alist buffer-name
                    (mapcar
                     (lambda (window)
                       (cons (buffer-name (window-buffer window)) window))
                     (window-list))))))
    target-screen))

(defun elscreen-find-file (filename)
  "Edit file FILENAME.
Switch to a screen visiting file FILENAME,
creating one if none already exists."
  (interactive "FFind file in new screen: ")
  (elscreen-find-and-goto-by-buffer (find-file-noselect filename) 'create))

(defun elscreen-find-file-read-only (filename)
  "Edit file FILENAME with new screen but don't allow changes.
Like \\[elscreen-find-file] but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing."
  (interactive "FFind file read-only in new screen: ")
  (elscreen-find-file filename)
  (toggle-read-only 1))

(defun elscreen-dired (dirname &optional switches)
  (interactive (progn
                 (or (featurep 'dired) (require 'dired))
                 (dired-read-dir-and-switches "in new screen ")))
  (elscreen-find-and-goto-by-buffer (dired-noselect dirname switches) 'create))

(defun elscreen-execute-extended-command (prefix-arg)
  (interactive "P")
  (let ((prefix-arg prefix-arg)
        (prefix-key (key-description elscreen-prefix-key))
        target-screen)
    (setq this-command (intern (completing-read
                                ;; Note: this has the hard-wired
                                ;;  "C-u" and "M-x" string bug in common
                                ;;  with all Emacs's.
                                ;; (i.e. it prints C-u and M-x regardless of
                                ;; whether some other keys were actually bound
                                ;; to `execute-extended-command' and
                                ;; `universal-argument'.
                                (cond ((eq prefix-arg '-)
                                       (format "- %s M-x " prefix-key))
                                      ((equal prefix-arg '(4))
                                       (format "C-u %s M-x " prefix-key))
                                      ((integerp prefix-arg)
                                       (format "%d %s M-x "
                                               prefix-arg prefix-key))
                                      ((and (consp prefix-arg)
                                            (integerp (car prefix-arg)))
                                       (format "%d %s M-x "
                                               (car prefix-arg) prefix-key))
                                      (t
                                       (format "%s M-x " prefix-key)))
                                obarray 'commandp t nil
                                (static-if elscreen-on-xemacs
                                    'read-command-history
                                  'extended-command-history))))
    (if (setq target-screen (elscreen-create-internal 'noerror))
        (elscreen-goto target-screen)
      (select-window (split-window)))
    (command-execute this-command t)))

;;; Command-line processing at startup time

(defun elscreen-command-line-funcall (switch-string)
  (let ((argval (intern (car command-line-args-left)))
        screen elscreen-window-configuration)
    (setq command-line-args-left (cdr command-line-args-left))
    (save-window-excursion
      (elscreen-apply-window-configuration
       (elscreen-default-window-configuration))
      (if (commandp argval)
          (command-execute argval)
        (funcall argval))
      (setq elscreen-window-configuration
            (elscreen-current-window-configuration)))
    (setq file-count (1+ file-count))
    (cond
     ((= file-count 1)
      (elscreen-apply-window-configuration elscreen-window-configuration))
     ((setq screen (elscreen-create-internal 'noerror))
      (elscreen-set-window-configuration screen
                                           elscreen-window-configuration)))))

(defun elscreen-command-line-find-file (file file-count &optional line column)
  (let ((line (or line 0))
        (column (or column 0)))
    (cond
     ((= file-count 1)
      (find-file file))
     ((> file-count 10)
      (find-file-noselect file))
     (t
      (elscreen-find-screen-by-buffer (find-file-noselect file) 'create)))
    (or (zerop line)
        (goto-line line))
    (unless (< column 1)
      (move-to-column (1- column)))))

(when elscreen-startup-command-line-processing
  (setq command-switch-alist
        (append command-switch-alist
                '(("-elscreen-funcall" . elscreen-command-line-funcall)
                  ("-e"                . elscreen-command-line-funcall))))

  (static-when elscreen-on-emacs
    (defun elscreen-e21-command-line ()
      (when (string-match "\\`-" argi)
        (error "Unknown option `%s'" argi))
      (setq file-count (1+ file-count))
      (setq inhibit-startup-buffer-menu t)
      (let* ((file
              (expand-file-name
               (command-line-normalize-file-name orig-argi)
               dir)))
        (elscreen-command-line-find-file file file-count line column))
      (setq line 0)
      (setq column 0)
      t)

    (add-hook 'after-init-hook (lambda ()
                                 (add-to-list 'command-line-functions
                                              'elscreen-e21-command-line t))))

  (static-when elscreen-on-xemacs
    (defadvice command-line-1 (around elscreen-xmas-command-line-1 activate)
      (cond
       ((null command-line-args-left)
        ad-do-it)
       (t
        (let ((dir command-line-default-directory)
              (file-count 0)
              (line nil)
              (end-of-options nil)
              file-p arg tem)
          (while command-line-args-left
            (setq arg (pop command-line-args-left))
            (cond
             (end-of-options
              (setq file-p t))
             ((setq tem (when (eq (aref arg 0) ?-)
                          (or (assoc arg command-switch-alist)
                              (assoc (substring arg 1)
                                     command-switch-alist))))
              (funcall (cdr tem) arg))
             ((string-match "\\`\\+[0-9]+\\'" arg)
              (setq line (string-to-int arg)))
             ((or (string= arg "-") (string= arg "--"))
              (setq end-of-options t))
             (t
              (setq file-p t)))

            (when file-p
              (setq file-p nil)
              (incf file-count)
              (elscreen-command-line-find-file
               (expand-file-name arg dir) file-count line)
              (setq line nil)))))))))

;;; Unsupported Functions...

(defun elscreen-link ()
  (interactive)
  (cond
   ((null (one-window-p))
    (elscreen-message "current screen must not have two or more window!"))
   ((or (null (elscreen-get-previous-screen))
        (elscreen-one-screen-p))
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

;;; Start ElScreen!

(elscreen-start)
