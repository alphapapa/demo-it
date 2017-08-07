;;; DEMO-IT-RECORDING --- recording demo-it presentations
;;
;; Author: Marcin Borkowski <mbork@mbork.pl>
;; Copyright © 2017, Marcin Borkowski, all rights reserved.
;; Created: 30 July 2017
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Commands to enable "recording" demo-it presentations, using
;; keyboard macros.  An early proof-of-concept.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar demo-it--recorded-actions nil
  "A list of actions recorded, as keyboard macros.
Note: this is in reversed order!")

(defun demo-it-clear-recorded-actions ()
  "Clear the recorded actions."
  (interactive)
  (setq demo-it--recorded-actions nil))

(defun demo-it--save-kbd-macro (&rest _ignore)
  "Store the last keyboard macro in `demo-it--recorded-actions'.
Remove demo-it-recording advice."
  (push last-kbd-macro demo-it--recorded-actions)
  (advice-remove 'kmacro-end-or-call-macro #'demo-it--save-kbd-macro)
  (advice-remove 'keyboard-quit #'demo-it--cancel-kbd-macro))

(defun demo-it--cancel-kbd-macro (&rest _ignore)
  "Do not store the last keyboard macro.
Remove demo-it-recording advice."
  (advice-remove 'kmacro-end-or-call-macro #'demo-it--save-kbd-macro)
  (advice-remove 'keyboard-quit #'demo-it--cancel-kbd-macro))

(defun demo-it-start-recording ()
  "Start recording a keyboard macro to end up in a demo-it
presentation."
  (interactive)
  (kmacro-start-macro nil)
  (message "%s" "Recording for demo-it started.  Press F4 to stop and C-g to cancel.")
  (advice-add 'kmacro-end-or-call-macro :after #'demo-it--save-kbd-macro)
  (advice-add 'keyboard-quit :after #'demo-it--cancel-kbd-macro))

(defun demo-it--kbd-macro-to-demo-it-action (macro)
  "Convert MACRO to a suitable demo-it action."
  (when (vectorp macro)
    (setq macro (mapcar (lambda (key)
			  (if (eq key 'return)
			      10
			    key))
			action)))
  (condition-case err
      (let ((macro-string (seq-into macro 'string)))
	(if (string-match (seq-into '(?[ 0 ?- 9 11 ?- 31 ?]) 'string) macro-string)
	    (format "\"%s\"" (key-description macro))
	  (format "(demo-it-insert \"%s\")"
		  (replace-regexp-in-string
		   "\"" "\\\\\""
		   (replace-regexp-in-string
		    "\n" "\\\\n"
		    (replace-regexp-in-string
		     "\\\\" "\\\\\\\\" macro-string))))))
    (wrong-type-argument (format "\"%s\"" (key-description macro)))))

(defun demo-it-yank-recorded-actions ()
  "Yank the recorded actions at point."
  (interactive)
  (let ((actions (reverse demo-it--recorded-actions)))
    (insert (format "(demo-it-create\n%s)\n"
		    (mapconcat (lambda (action)
				 (format "  %s\n" (demo-it--kbd-macro-to-demo-it-action action)))
			       actions
			       ""))))
  ;; (demo-it-clear-recorded-actions)
  )

(provide 'demo-it-recording)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; demo-it-recording.el ends here

