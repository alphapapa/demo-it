;;; DEMO-IT --- Create demonstrations
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright (C) 2014  Howard Abrams
;; Keywords: demonstration presentation test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   When making demonstrations of new products, technologies and other
;;   geekery, I love the versatility of using Emacs to demonstrate the
;;   trifecta of sprint reviews, including:
;;
;;   - Presentations explaining the technologies
;;   - Source code ... correctly highlighted
;;   - Executing the code in Eshell ... or similar demonstration
;;   - Test a new feature
;;
;;   However, I don't want to fat-finger, mentally burp, or even delay
;;   the gratification while I type, so I predefine each "step" as an
;;   Elisp function or keyboard macro, and then have =demo-it= execute
;;   each function when I hit either the SPACE key or the F12 key
;;   (advanced minor mode).
;;
;;   Using the library is a four step process:
;;
;;   1. Load the library in your own Elisp source code file
;;   2. Create a collection of functions that "do things".
;;   3. Create the ordered list of functions/steps with `demo-it-create'
;;   4. Start the demonstration with `demo-it-start'
;;
;;   For instance:
;;
;;       (require 'demo-it)   ;; Load this library of functions
;;
;;       (defun dit-load-source-code ()
;;         "Load some source code in a side window."
;;         (demo-it-presentation-advance)
;;         (demo-it-load-fancy-file "example.py" :line 5 12 :side))
;;
;;       (defun dit-run-code ()
;;         "Execute our source code in an Eshell buffer."
;;         ;; Close other windows and advance the presentation:
;;         (demo-it-presentation-return)
;;         (demo-it-start-shell)
;;         (demo-it-run-in-shell "python example.py Snoopy"))
;;
;;       (demo-it-create :single-window :insert-slow :full-screen
;;                       (demo-it-title-screen "example-title.org")
;;                       (demo-it-presentation "example.org")
;;                        dit-load-source-code
;;                        dit-run-code
;;                       (demo-it-run-in-shell "exit" nil :instant))
;;
;;       (demo-it-start)
;;
;;   Each "step" is a series of Elisp functions that "do things".
;;   While this package has a collection of helping functions, the steps
;;   can use any Elisp command to show off a feature.
;;
;;   I recommend installing these other Emacs packages:
;;
;;   - https://github.com/takaxp/org-tree-slide
;;   - https://github.com/sabof/org-bullets
;;   - https://github.com/magnars/expand-region.el
;;   - https://github.com/Bruce-Connor/fancy-narrow
;;
;;   See http://github.com/howardabrams/demo-it for more details and
;;   better examples.  You will want to walk through the source code
;;   for all the utility functions.
;;
;;; Code:

(require 'cl-lib)

;; Predefined necessary external functions:
(declare-function face-remap-remove-relative "face-remap.el")
(defvar org-image-actual-width)

;; And functions from other projects I like to use...
(declare-function eshell-send-input "ext:eshell")
(declare-function show-all "ext:eshell.c")

;; Load our 'modules' from other files:
(require 'demo-it-custom)

;; And specify the customization variables set in that module:
(defvar demo-it--shell-or-eshell)
(defvar demo-it--keymap-mode-style)
(defvar demo-it--insert-text-speed)
(defvar demo-it--open-windows)
(defvar demo-it--open-windows-size)
(defvar demo-it--text-scale)
(defvar demo-it--start-fullscreen)
(defvar demo-it--start-single-window)
(declare-function demo-it--get-insert-text-speed "demo-it-custom.el")
(declare-function demo-it--set-property "demo-it-custom.el")


;; ----------------------------------------------------------------------
;; MINOR MODES
;;
;;   We define two styles of minor modes for dealing with special keys
;;   to advance the demonstration along ... a simple uses space and
;;   return, and an 'advanced' mode that requires a special function
;;   key...

(define-minor-mode demo-it-mode "Pressing 'space' advances demo."
  :lighter " demo"
  :require 'demo-it
  :global t
  :keymap '((" "               . demo-it-step)
            (""              . demo-it-step)
            ("[down]"          . demo-it-step)
            ("[mouse-1]"       . demo-it-set-mouse-or-advance)
            ([nil mouse-1]     . demo-it-step)
            ([nil wheel-up]    . demo-it-ignore-event)
            ([nil wheel-down]  . demo-it-ignore-event)
            ([nil wheel-left]  . demo-it-ignore-event)
            ([nil wheel-right] . demo-it-ignore-event)
            ("q"               . demo-it-disable-mode)
            ("Q"               . demo-it-end)))

(define-minor-mode demo-it-mode-adv "Pressing '<f12>' advances demo."
  :lighter " demo-adv"
  :require 'demo-it
  :global  t
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd   "<f12>") 'demo-it-step)
             (define-key map (kbd "s-<f12>") 'demo-it-insert-text)
             (define-key map (kbd "A-<f12>") 'demo-it-insert-text)
             (define-key map (kbd "M-<f12>") 'demo-it-end)
             map))

;; Demo Mode
;;
;;   Allows us to advance to the next step by pressing the
;;   space bar or return. Press the 'q' key to stop the mode.

(defun demo-it-disable-mode ()
  "Called when 'q' (or similar key) pressed to disable either the
simple or advanced version of `demo-it-mode'."
  (interactive)
  (demo-it-mode -1)
  (demo-it-mode-adv -1))

;; ----------------------------------------------------------------------
;; DEMONSTRATION MAKER
;;
;;   When we start a demonstration, we would pass in a list of functions
;;   to call for each step, and then call =demo-step= to execute the
;;   first one on the list.

;; A "global" variable (shudder) to track state of the demonstration:
(defvar demo-it--step 0  "Stores the current demo 'step' function.")
(defvar demo-it--steps '() "List of functions to be executed in order.")
(defvar demo-it-start-winconf nil
  "Window configuration when starting demo. Used to restore
  buffer positions after demonstration is complete.")

;;;###autoload
(defun demo-it-start (&optional steps advanced-mode)
  "Start the current demonstration and kick off the first step.
STEPS is a list of functions or keystrokes to execute.
If nil, the STEPS must be specified by a call to `demo-it-create'.

The optional ADVANCED-MODE turns on keybindings where <F12>
advances the steps instead of Space.  This mode is better for
more interactive demonstrations."
  (when (or demo-it-mode demo-it-mode-adv)
    (error "Do not start new demonstrations DURING demonstration"))

  (setq demo-it-start-winconf (current-window-configuration))
  (setq demo-it--step 0)      ;; Reset the step to the beginning
  (when steps
    (setq demo-it--steps steps)) ;; Store the steps.

  (if (or advanced-mode (eq demo-it--keymap-mode-style :advanced-mode))
      (demo-it-mode-adv t)
    (demo-it-mode t))

  (demo-it-step))

(defmacro demo-it-create (&rest forms)
  "Create and store an ordered list of steps and configuration values. The FORMS can be either function names, expressions or keywords, like :advanced-mode and :variable-width."
  `(progn
     (demo-it--set-properties (cl-remove-if-not 'keywordp '(,@forms)))
     (setq demo-it--steps     (cl-remove-if     'keywordp '(,@forms)))))

(defun demo-it--set-properties (l)
  "Sets a series of single property values from list, L."
  (mapcar 'demo-it--set-property l))

(defun demo-it-end ()
  "End the current demonstration by resetting the values
inflicted on the presentation buffer as well as closing other
windows."
  (interactive)
  (demo-it-disable-mode)
  (demo-it-presentation-quit)
  (set-window-configuration demo-it-start-winconf))

;; Next Step
;;
;;   Hitting the <F12> key should be bound to triggering the next step in
;;   the demonstration.

(defun demo-it-step (&optional step)
  "Execute the next step in the current demonstration.  Jump to a
particular STEP if the optional parameter is given, i.e. C-6 <F12>
to run the 6th step."
  (interactive "P")
  (if step
      (setq demo-it--step step)    ;; Changing Global state, yay!
    (setq demo-it--step (1+ demo-it--step)))
  (let
      ;; At this point, step is 1-based, and I need it 0-based
      ;; and f-step is the function to call for this step...
      ((f-step (nth (1- demo-it--step) demo-it--steps)))
    (if f-step
        (demo-it--execute-step f-step)
      (read-event "Demonstration finished. Hit any key to return.")
      (demo-it-end))))

(defun demo-it-restep ()
  "Execute the previous step in the current demonstration.

Useful when the previous step failed, and you want to redo it."
  (interactive)
  (let
      ;; At this point, step is 1-based, and I need it 0-based
      ;; and f-step is the function to call for this step...
      ((f-step (nth (1- demo-it--step) demo-it--steps)))
    (if f-step
        (demo-it--execute-step f-step)
      (message "Finished the entire demonstration."))))

(defun demo-it--execute-step (f-step)
  "Executes F-STEP depending on its type, e.g. expression, function, etc."
  (condition-case err
      (cond ((functionp f-step) (funcall f-step))
            ((listp     f-step) (eval f-step))     ; An expression
            ((stringp   f-step) (execute-kbd-macro (kbd f-step)))
            ((keywordp  f-step) (demo-it--set-property f-step))
            (t                  (error "invaid step: %s" f-step)))
    (error (read-event (format "Abort the demonstration because of error. Hit any key to return.\n%S" err))
           (demo-it-end))))

;; Position or advance the slide? Depends...

(defun demo-it-set-mouse-or-advance (evt)
  "Advances to the next step if clicked on the right side of any
window, otherwise, it position the point as expected.  With EVT,
function can be bound to the mouse click."
  (interactive "e")
  (if (posn-area (event-start evt))  ;; Clicked in special area?
      (demo-it-step)
    (let ((col (car (posn-col-row (event-start evt))))
          (wid (window-width (posn-window (event-start evt)))))
      (if (> col (- wid 4))
          (demo-it-step)
        (mouse-set-point evt)))))

(defun demo-it-show-step ()
  "Display the expected function to be run during the next step."
  (interactive)
  (let ((func  (nth demo-it--step demo-it--steps)))
    (message "Step: %d - Going to run: %s" demo-it--step func)))

(defun demo-it-ignore-event (evt)
  "Empty function that absorbs the EVT parameter to keep demonstration from flpping out."
  (interactive "P")
  (message ""))


;; ----------------------------------------------------------------------
;; HIDE MODELINE
;;
;;    Call the `demo-it-hide-mode-line' when displaying images and
;;    org-mode files displayed as "presentations", so that we aren't
;;    bothered by the sight of the mode.

(defvar demo-it--old-mode-line nil)
(make-variable-buffer-local 'demo-it--old-mode-line)

(defun demo-it-hide-mode-line ()
  "Hide mode line for a particular buffer."
  (interactive)
  (when mode-line-format
    (setq demo-it--old-mode-line mode-line-format)
    (setq mode-line-format nil)))

(defun demo-it-show-mode-line ()
  "Show mode line for a particular buffer, if it was previously hidden with 'demo-it--hide-mode-line."
  (interactive)
  (if demo-it--old-mode-line
      (setq mode-line-format demo-it--old-mode-line)))


;; ----------------------------------------------------------------------
;; SIDE WINDOWS
;;
;;    Typically, we make a side window that is large enough to have some
;;    fun in, as the main window would serve as little more than an
;;    outline.

(defun demo-it--make-side-window (&optional side)
  "Splits window horizontally and selects other window.

SIDE is either :below or :side and defaults to the value of
`demo-it--side-windows'."
  (if (null side)
      (setq side demo-it--side-windows))

  (select-window (if (or (eq side 'below) (eq side :below))
                     (split-window-vertically)
                   (split-window-horizontally))))

;; Since the `make-side-window' shouldn't be called, it has two
;; dashes, but to maintain backward compatibility, we make an alias:
(define-obsolete-function-alias 'demo-it-make-side-window
  'demo-it--make-side-window "2016-Oct")

;; Load a File in the Side Window
;;
;;    Splits the window and loads a file on the right side of the screen.

(defun demo-it-load-file (file &optional side size)
  "Splits window and load FILE in other buffer.

SIDE can be :below (for vertical split) or :side (for
horizontal), and defaults to the customized value of
`demo-it--side-windows'.

SIZE can specify the text font scale, and if `nil', it uses the value of ,
which defaults to 1 step larger.  This function is called with
source code since the mode line is still shown."
  (demo-it-make-side-window side)
  (find-file file)
  (if size (text-scale-set size)
           (text-scale-set demo-it--text-scale)))

(defun demo-it--get-section (type &optional start end)
  "Return tuple of beginning and end of a section of buffer.

If TYPE is :char or 'char, START and END refers to specific
character positions, but if TYPE is :line or 'line, this returns
the point positions as if START and END are line numbers."

  ;; Due to the way we call this function, we need to allow start and
  ;; end to be null, and if so, we select the entire buffer.
  (when (or (null start) (null end))
    (setq type :char)
    (setq start (point-min))
    (setq end   (point-max)))

  (when (or (eq type :line) (eq type 'line))
    (save-excursion
      ;; Re-implementing `goto-line' for the win!
      (goto-char (point-min)) (forward-line (1- start))
      (setq start (point))
      (goto-char (point-min)) (forward-line end)
      (setq end (point))))
  (cons start end))

(defun demo-it-load-part-file (file type start end &optional side size)
  "Splits window and loads FILE, but narrow to particular region.

If TYPE is set to :line, then START and END refers to the first
and last lines to narrow. If TYPE is set to :char, then START and
END refer to specific character positions.

See `demo-it-load-file' for an explanation of SIDE and SIZE.
Also see `demo-it-load-fancy-file' for an alternative version."
  (demo-it-load-file file side size)
  (let ((positions (demo-it--get-section type start end)))
    (narrow-to-region (car positions) (cdr positions))))

(defun demo-it-show-image (file &optional side)
  "Load FILE as image (or any other special file) in another
window without a mode line or fringe.  SIDE can be either :side
or :below, and if `nil', the default is to use the value of
`demo-it--side-windows'."
  (demo-it-load-file file side)
  (fringe-mode '(0 . 0))
  (demo-it-hide-mode-line))

;; Compare and Contrast Files
;;
;;   Places two files next to each other so that you can diff
;;   or at least visually compare them. I suppose that after
;;   they are loaded, you can switch to them with something like:
;;      (pop-to-buffer "example.py")
;;   To further manipulate them.

(defun demo-it-compare-files (file1 file2 &optional side size)
  "Load FILE1 and FILE2 as either two windows on top of each
other on the side of the screen, or two windows below (depending
on the value of SIDE).  The SIZE specifies the text scaling of
both buffers."
  (if (null side)
      (setq side demo-it--side-windows))

  (if (or (eq side 'below) (eq side :below))
      (progn
        (demo-it-load-file file1 :below size)
        (demo-it-load-file file2 :side size))
    (progn
      (demo-it-load-file file1 :side size)
      (demo-it-load-file file2 :below size))))

;; ----------------------------------------------------------------------
;; SHELL WORK
;;
;;    Kick off a shell in another window, change to a particular
;;    directory, and automatically run something.

(defun demo-it-start-shell (&optional directory command name side size)
  "Start a shell or eshell instance, and change to DIRECTORY to
execute COMMAND.  NAME optionally labels the buffer.  SIDE can be
either 'below or to the 'side, and SIZE specifies the text scale,
which defaults to 1 level larger."
  (let ((title (demo-it--shell-buffer-name name)))
    (demo-it-make-side-window side)

    (if (eq demo-it--shell-or-eshell :shell)
        (shell title)

      (eshell "new")
      (rename-buffer title))

    (if size (text-scale-set size)
      (text-scale-set 1))

    (when directory
      (insert (concat "cd " directory))
      (eshell-send-input))
    (erase-buffer)
    (eshell-send-input)

    (when command
      (demo-it-insert-shell command))))

(define-obsolete-function-alias 'demo-it-start-eshell 'demo-it-start-shell "2016-Oct")

(defun demo-it--shell-buffer-name (name)
  "Return the buffer NAME for the shell or eshell window."
  (if name
      (concat "Shell: " name)
    "Shell"))

(defun demo-it-insert-shell (command)
  "Inserts some text in the given shell or eshell."
  (demo-it-insert command)
  (eshell-send-input))

(defun demo-it-insert (str)
  "Insert STR into the current buffer as if you were typing it by hand."
  (let ((timings (demo-it--get-insert-text-speed)))

    (if (eq timings :instant)
        (insert str)
      (let ((bottom-limit (car timings))
            (top-limit    (cdr timings)))

        ;; If we are not inserting instantaneously, then loop over each
        ;; character in the string with a random delay based on this range:
        (dolist (ch (string-to-list str))
          (insert ch)
          (let ((tm  (+ (/ bottom-limit 1000.0)
                        (/ (random top-limit) 1000.0))))
            (sit-for tm)))))))

(defun demo-it-run-in-shell (command &optional name)
  "Run shell command COMMAND in a previously initialized Eshell.
If NAME is not specified, it defaults to `Shell'."
  (switch-to-buffer (demo-it--shell-buffer-name name))
  (demo-it-insert-shell command))

(define-obsolete-function-alias 'demo-it-run-in-eshell 'demo-it-run-in-shell "2016-Oct")
(define-obsolete-function-alias 'demo-it-type-in-eshell 'demo-it-run-in-shell "2016-Oct")

(defun demo-it-show-shell (&optional name side)
  "Call if the shell window of a given NAME has been
hidden. Optionally specify the SIDE (either 'below or 'side)."
  (demo-it-make-side-window side)
  (switch-to-buffer (demo-it--shell-buffer-name name)))

(define-obsolete-function-alias 'demo-it-show-eshell 'demo-it-show-shell "2016-Oct")


;; TITLE DISPLAY
;;
;;    Create a file to serve as a "title" as it will be displayed with a
;;    larger-than-life font and make Emacs not look like Emacs.

(defun demo-it-title-screen (file &optional size)
  "Display FILE to serve as the demonstration's title, as it will
be displayed with a larger-than-life font without a mode line,
etc.  SIZE specifies the text scale, which ignores the
`demo-it--text-scale' customization setting and defaults to 5x."
  (delete-other-windows)
  (fringe-mode '(0 . 0))

  (find-file file)
  (show-all)
  (demo-it-hide-mode-line)
  (setq cursor-type nil)
  (if (fboundp 'flyspell-mode)
      (flyspell-mode -1))
  (variable-pitch-mode 1)
  (if size (text-scale-set size)
    (text-scale-set 5))

  ;; Time to brag a wee bit...
  (message "%s" "† This presentation is running within Emacs."))


;; ----------------------------------------------------------------------
;; DEALING WITH FRAME
;;
;;    During a demonstration, it might be nice to toggle between
;;    full screen and "regular window" in a programmatic way:

(defun demo-it-toggle-fullscreen ()
  "Toggle the frame between full screen and normal size."
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; We can force the window to be full screen:

(defun demo-it-frame-fullscreen ()
  "Set the frame window to cover the full screen."
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

;; Let's make a right-side frame window:

(defun demo-it-frame-leftside ()
  "Set the window frame to be exactly half the physical display screen, and place it on the left side of the screen.  This can be helpful when showing off some other application."
  (interactive)
  (let* ((full-pixels (- (x-display-pixel-width) 16))
         (full-width  (/ full-pixels (frame-char-width)))
         (dest-width (/ full-width 2)))
    (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'width dest-width)
    (set-frame-parameter nil 'left 0)))

;; Temporary variables
;;
;;    Set variables during a demonstration.
;;    They are restored after the demonstration.

(defvar demo-it--setq-tempvars (make-hash-table))
(defvar demo-it--setq-voidvars nil)
(defvar demo-it--setq-nilvars nil)
(defun demo-it--setq (l)
  (cl-case (length l)
    (0)
    (1
     (error "Argument length is odd"))
    (t
     (let ((name (car l))
           (var  (eval (cadr l))))
       (cond ((and (boundp name) (symbol-value name))
              (puthash name (symbol-value name) demo-it--setq-tempvars))
             ((boundp name)
              (push name demo-it--setq-nilvars))
             (t
              (push name demo-it--setq-voidvars)))
       (set name var)
       (demo-it--setq (nthcdr 2 l))))))

(defmacro demo-it-setq (&rest list)
  "Like `setq', but the values are restored to original after the demo.
Actually restored by `demo-it--setq-restore'."
  `(demo-it--setq '(,@list)))

(defun demo-it--setq-restore ()
  "Restore values of setting by `demo-it-setq'."
  (cl-loop for name being the hash-keys in demo-it--setq-tempvars using (hash-values value)
           do (set name value))
  (dolist (name demo-it--setq-nilvars) (set name nil))
  (mapc 'makunbound demo-it--setq-voidvars)
  (clrhash demo-it--setq-tempvars)
  (setq demo-it--setq-nilvars nil
        demo-it--setq-voidvars nil))

;; Helper Functions

(defvar demo-it-text-entries (make-hash-table)
  "Collection of insertable text and keys for `demo-it-insert-text'.
Strings to insert Assign a collection of characters as keys and
strings, and call the `C-c i` to insert the text string as if you
were typing it.

For instance:
   (setq demo-it-text-entries #s(hash-table data
                         (?1 \"How about that?\"
                          ?2 \"Nah, this really ain't it.\")))")

(defun demo-it-insert-text (key)
  "Insert text into the current buffer based on a single character KEY.

The text is inserted as if you were typing it.  Make sure the
`demo-it-text-entries' hash-table has been initialized with a
character to be used as a key, and the text to insert."
  (interactive "cInsert text from which key?")
  (demo-it-insert (gethash key demo-it-text-entries)))




(defun demo-it-message-keybinding (key command)
  "Display message showing the KEY keybinding and its COMMAND."
  (interactive)
  (message "Typed: '%s' Command: '%s'" key command))

;; ----------------------------------------------------------------------

(require 'demo-it-present)
(require 'demo-it-extras)

;;   As a final harrah, we need to let other files know how to include
;;   this bad child.

(provide 'demo-it)

;;; demo-it.el ends here
