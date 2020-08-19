;;; keypression.el --- Keystroke visualizer          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; Keywords: key, screencast, tools
;; Version: 1.0.5
;; Homepage: https://github.com/chuntaro/emacs-keypression
;; Package-Requires: ((emacs "26.3"))

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

;; This package is a keystroke visualizer for GUI version Emacs.
;; You no longer need to use external tools to display keystrokes when creating screencasts!
;;
;; You can see how it works on the following page.
;; https://github.com/chuntaro/emacs-keypression
;;
;; Usage:
;; Add the following to your .emacs.
;;
;; (require 'keypression)
;;
;; Then, if you want to display keystrokes, do the following:
;;
;; M-x keypression-mode
;;
;; Run it again to turn it off.
;;
;; The default is a simple display, so you can customize it as follows.
;;
;; (setq keypression-use-child-frame nil
;;       keypression-fade-out-delay 1.0
;;       keypression-frame-justify 'keypression-left-justified
;;       keypression-cast-command-name t
;;       keypression-cast-command-name-format "%s  %s"
;;       keypression-combine-same-keystrokes t
;;       keypression-font-face-attribute '(:width normal :height 200 :weight bold))

;;; Code:

(require 'subr-x)
(eval-when-compile (require 'cl-lib))

(defgroup keypression nil
  "Keystroke visualizer for GUI version Emacs."
  :group 'tools)

(defcustom keypression-frames-maxnum 10
  "Maximum number of keystrokes to display."
  :type 'integer
  :group 'keypression)

(defcustom keypression-use-child-frame (or (eq system-type 'windows-nt)
                                           (eq window-system 'ns))
  "Whether to display keystrokes in child-frame.

Note: The child-frame is broken in GTK3 version.
--with-toolkit=no version has no problem."
  :type 'boolean
  :group 'keypression)

(defcustom keypression-cast-command-name nil
  "Whether to display command names in addition to keystrokes."
  :type 'boolean
  :group 'keypression)

(defcustom keypression-cast-command-name-format "%s %s"
  "Format for displaying command names."
  :type 'string
  :group 'keypression)

(defcustom keypression-frame-justify 'keypression-right-justified
  "Justification."
  :type '(choice (const :tag "Right justified" keypression-right-justified)
                 (const :tag "Left justified" keypression-left-justified))
  :group 'keypression)

(defcustom keypression-concat-self-insert t
  "Concatenate `self-insert-command' keystrokes."
  :type 'boolean
  :group 'keypression)

(defcustom keypression-concat-digit-argument t
  "Concatenate `digit-argument' keystrokes."
  :type 'boolean
  :group 'keypression)

(defcustom keypression-combine-same-keystrokes nil
  "Combine consecutive same keystrokes."
  :type 'boolean
  :group 'keypression)

(defcustom keypression-combine-format "%s #%d"
  "Format to combine keystrokes."
  :type 'boolean
  :group 'keypression)

(defcustom keypression-fade-out-delay 0.5
  "Number of seconds before fade out starts."
  :type 'number
  :group 'keypression)

(defcustom keypression-fade-out-seconds 0.2
  "Number of seconds until fade out ends."
  :type 'number
  :group 'keypression)

(defcustom keypression-fade-out-fps 20
  "Fade out frames per second."
  :type 'number
  :group 'keypression)

(defcustom keypression-foreground-for-light-mode "white"
  "Keypression foreground color when Emacs is light background."
  :type 'color
  :group 'keypression)

(defcustom keypression-background-for-light-mode "black"
  "Keypression background color when Emacs is light background."
  :type 'color
  :group 'keypression)

(defcustom keypression-foreground-for-dark-mode "#404040"
  "Keypression foreground color when Emacs is dark background."
  :type 'color
  :group 'keypression)

(defcustom keypression-background-for-dark-mode "AntiqueWhite"
  "Keypression background color when Emacs is dark background."
  :type 'color
  :group 'keypression)

(defcustom keypression-frame-background-mode nil
  "Emacs background color mode.
The value is `light', `dark', or nil, and if nil, it is detected automatically."
  :type '(choice (const light) (const dark) (const nil))
  :group 'keypression)

(defcustom keypression-font nil
  "Set when you want to specify the font."
  :type '(choice (const nil) string)
  :group 'keypression)

(defcustom keypression-font-face-attribute '(:width normal :height 300 :weight bold)
  "Set when you want to specify the font size, etc.
See `set-face-attribute' help for details."
  :type '(plist)
  :group 'keypression)

(defcustom keypression-left-fringe 8
  "Left margin of string."
  :type 'integer
  :group 'keypression)

(defcustom keypression-right-fringe 8
  "Right margin of string."
  :type 'integer
  :group 'keypression)

(defcustom keypression-frame-gap 8
  "Gap between Keypression frames."
  :type 'integer
  :group 'keypression)

(defcustom keypression-frame-origin 'keypression-origin-bottom-right
  "Position of origin when displaying Keypression frame."
  :type '(choice (const keypression-origin-bottom-right)
                 (const keypression-origin-top-left))
  :group 'keypression)

(defcustom keypression-x-offset 8
  "Horizontal offset from the specified origin."
  :type 'integer
  :group 'keypression)

(defcustom keypression-y-offset 4
  "Vertical offset from the specified origin."
  :type 'integer
  :group 'keypression)

(defcustom keypression-space-substitution-string "␣"
  "Blank symbol.  Specify \"SPC\" etc."
  :type 'string
  :group 'keypression)

(defcustom keypression-ignore-mouse-events '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 mouse-movement wheel-up wheel-down)
  "List of mouse events to ignore."
  :type '(set (const mouse-1)
              (const mouse-2)
              (const mouse-3)
              (const mouse-4)
              (const mouse-5)
              (const mouse-movement)
              (const wheel-up)
              (const wheel-down))
  :group 'keypression)

;;; Global variables

(defvar keypression--nactives 0)

(defvar keypression--frames nil)
(defvar keypression--buffers nil)
(defvar keypression--strings nil)
(defvar keypression--heights nil)

(defvar keypression--fade-out-delay 0)
(defvar keypression--fade-out-delay-bottom-line 0)
(defvar keypression--fade-out-timer nil)

(defvar keypression--nmatches 1)
(defvar keypression--last-keystrokes "")
(defvar keypression--last-command nil)
(defvar keypression--last-command-2 nil)
(defvar keypression--concat-string "")

(defvar keypression--prev-frame-alpha-lower-limit 20)

;;; Functions

(defsubst keypression--contrasty-color (name)
  (if (> (color-distance name "black") 292485)
      "black" "white"))

(defun keypression--light-background-p ()
  (if keypression-frame-background-mode
      (not (eq keypression-frame-background-mode 'dark))
    (string= (keypression--contrasty-color (frame-parameter (selected-frame) 'background-color))
             "black")))

(defsubst keypression--set-frame-alpha (frame alpha)
  (modify-frame-parameters frame `((alpha . ,alpha))))

(defmacro keypression--decrement (fade-out-delay)
  `(cl-decf ,fade-out-delay (/ 1.0 keypression-fade-out-fps)))

(defun keypression--fade-out ()
  (keypression--decrement keypression--fade-out-delay-bottom-line)
  (when (and (= 1 keypression--nactives)
             (< 0 keypression--fade-out-delay-bottom-line))
    (setq keypression--fade-out-delay keypression--fade-out-delay-bottom-line
          keypression--fade-out-delay-bottom-line 0))
  (when-let (frame (and keypression--frames
                        (< 0 keypression--nactives)
                        (< (keypression--decrement keypression--fade-out-delay) 0)
                        (aref keypression--frames (1- keypression--nactives))))
    (let* ((delta (/ 1.0 (* keypression-fade-out-fps keypression-fade-out-seconds)))
           (alpha (- (frame-parameter frame 'alpha) delta)))
      (if (< 0.0 alpha)
          (keypression--set-frame-alpha frame alpha)
        (keypression--set-frame-alpha frame 0.0)
        (if (< 0 keypression--nactives)
            (cl-decf keypression--nactives)
          (setq keypression--nmatches 1
                keypression--last-keystrokes ""))))))

(defun keypression--create-fade-out-timer ()
  (setq keypression--fade-out-timer
        (run-at-time nil
                     (/ 1.0 keypression-fade-out-fps)
                     #'keypression--fade-out)))

(defun keypression--frame-position-x (i)
  (let ((bottom-right (eq keypression-frame-origin 'keypression-origin-bottom-right))
        (right-justified (eq keypression-frame-justify 'keypression-right-justified)))
    (if keypression-use-child-frame
        (if bottom-right
            (- keypression-x-offset)
          keypression-x-offset)
      (let ((frame (aref keypression--frames i))
            (fx (car (frame-position))))
        (if bottom-right
            (if right-justified
                (max 0 (- (+ fx (frame-pixel-width))
                          (frame-pixel-width frame)
                          keypression-x-offset))
              (- (+ fx (frame-pixel-width))
                 keypression-x-offset))
          (if right-justified
              (- (+ fx keypression-x-offset)
                 (frame-pixel-width frame))
            (max 0 (+ fx keypression-x-offset))))))))

(defun keypression--frame-position-y (i)
  (let* ((frame-height (frame-pixel-height (aref keypression--frames i)))
         (bottom-right (eq keypression-frame-origin 'keypression-origin-bottom-right))
         (y (aset keypression--heights i (if bottom-right
                                             (+ (if (zerop i)
                                                    keypression-y-offset
                                                  (+ (aref keypression--heights (1- i))
                                                     keypression-frame-gap))
                                                frame-height)
                                           (if (zerop i)
                                               keypression-y-offset
                                             (+ (aref keypression--heights (1- i))
                                                keypression-frame-gap
                                                frame-height))))))
    (if keypression-use-child-frame
        (if bottom-right (- y) y)
      (let ((fy (cdr (frame-position))))
        (if bottom-right
            (- (+ fy (frame-pixel-height))
               y)
          (+ fy y))))))

(defun keypression--set-frame-string (i string)
  (aset keypression--strings i string)
  (with-current-buffer (aref keypression--buffers i)
    (erase-buffer)
    (insert string))
  (let ((window-resize-pixelwise t)
        (frame-resize-pixelwise t)
        (window-min-width 0)
        (window-min-height 0)
        (frame (aref keypression--frames i)))
    (fit-frame-to-buffer frame nil 0 nil 0)
    ;; (set-frame-width frame (string-width string))
    ))

(defsubst keypression--shift-frame-string ()
  (let ((n (min keypression--nactives (1- keypression-frames-maxnum))))
    (cl-loop for i from (1- n) downto 0 do
             (keypression--set-frame-string (1+ i) (aref keypression--strings i)))))

(defsubst keypression--set-position-active-frames (&optional first-only)
  (dotimes (i keypression--nactives)
    (let ((frame (aref keypression--frames i)))
      (set-frame-position frame
                          (keypression--frame-position-x i)
                          (keypression--frame-position-y i))
      (when (or (not first-only)
                (= i 0))
        (keypression--set-frame-alpha frame 1.0))
      (unless (frame-visible-p frame)
        (make-frame-visible frame)))))

(defsubst keypression--digit-argument-p (command)
  (and keypression-concat-digit-argument
       (memq command '(digit-argument universal-argument universal-argument-more))))

(defun keypression--same-command-p ()
  (cond
   ((keypression--digit-argument-p keypression--last-command))
   ((keypression--digit-argument-p this-command)
    (keypression--digit-argument-p keypression--last-command))
   ((and (eq this-command keypression--last-command)
         (not (keypression--digit-argument-p keypression--last-command-2))))))

(defun keypression--push-back-self-insert-string (str &optional separator)
  (cl-callf concat keypression--concat-string
    (when (and separator (< 0 (length keypression--concat-string)))
      separator)
    str))

(defun keypression--push-string (keys)
  (let* ((string (if (and keypression-cast-command-name
                          this-command)
                     (format keypression-cast-command-name-format
                             keys this-command)
                   keys))
         (self-insert (and keypression-concat-self-insert
                           (eq this-command 'self-insert-command)))
         (same-key (and keypression-combine-same-keystrokes
                        (string= keys keypression--last-keystrokes)))
         (digit-arg (keypression--digit-argument-p this-command))
         (before-digit-arg (keypression--digit-argument-p keypression--last-command)))
    (cond
     ((and (keypression--same-command-p)
           (or self-insert same-key digit-arg before-digit-arg))
      ;; Just rewrite the bottom line.
      (let ((str (cond
                  ((and self-insert (not before-digit-arg))
                   (keypression--push-back-self-insert-string keys))
                  (digit-arg
                   (keypression--push-back-self-insert-string keys " "))
                  (before-digit-arg
                   (keypression--push-back-self-insert-string string " "))
                  (t ;; same-key
                   (format keypression-combine-format
                           string
                           (cl-incf keypression--nmatches))))))
        (keypression--set-frame-string 0 str)
        (when (zerop keypression--nactives)
          (cl-incf keypression--nactives))
        (setq keypression--fade-out-delay-bottom-line keypression-fade-out-delay)
        (keypression--set-position-active-frames t)))
     (t
      (setq keypression--nmatches 1
            keypression--concat-string keys)
      (keypression--shift-frame-string)
      (keypression--set-frame-string 0 string)
      (setq keypression--fade-out-delay keypression-fade-out-delay)
      (when (< keypression--nactives keypression-frames-maxnum)
        (cl-incf keypression--nactives))
      (keypression--set-position-active-frames)))
    (setq keypression--last-keystrokes keys
          keypression--last-command-2 keypression--last-command
          keypression--last-command this-command)))

(defsubst keypression--keys-to-string (keys)
  (if (and (eq this-command 'self-insert-command)
           (string= keys " "))
      keypression-space-substitution-string
    (key-description keys)))

(defun keypression--pre-command ()
  (unless (memq (event-basic-type last-command-event) keypression-ignore-mouse-events)
    (keypression--push-string (keypression--keys-to-string (this-command-keys)))))

(cl-defun keypression--create-frame (buffer-or-name
                                     &key
                                     parent-frame
                                     foreground-color
                                     background-color
                                     left-fringe
                                     right-fringe
                                     internal-border-width
                                     internal-border-color
                                     font
                                     keep-ratio
                                     override-parameters
                                     respect-header-line
                                     respect-mode-line
                                     respect-tab-line)
  "Create a frame. Copied from posframe."
  (let ((left-fringe (or left-fringe 0))
        (right-fringe (or right-fringe 0))
        (internal-border-width (or internal-border-width 0))
        (buffer (get-buffer-create buffer-or-name))
        (after-make-frame-functions nil)
        frame)
    (with-current-buffer buffer
      ;; Many variables take effect after call `set-window-buffer'
      (setq-local display-line-numbers nil)
      (setq-local frame-title-format "")
      (setq-local left-margin-width nil)
      (setq-local right-margin-width nil)
      (setq-local left-fringe-width nil)
      (setq-local right-fringe-width nil)
      (setq-local fringes-outside-margins 0)
      (setq-local truncate-lines nil)
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local show-trailing-whitespace nil)
      (unless respect-mode-line
        (setq-local mode-line-format nil))
      (unless respect-header-line
        (setq-local header-line-format nil))
      (unless respect-tab-line
        (setq-local tab-line-format nil))
      (setq-local indicate-buffer-boundaries nil)

      (setq frame
            (make-frame
             `(,@override-parameters
               ,(when foreground-color
                  (cons 'foreground-color foreground-color))
               ,(when background-color
                  (cons 'background-color background-color))
               ,(when font
                  (cons 'font font))
               (parent-frame . ,(or parent-frame (window-frame)))
               (keep-ratio ,keep-ratio)
               (fullscreen . nil)
               (no-accept-focus . t)
               (min-width  . 0)
               (min-height . 0)
               (border-width . 0)
               (internal-border-width . ,internal-border-width)
               (vertical-scroll-bars . nil)
               (horizontal-scroll-bars . nil)
               (left-fringe . ,left-fringe)
               (right-fringe . ,right-fringe)
               (menu-bar-lines . 0)
               (tool-bar-lines . 0)
               (tab-bar-lines . 0)
               (line-spacing . 0)
               (unsplittable . t)
               (no-other-frame . t)
               (undecorated . t)
               (visibility . nil)
               (cursor-type . nil)
               (minibuffer . nil)
               (width . 1)
               (height . 1)
               (no-special-glyphs . t)
               (inhibit-double-buffering . nil)
               ;; Do not save child-frame when use desktop.el
               (desktop-dont-save . t))))
      (when internal-border-color
        (set-face-background 'internal-border
                             internal-border-color frame))
      (let ((window (frame-root-window frame)))
        ;; This method is more stable than 'setq mode/header-line-format nil'
        (unless respect-mode-line
          (set-window-parameter window 'mode-line-format 'none))
        (unless respect-header-line
          (set-window-parameter window 'header-line-format 'none))
        (unless respect-tab-line
          (set-window-parameter window 'tab-line-format 'none))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window t))
      frame)))

(defun keypression--finalize ()
  (setq frame-alpha-lower-limit keypression--prev-frame-alpha-lower-limit)
  (remove-hook 'pre-command-hook 'keypression--pre-command)
  (when keypression--fade-out-timer
    (cancel-timer keypression--fade-out-timer))
  (cl-flet ((mapc-when (array func &rest args)
                       (mapc (lambda (elem)
                               (when elem
                                 (apply func elem args)))
                             array)))
    (mapc-when keypression--buffers #'kill-buffer)
    (mapc-when keypression--frames #'delete-frame t))
  (setq keypression--fade-out-timer nil
        keypression--frames nil
        keypression--buffers nil
        keypression--strings nil
        keypression--heights nil)
  (setq keypression--nactives 0))

(defun keypression--create-arrays ()
  (dolist (array '(keypression--frames
                   keypression--buffers
                   keypression--strings
                   keypression--heights))
    (unless (symbol-value array)
      (set array (make-vector keypression-frames-maxnum nil)))))

(defun keypression--workable-p ()
  "Test keypression workable status."
  (and (>= emacs-major-version 26)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(defun keypression--initialize ()
  (unless (keypression--workable-p)
    (error "Keypression: Not GUI version Emacs"))
  (keypression--finalize)
  (setq keypression--prev-frame-alpha-lower-limit frame-alpha-lower-limit
        frame-alpha-lower-limit 0.0)
  (keypression--create-arrays)
  (keypression--create-fade-out-timer)
  (add-hook 'kill-emacs-hook #'keypression--finalize)
  (let* ((parent-frame (when keypression-use-child-frame (window-frame (selected-window))))
         (fg (if (keypression--light-background-p)
                 keypression-foreground-for-light-mode
               keypression-foreground-for-dark-mode))
         (bg (if (keypression--light-background-p)
                 keypression-background-for-light-mode
               keypression-background-for-dark-mode)))
    (dotimes (i keypression-frames-maxnum)
      (with-current-buffer (get-buffer-create (format " *keypression-%d*" i))
        (with-selected-frame (keypression--create-frame
                              (current-buffer)
                              :override-parameters (if keypression-use-child-frame
                                                       `((parent-frame . ,parent-frame)
                                                         (font . ,keypression-font))
                                                     `((parent-frame . ,parent-frame)
                                                       (z-group . above)
                                                       (font . ,keypression-font)))
                              :foreground-color fg
                              :background-color bg
                              :left-fringe keypression-left-fringe
                              :right-fringe keypression-right-fringe)
          (keypression--set-frame-alpha (selected-frame) 0.0)
          (aset keypression--frames i (selected-frame))
          (aset keypression--buffers i (current-buffer))
          (apply #'set-face-attribute 'default (selected-frame) keypression-font-face-attribute)
          (set-face-attribute 'fringe (selected-frame) :background bg)
          ;; Workaround for invisible bugs...
          (when (eq system-type 'windows-nt)
            (make-frame-visible)))))
    (raise-frame parent-frame))
  (run-at-time 0.5 nil (lambda ()
                         (add-hook 'pre-command-hook 'keypression--pre-command))))

;;;###autoload
(define-minor-mode keypression-mode
  "Keystroke visualizer for GUI version Emacs."
  :lighter " KeyC"
  :global t
  (if keypression-mode
      (keypression--initialize)
    (keypression--finalize)))

(provide 'keypression)
;;; keypression.el ends here
