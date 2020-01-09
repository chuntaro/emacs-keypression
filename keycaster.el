;;; keycaster.el --- Keystroke visualizer            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; Keywords: key, screencast, tools
;; Version: 0.9.0
;; Homepage: https://github.com/chuntaro/emacs-keycaster
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
;; https://github.com/chuntaro/emacs-keycaster
;;
;; Usage:
;; Add the following to your .emacs.
;;
;; (require 'keycaster)
;;
;; Then, if you want to display keystrokes, do the following:
;;
;; M-x keycaster-mode
;;
;; Run it again to turn it off.
;;
;; The default is a simple display, so you can customize it as follows.
;;
;; (setq keycaster-use-child-frame nil
;;       keycaster-fade-out-delay 1.0
;;       keycaster-frame-justify 'keycaster-left-justified
;;       keycaster-cast-command-name t
;;       keycaster-cast-command-name-format "%s  %s"
;;       keycaster-combine-same-keystrokes t
;;       keycaster-font-face-attribute '(:width normal :height 200 :weight bold))

;;; Code:

(require 'subr-x)
(require 'css-mode)                     ; for css--contrasty-color

(defgroup keycaster nil
  "Keystroke visualizer for GUI version Emacs."
  :group 'tools)

(defcustom keycaster-frames-maxnum 10
  "Maximum number of keystrokes to display."
  :type 'integer
  :group 'keycaster)

(defcustom keycaster-use-child-frame (eq system-type 'windows-nt)
  "Whether to display keystrokes in child-frame.

Note: The child-frame is broken in GTK3 version.
--with-toolkit=no version has no problem."
  :type 'boolean
  :group 'keycaster)

(defcustom keycaster-cast-command-name nil
  "Whether to display command names in addition to keystrokes."
  :type 'boolean
  :group 'keycaster)

(defcustom keycaster-cast-command-name-format "%s %s"
  "Format for displaying command names."
  :type 'string
  :group 'keycaster)

(defcustom keycaster-frame-justify 'keycaster-right-justified
  "Justification."
  :type '(choice (const :tag "Right justified" keycaster-right-justified)
                 (const :tag "Left justified" keycaster-left-justified))
  :group 'keycaster)

(defcustom keycaster-concat-self-insert t
  "Concatenate self-insert-command keystrokes."
  :type 'boolean
  :group 'keycaster)

(defcustom keycaster-combine-same-keystrokes nil
  "Combine consecutive same keystrokes."
  :type 'boolean
  :group 'keycaster)

(defcustom keycaster-combine-format "%s #%d"
  "Format to combine keystrokes."
  :type 'boolean
  :group 'keycaster)

(defcustom keycaster-fade-out-delay 0.5
  "Number of seconds before fade out starts."
  :type 'number
  :group 'keycaster)

(defcustom keycaster-fade-out-seconds 0.2
  "Number of seconds until fade out ends."
  :type 'number
  :group 'keycaster)

(defcustom keycaster-fade-out-fps 20
  "Fade out frames per second."
  :type 'number
  :group 'keycaster)

(defcustom keycaster-foreground-for-light-mode "white"
  "Keycaster foreground color when Emacs is light background."
  :type 'color
  :group 'keycaster)

(defcustom keycaster-background-for-light-mode "black"
  "Keycaster background color when Emacs is light background."
  :type 'color
  :group 'keycaster)

(defcustom keycaster-foreground-for-dark-mode "#404040"
  "Keycaster foreground color when Emacs is dark background."
  :type 'color
  :group 'keycaster)

(defcustom keycaster-background-for-dark-mode "AntiqueWhite"
  "Keycaster background color when Emacs is dark background."
  :type 'color
  :group 'keycaster)

(defcustom keycaster-frame-background-mode nil
  "Emacs background color mode.
The value is `light', `dark', or `nil', and if nil, it is detected automatically."
  :type '(choice (const light) (const dark) (const nil))
  :group 'keycaster)

(defcustom keycaster-font nil
  "Set when you want to specify the font."
  :type '(choice (const nil) string)
  :group 'keycaster)

(defcustom keycaster-font-face-attribute '(:width normal :height 300 :weight bold)
  "Set when you want to specify the font size, etc.
See `set-face-attribute' help for details."
  :type '(plist)
  :group 'keycaster)

(defcustom keycaster-left-fringe 8
  "Left margin of string."
  :type 'integer
  :group 'keycaster)

(defcustom keycaster-right-fringe 8
  "Right margin of string"
  :type 'integer
  :group 'keycaster)

(defcustom keycaster-frame-gap 8
  "Gap between Keycaster frames."
  :type 'integer
  :group 'keycaster)

(defcustom keycaster-frame-origin 'keycaster-origin-bottom-right
  "Position of origin when displaying Keycaster frame."
  :type '(choice (const keycaster-origin-bottom-right)
                 (const keycaster-origin-top-left))
  :group 'keycaster)

(defcustom keycaster-x-offset 8
  "Horizontal offset from the specified origin."
  :type 'integer
  :group 'keycaster)

(defcustom keycaster-y-offset 4
  "Vertical offset from the specified origin."
  :type 'integer
  :group 'keycaster)

(defcustom keycaster-space-substitution-string "␣"
  "Blank symbol. Specify \"SPC\" etc."
  :type 'string
  :group 'keycaster)

;;; Global variables

(defvar keycaster--nactives 0)

(defvar keycaster--frames nil)
(defvar keycaster--buffers nil)
(defvar keycaster--strings nil)
(defvar keycaster--heights nil)

(defvar keycaster--fade-out-delay 0)
(defvar keycaster--fade-out-timer nil)

(defvar keycaster--nmatches 1)
(defvar keycaster--last-keystrokes "")
(defvar keycaster--self-insert-string "")

(defvar keycaster--prev-frame-alpha-lower-limit 20)

;;; Functions

(defun keycaster--light-background-p ()
  (if keycaster-frame-background-mode
      (not (eq keycaster-frame-background-mode 'dark))
    (string= (css--contrasty-color (frame-parameter (selected-frame) 'background-color))
             "black")))

(defsubst keycaster--set-frame-alpha (frame alpha)
  (modify-frame-parameters frame `((alpha . ,alpha))))

(defun keycaster--fade-out ()
  (when-let (frame (and keycaster--frames
                        (< 0 keycaster--nactives)
                        (< (cl-decf keycaster--fade-out-delay (/ 1.0 keycaster-fade-out-fps)) 0)
                        (aref keycaster--frames (1- keycaster--nactives))))
    (let* ((delta (/ 1.0 (* keycaster-fade-out-fps keycaster-fade-out-seconds)))
           (alpha (- (frame-parameter frame 'alpha) delta)))
      (if (< 0.0 alpha)
          (keycaster--set-frame-alpha frame alpha)
        (keycaster--set-frame-alpha frame 0.0)
        (if (< 0 keycaster--nactives)
            (cl-decf keycaster--nactives)
          (setq keycaster--nmatches 1
                keycaster--last-keystrokes ""))))))

(defun keycaster--create-fade-out-timer ()
  (setq keycaster--fade-out-timer
        (run-at-time nil
                     (/ 1.0 keycaster-fade-out-fps)
                     #'keycaster--fade-out)))

(defun keycaster--frame-position-x (i)
  (let ((bottom-right (eq keycaster-frame-origin 'keycaster-origin-bottom-right))
        (right-justified (eq keycaster-frame-justify 'keycaster-right-justified)))
    (if keycaster-use-child-frame
        (if bottom-right
            (- keycaster-x-offset)
          keycaster-x-offset)
      (let ((frame (aref keycaster--frames i))
            (fx (car (frame-position))))
        (if bottom-right
            (if right-justified
                (max 0 (- (+ fx (frame-pixel-width))
                          (frame-pixel-width frame)
                          keycaster-x-offset))
              (- (+ fx (frame-pixel-width))
                 keycaster-x-offset))
          (if right-justified
              (- (+ fx keycaster-x-offset)
                 (frame-pixel-width frame))
            (max 0 (+ fx keycaster-x-offset))))))))

(defun keycaster--frame-position-y (i)
  (let* ((frame-height (frame-pixel-height (aref keycaster--frames i)))
         (bottom-right (eq keycaster-frame-origin 'keycaster-origin-bottom-right))
         (y (aset keycaster--heights i (if bottom-right
                                           (+ (if (zerop i)
                                                  keycaster-y-offset
                                                (+ (aref keycaster--heights (1- i))
                                                   keycaster-frame-gap))
                                              frame-height)
                                         (if (zerop i)
                                             keycaster-y-offset
                                           (+ (aref keycaster--heights (1- i))
                                              keycaster-frame-gap
                                              frame-height))))))
    (if keycaster-use-child-frame
        (if bottom-right (- y) y)
      (let ((fy (cdr (frame-position))))
        (if bottom-right
            (- (+ fy (frame-pixel-height))
               y)
          (+ fy y))))))

(defun keycaster--set-frame-string (i string)
  (aset keycaster--strings i string)
  (with-current-buffer (aref keycaster--buffers i)
    (erase-buffer)
    (insert string))
  (let ((window-resize-pixelwise t)
        (frame-resize-pixelwise t)
        (window-min-width 0)
        (window-min-height 0)
        (frame (aref keycaster--frames i)))
    (fit-frame-to-buffer frame nil 0 nil 0)
    ;; (set-frame-width frame (string-width string))
    ))

(defsubst keycaster--shift-frame-string ()
  (let ((n (min keycaster--nactives (1- keycaster-frames-maxnum))))
    (cl-loop for i from (1- n) downto 0 do
             (keycaster--set-frame-string (1+ i) (aref keycaster--strings i)))))

(defsubst keycaster--set-position-active-frames (&optional first-only)
  (dotimes (i keycaster--nactives)
    (let ((frame (aref keycaster--frames i)))
      (set-frame-position frame
                          (keycaster--frame-position-x i)
                          (keycaster--frame-position-y i))
      (when (or (not first-only)
                (= i 0))
        (keycaster--set-frame-alpha frame 1.0))
      (unless (frame-visible-p frame)
        (make-frame-visible frame)))))

(defun keycaster--push-string (keys)
  (let* ((string (if (and keycaster-cast-command-name
                          this-command)
                     (format keycaster-cast-command-name-format
                             keys this-command)
                   keys))
         (self-insert (and keycaster-concat-self-insert
                           (eq this-command 'self-insert-command)))
         (same-key (and keycaster-combine-same-keystrokes
                        (string= keys keycaster--last-keystrokes))))
    (cond
     ((or self-insert same-key)
      ;; Just rewrite the bottom line.
      (let ((str (if self-insert
                     (setq keycaster--self-insert-string
                           (concat keycaster--self-insert-string keys))
                   (format keycaster-combine-format
                           string
                           (cl-incf keycaster--nmatches)))))
        (keycaster--set-frame-string 0 str)
        (when (zerop keycaster--nactives)
          (cl-incf keycaster--nactives))
        (when (< keycaster--nactives 2)
          (setq keycaster--fade-out-delay keycaster-fade-out-delay))
        (keycaster--set-position-active-frames t)))
     (t
      (setq keycaster--nmatches 1
            keycaster--self-insert-string "")
      (keycaster--shift-frame-string)
      (keycaster--set-frame-string 0 string)
      (setq keycaster--fade-out-delay keycaster-fade-out-delay)
      (when (< keycaster--nactives keycaster-frames-maxnum)
        (cl-incf keycaster--nactives))
      (keycaster--set-position-active-frames)))
    (setq keycaster--last-keystrokes keys)))

(defsubst keycaster--modifiers-to-string (modifiers)
  (mapconcat (lambda (mod)
               (cl-case mod
                 (meta "M-")
                 (control "C-")
                 (shift "S-")
                 (hyper "h-")
                 (super "s-")
                 (alt "A-")
                 (otherwise "")))
             modifiers
             ""))

(defsubst keycaster--char-to-string (char)
  (cl-case char
    (#x20                               ; space
     keycaster-space-substitution-string)
    (otherwise
     (char-to-string char))))

(defun keycaster--keys-to-string (keys)
  (mapconcat #'identity
             (cl-mapcan (lambda (key)
                          (when-let (type (event-basic-type key))
                            (list (concat (keycaster--modifiers-to-string
                                           (event-modifiers key))
                                          (cond
                                           ((symbolp type)
                                            (symbol-name type))
                                           ((integerp type)
                                            (keycaster--char-to-string type)))))))
                        keys)
             " "))

(cl-defun keycaster--create-frame (buffer-or-name
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
                                   respect-mode-line)
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
        (set-window-buffer window buffer)
        (set-window-dedicated-p window t))
      frame)))

(defun keycaster-pre-command ()
  (keycaster--push-string (keycaster--keys-to-string (this-command-keys-vector))))

(defun keycaster-finalize ()
  (interactive)
  (setq frame-alpha-lower-limit keycaster--prev-frame-alpha-lower-limit)
  (remove-hook 'pre-command-hook 'keycaster-pre-command)
  (when keycaster--fade-out-timer
    (cancel-timer keycaster--fade-out-timer))
  (cl-flet ((mapc-when (array func &rest args)
                       (mapc (lambda (elem)
                               (when elem
                                 (apply func elem args)))
                             array)))
    (mapc-when keycaster--buffers #'kill-buffer)
    (mapc-when keycaster--frames #'delete-frame t))
  (setq keycaster--fade-out-timer nil
        keycaster--frames nil
        keycaster--buffers nil
        keycaster--strings nil
        keycaster--heights nil)
  (setq keycaster--nactives 0))

(defun keycaster--create-arrays ()
  (dolist (array '(keycaster--frames
                   keycaster--buffers
                   keycaster--strings
                   keycaster--heights))
    (unless (symbol-value array)
      (set array (make-vector keycaster-frames-maxnum nil)))))

(defun keycaster-workable-p ()
  "Test keycaster workable status."
  (and (>= emacs-major-version 26)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(defun keycaster-initialize ()
  (interactive)
  (unless (keycaster-workable-p)
    (error "Keycaster: Not GUI version Emacs"))
  (keycaster-finalize)
  (setq keycaster--prev-frame-alpha-lower-limit frame-alpha-lower-limit
        frame-alpha-lower-limit 0.0)
  (keycaster--create-arrays)
  (keycaster--create-fade-out-timer)
  (add-hook 'kill-emacs-hook #'keycaster-finalize)
  (let* ((parent-frame (when keycaster-use-child-frame (window-frame (selected-window))))
         (fg (if (keycaster--light-background-p)
                 keycaster-foreground-for-light-mode
               keycaster-foreground-for-dark-mode))
         (bg (if (keycaster--light-background-p)
                 keycaster-background-for-light-mode
               keycaster-background-for-dark-mode)))
    (dotimes (i keycaster-frames-maxnum)
      (with-current-buffer (get-buffer-create (format " *keycaster-%d*" i))
        (with-selected-frame (keycaster--create-frame
                              (current-buffer)
                              :override-parameters (if keycaster-use-child-frame
                                                       `((parent-frame . ,parent-frame)
                                                         (font . ,keycaster-font))
                                                     `((parent-frame . ,parent-frame)
                                                       (z-group . above)
                                                       (font . ,keycaster-font)))
                              :foreground-color fg
                              :background-color bg
                              :left-fringe keycaster-left-fringe
                              :right-fringe keycaster-right-fringe)
          (keycaster--set-frame-alpha (selected-frame) 0.0)
          (aset keycaster--frames i (selected-frame))
          (aset keycaster--buffers i (current-buffer))
          (apply #'set-face-attribute 'default (selected-frame) keycaster-font-face-attribute)
          (set-face-attribute 'fringe (selected-frame) :background bg)
          ;; Workaround for invisible bugs...
          (when (eq system-type 'windows-nt)
            (make-frame-visible)))))
    (raise-frame parent-frame))
  (run-at-time 0.5 nil (lambda ()
                         (add-hook 'pre-command-hook 'keycaster-pre-command))))

;;;###autoload
(define-minor-mode keycaster-mode
  "Keystroke visualizer for GUI version Emacs."
  :lighter " KeyC"
  :global t
  (if keycaster-mode
      (keycaster-initialize)
    (keycaster-finalize)))

(provide 'keycaster)
;;; keycaster.el ends here
