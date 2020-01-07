# Keystroke visualizer for GUI version Emacs

This package is a keystroke visualizer for GUI version Emacs.  
You no longer need to use external tools to display keystrokes when creating screencasts!

![emacs-keycaster](https://raw.githubusercontent.com/wiki/chuntaro/emacs-keycaster/images/screencast.gif)

# Usage
Add the following to your .emacs.

```emacs-lisp
(require 'keycaster)
```

Then, if you want to display keystrokes, do the following:

```
M-x keycaster-mode
```

Run it again to turn it off.

The default is a simple display, so you can customize it as follows.

```emacs-lisp
(setq keycaster-use-child-frame nil
      keycaster-fade-out-delay 1.0
      keycaster-frame-justify 'keycaster-left-justified
      keycaster-cast-command-name t
      keycaster-cast-command-name-format "%s  %s"
      keycaster-combine-same-keystrokes t
      keycaster-font-face-attribute '(:width normal :height 200 :weight bold))
```
