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
