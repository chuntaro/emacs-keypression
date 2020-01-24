# Keystroke visualizer for GUI version Emacs

This package is a keystroke visualizer for GUI version Emacs.  
You no longer need to use external tools to display keystrokes when creating screencasts!

![emacs-keypression](https://raw.githubusercontent.com/wiki/chuntaro/emacs-keypression/images/screencast.gif)

# Usage
Add the following to your .emacs.

```emacs-lisp
(require 'keypression)
```

Then, if you want to display keystrokes, do the following:

```
M-x keypression-mode
```

Run it again to turn it off.

The default is a simple display, so you can customize it as follows.

```emacs-lisp
(setq keypression-use-child-frame nil
      keypression-fade-out-delay 1.0
      keypression-frame-justify 'keypression-left-justified
      keypression-cast-command-name t
      keypression-cast-command-name-format "%s  %s"
      keypression-combine-same-keystrokes t
      keypression-font-face-attribute '(:width normal :height 200 :weight bold))
```

![emacs-keypression](https://raw.githubusercontent.com/wiki/chuntaro/emacs-keypression/images/screencast2.gif)
