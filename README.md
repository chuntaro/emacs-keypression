[![MELPA](https://melpa.org/packages/keypression-badge.svg)](https://melpa.org/#/keypression)

# Keystroke visualizer for GUI version Emacs

This package is a keystroke visualizer for GUI version Emacs.  
You no longer need to use external tools to display keystrokes when creating screencasts!

![emacs-keypression](https://raw.githubusercontent.com/wiki/chuntaro/emacs-keypression/images/screencast.gif)

# Install

Keypression is available in melpa.
You can install keypression via package-install <kbd>M-x package-install [ret] keypression [ret]</kbd>

# Usage
Add the following to your .emacs. (This is not necessary if you installed from MELPA)

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

To change the display position, do the following.

```emacs-lisp
(setq keypression-x-offset 100
      keypression-y-offset 100)
```

![emacs-keypression](https://raw.githubusercontent.com/wiki/chuntaro/emacs-keypression/images/screencast3.gif)

# Restriction

On Windows, ```child-frame``` works well, but on Linux (GTK+3) and macOS versions, ```child-frame``` is disabled by default because of a problem.  
If child-frame is disabled, all the input key display frames are displayed on the taskbar.  
I think there is no problem to create a screencast, but it seems to be difficult to switch applications.
