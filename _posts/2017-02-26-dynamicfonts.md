---
layout:     post
title:      Dynamically change font size in Emacs
date:       2017-02-26
summary:    How to set font size automatically based on screen resolution.
categories: emacs
---

I take my Emacs configuration with me on every computer I use. I also plug a big
external display —and the awesome Das Keyboard 4 Ultimate— during long coding
sessions, so it is only natural that I have to tailor my setup to accommodate
different font sizes.

Being the customizable editor that Emacs is, it wasn’t hard to devise a neat
trick to please my needs.

First, a simple function to set the default fonts.

``` emacs-lisp
(defun mu-setup-main-fonts (default-height variable-pitch-height)
  "Set up default fonts.

Use DEFAULT-HEIGHT for default face and VARIABLE-PITCH-HEIGHT
for variable-pitch face."
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans"
                      :height variable-pitch-height
                      :weight 'regular))
```

Now I just have to call this function with the proper values for `:height`
according to the screen size.

``` emacs-lisp
(when window-system
  (if (> (x-display-pixel-width) 1800)
      (mu-setup-main-fonts 150 160)
    (mu-setup-main-fonts 130 140)))
```

Of course, more specific settings for various display resolutions are just a
`cond` away.
