---
layout:     post
title:      A fancy Emacs version
date:       2018-05-25
summary:    Display a detailed Emacs version with a custom function.
categories: emacs
---

If you like to play diligently and kindly with the Emacs community, you will end
up reporting bugs or asking for new features to your favourite package
maintainers.

My skills at bug reporting have improved over the years. From the angry *“it
doesn’t work, man!!1!”* to more polite questions, I am always looking for better
reports to help debug and solve the problems I encounter.

Usually, package maintainers ask you to reproduce the bug in a clean `emacs -Q`
environment. And that’s good. But they are also happy when you provide as much
information as possible about the system you are using.

That’s why I came up with a custom function to gather details about my Emacs
version and where I am running it. It all started with [this
answer](https://emacs.stackexchange.com/questions/35497/does-emacs-have-an-option-to-display-build-settings/35500#35500)
from Drew Adams on Emacs StackExchange. His code does way much more than I need,
though, so I stripped down the inessential and turned everything into this:

``` emacs-lisp
(defun mu--os-version ()
  "Call `lsb_release' to retrieve OS version."
  (replace-regexp-in-string
   "Description:\\|[\t\n\r]+" ""
   (shell-command-to-string "lsb_release -d")))

(defun mu--gnome-version ()
  "Call `gnome-shell' to retrieve GNOME version."
  (replace-regexp-in-string
   "[\t\n\r]+" ""
   (shell-command-to-string "gnome-shell --version")))

;;;###autoload
(defun mu-display-version ()
  "Display Emacs version and system details in a temporary buffer."
  (interactive)
  (let ((buffer-name "*version*"))
    (with-help-window buffer-name
      (with-current-buffer buffer-name
        (insert (emacs-version) "\n")
        (insert "\nRepository revision: " emacs-repository-version "\n")
        (when (and system-configuration-options
                   (not (equal system-configuration-options "")))
          (insert "\nConfigured using:\n"
                  system-configuration-options))
        (insert "\n\nOperating system: " (mu--os-version) "\n")
        (insert "Window system: " (getenv "XDG_SESSION_TYPE") "\n")
        (insert "Desktop environment: " (mu--gnome-version))))))
```

By using <kbd>M-x mu-display-version</kbd> I now get a handy temporary buffer
with all the details I want.

<figure>
    <img src="/images/version.png">
</figure>

A simple <kbd>q</kbd> will close the buffer, and I am pretty sure I can add more
useful stuff in there. For now, it suffices.
