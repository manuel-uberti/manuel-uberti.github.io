---
layout:     post
title:      What’s the theme?
date:       2018-06-01
summary:    A function to find the active Emacs colour theme name.
categories: emacs
---

[Last time](https://manuel-uberti.github.io/emacs/2018/05/25/display-version/)
I explained how to display your Emacs version with extra useful
details. I closed the article saying that there could be more interesting stuff
to display, and here I am with something new.

Sometimes I happen to report [missing
font-locking](https://github.com/clojure-emacs/clojure-mode/issues/474) or other
small issues with faces in a mode I use. Why not have my current colour theme
name displayed along with the bits and pieces gathered in the previous article?

As Steve Purcell [explained on StackExchange](https://superuser.com/a/320289),
the concept of *current theme* is a bit fuzzy in Emacs. You can have multiple
themes on at the same time, just have a look at <kbd>M-x
customize-themes</kbd>. Moreover, activating a theme means applying its settings
to the faces and variables it specifies.

Thus I came up with this little function:

``` emacs-lisp
(defun mu--colour-theme ()
  "Get the currently applied colour theme."
  (replace-regexp-in-string
   "[\t\n\r ]+" ""
   (let* ((command (concat "rg load-theme " user-emacs-directory))
          (res (shell-command-to-string command)))
     (nth 1 (split-string res "'")))))
```

Basically I run [ripgrep](https://github.com/BurntSushi/ripgrep) in my
`.emacs.d` to get the lines containing `load-theme`. Obviously this will return
the line in `mu–-colour-theme` as well. However, since I use `split-string` with
`'` as separator I can get the element I need from the result of `ripgrep`. `nth
1` gives me what I am looking for: `sanityinc-tomorrow-night`.

Now I can have the active colour theme displayed in my environment details
buffer:

``` emacs-lisp
;;;###autoload
(defun mu-env-info ()
  "Display Emacs version and system details in a temporary buffer."
  (interactive)
  (let ((buffer-name "*env-info*"))
    (with-help-window buffer-name
      (with-current-buffer buffer-name
        (insert (emacs-version) "\n\n")
        (insert "Repository revision: " emacs-repository-version "\n")
        (when (and system-configuration-options
                   (not (equal system-configuration-options "")))
          (insert "Configured using:\n"
                  system-configuration-options))
        (insert "\n\nEmacs uptime: " (emacs-uptime) "\n")
        (insert "Colour theme: " (mu--colour-theme) "\n")
        (insert "Operating system: " (mu--os-version) "\n")
        (insert "Window system: " (getenv "XDG_SESSION_TYPE") "\n")
        (insert "Desktop environment: " (mu--gnome-version))))))
```
