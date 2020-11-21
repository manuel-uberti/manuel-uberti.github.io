---
layout:     post
title:      Restricting Flymake to my projects
date:       2020-11-21
summary:    How `project.el` can be useful with Flymake.
categories: emacs
---

My recent move [from Flycheck to Flymake](https://www.manueluberti.eu/emacs/2020/09/23/flymake/) has proven to be a solid choice because
the Emacs built-in syntax checker has yet to let me down. And since by now I am
sure my love for `project.el` is absolutely obvious, could I miss the opportunity
to make these two underappreciated gems shine together?

Honestly, though, the credit here goes all to Protesilaos Stavrou. [His Flymake
setup](https://protesilaos.com/dotemacs/#h:b8bfcc05-c0d3-4093-b3fe-f06187d22c6a) made me aware of a neat way to limit the use of Flymake to the places
I actually need it.

All I had to do was adapt it to my preferences:

``` emacs-lisp
(defun mu-flymake-mode-activate-h ()
  "Activate `flymake-mode' only in my projects."
  (project--ensure-read-project-list)
  (let* ((known-projects (project-known-project-roots))
         (pr (project-root (project-current))))
    (if (and (eq buffer-read-only nil)
             (member pr known-projects))
        (flymake-mode +1)
      (flymake-mode -1))))
```

I then hooked this little function to `prog-mode-hook` and `text-mode-hook` and
everything was good to go.

Note that `project.el` must be required before running `mu-flymake-mode-activate`,
otherwise Emacs will complain about `project--ensure-read-project-list` not being
available.
