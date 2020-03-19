---
layout:     post
title:      ! 'Lockdown Beam: bm.el'
date:       2020-03-19
summary:    Visual bookmarks in your buffers.
categories: emacs
---

I guess there is no need to explain the choice of the word “lockdown” in the
title. I don’t know about you, but here in Italy things are getting worse and
worse. Every time I look outside the window I see desolation, and for a George
Romero fan this is getting scarier and scarier. I am lucky, though. I can work
from home (thank you, [7bridges](https://7bridges.eu)) and study from home (thank you, [Ca' Foscari](https://www.unive.it/)).
But I can also take the time to spread words of love about some Emacs packages
I rarely talk about.

*Lockdown Beam* aims to be a series of small articles on packages that deserve
more attention, but I can’t tell now how long this series is going to run.
I tend to use *all* the packages configured in my `init.el`, and most of them are
fairly popular, so it’s not like digging obscure entries from the dust of my
setup. However, out of more than 100 packages, I am pretty sure I can find a
decent amount of poor neglected things in there.

Let’s start with Jo Odland’s [bm.el](https://github.com/joodland/bm).

There are many great ways to bookmark things in Emacs[^1], but sometimes a visual
clue just adds a good feeling of immediacy one (e.g., myself) can find pleasant.
The README says this is a feature from Visual Studio that the author was missing
in Emacs, but you will have to check for yourself whether this is something
Visual Studio does. You know there is no such thing as a different text editor
for me.

Following the documentation, setting up `bm.el` is easy. If you use `use-package`,
there a is nice example ready for copy-and-paste at [Configuring bm.el with
use-package](https://github.com/joodland/bm#configuring-bmel-with-use-package). I started from there, but I replaced the hook on
`vc-before-checkin-hook` with a hook on `magit-pre-refresh-hook`.

I find `bm.el` really useful when studying source code from others, or when I want
to quickly set jumping points in a log file cluttered with stacktraces.
It’s quicker then moving around with Isearch or `helm-occur`, and unlike [avy](https://github.com/abo-abo/avy) it’s
not limited to what is currently visible on the screen.

That’s all for today. Next time a little friend will help me exploit Eldoc.

Stay safe.

## Notes

[^1]: Check out what Protesilaos Stavrou has to say about [registers](https://protesilaos.com/codelog/2020-03-08-emacs-registers/).








