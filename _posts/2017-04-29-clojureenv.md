---
layout:     post
title:      Daily Clojure workflow
date:       2017-04-29
summary:    My current setup for Clojure programming.
categories: programming
---

It’s already been a month since I started my [new
job](https://manuel-uberti.github.io/programming/2017/03/12/anewjob/). All is
going well and just as expected, and it’s been interesting to see how my
carefully tuned Emacs configuration dealt with everyday Clojure programming.

Truth be told, I’ve never used Emacs consistently for work. Before Clojure
I mainly did Java and Emacs support for Java is not as good as what it offers
for other programming languages. Yes, I kept Emacs around for other stuff, but
it would be a dull lie to tell I was proudly using Emacs all day in the office.

Anyway, Clojure is the new kid in town now so it’s Emacs all the way. The
obvious first choice is [CIDER](https://github.com/clojure-emacs/cider) and
I genuinely don’t have enough words to say how wonderful it is. I couple it
with [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el)
and [Smartparens](https://github.com/Fuco1/smartparens) to get the most out of
my coding experience.

I especially love how CIDER enables me to switch easily between Clojure and
ClojureScript, with two REPLs ready to go and documentation just under my
fingertips. clj-refactor enriches exploratory development with hot reloading of
dependencies and handy change of missing requires.

Then there is [Projectile](https://github.com/bbatsov/projectile). Even on small
Clojure projects we are using to test available libraries there are plenty of
files around. Mickey Petersen talks about *tempo* when it comes to using
Emacs. Projectile guarantees you don’t lose your tempo while working on projects
of different sizes.

What else? I don’t think [Magit](https://magit.vc) needs my over enthusiastic
words. [Ivy](https://github.com/abo-abo/swiper) is proving to be the right tool
at the right time, with Swiper ever so helpful. And now I am only waiting for
the day we will need proper documents to bring out the
almighty [AUCTeX](https://www.gnu.org/software/auctex/).

In the immortal words
of [Bozhidar Batsov](https://www.youtube.com/watch?v=8wLwbpCxRf0):

> Emacs is power.
>
> Emacs is magic.
>
> Emacs is fun.
>
> Emacs is forever.
