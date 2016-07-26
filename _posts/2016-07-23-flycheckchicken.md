---
layout:     post
title:      Adding CHICKEN Scheme support to Flycheck
date:       2016-07-26
summary:    The great experience of contributing to Flycheck.
categories: emacs
---

There is no need for me to tell you how great
[Flycheck](http://www.flycheck.org/) is. If you do not trust me, at least trust
[Mickey Petersen](https://www.masteringemacs.org/article/spotlight-flycheck-a-flymake-replacement)
on this.

Since I have been playing around with [CHICKEN Scheme](http://call-cc.org/) for
quite some time now, it seemed only natural to wish for on-the-fly syntax
checking. Why limit myself to wishing, though? Why do not push it a little
further and try to help Flycheck maintainers?

Before starting any coding, I took the time to read the
[Contributor’s Guide](http://www.flycheck.org/en/latest/contributor/contributing.html). Like
all of Flycheck’s documentation, the Contributor’s Guide is well-written and
easy to understand. The guidelines about
[pull requests](http://www.flycheck.org/en/latest/contributor/contributing.html#pull-requests)
and
[commit messages](http://www.flycheck.org/en/latest/contributor/contributing.html#commit-guidelines),
in particular, put me on the right track.

I forked the Flycheck’s repository and started going through the code in
[flycheck.el](https://github.com/flycheck/flycheck/blob/master/flycheck.el). The
lack of experience brought me to shamelessly copying the Racket checker and
trying to figure out how it worked. It was not too hard to understand, but maybe
starting with the Perl checker — see
[here](http://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html) —
would have been a better choice. Nonetheless, I stole the idea of having
[Geiser](http://www.nongnu.org/geiser/) as a requirement from the Racket
checker.

Anyway, it was not time to code yet. I needed to understand if on-the-fly
syntax checking in CHICKEN Scheme would be possible at all. Flycheck gives you
the power to work with different syntax checkers within the same interface, but
I didn’t know of a similar checker for CHICKEN Scheme.

With some help on the `#chicken` IRC channel and from
[Vítor De Araújo](https://twitter.com/vbuaraujo) on Twitter, I was able to
devise my first attempt and create the proper
[pull request](https://github.com/flycheck/flycheck/pull/987).

And that is when the real magic started.

I knew Flycheck maintainers are kind and helpful because I had already had some
chances to interact with them, but I cannot remember the last time I worked on
something with *this* kind of patient assistance.

Questions promptly answered, suggestions readily available. My code kept getting
better and better. Useless lines were removed, documentation was added and the
integration test felt like the right thing to do.

I learnt a lot and could not be happier with the result. I hope I will be able
to contribute to Flycheck again. Moreover, I do hope every project I wish to
contribute to can follow the Flycheck’s community example.

Now go enjoy your on-the-fly syntax checking with CHICKEN Scheme.
