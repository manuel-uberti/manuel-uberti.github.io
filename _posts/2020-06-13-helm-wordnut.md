---
layout:     post
title:      My reliable English dictionary
date:       2020-06-13
summary:    A Helm interface for WordNet.
categories: emacs
---

Despite my best efforts, my English needs constant work[^1]. I mostly write in this
language when I am online, but good prose is a matter of trying, failing, trying
again, failing again, and never give up. A bloody battle of words.

An ever-present companion during my writing sessions is the dictionary. Since
I don’t want distractions of any kind, I want the dictionary to be offline and
easy to use. The more time I spend away from my text, the easier it is for my
mind to drift off.

[WordNet](https://wordnet.princeton.edu) meets my expectations almost exactly. It’s quickly installable on my
Ubuntu machine, a breeze to use, and it comes with a command-line tool (`wn`) and
a graphical one (`wnb`) which are simple and fast.

However, I don’t want to leave Emacs for a task like this. I want to be able to
search for the word at point or to type something and see if what I have in mind
makes any sense at all. Why not taking advantage of the power of Helm?
That’s why I wrote [helm-wordnut](https://github.com/emacs-helm/helm-wordnut). 

As I explain in the README, using WordNet within Emacs was already possible
through [helm-wordnet](https://github.com/raghavgautam/helm-wordnet) and [wordnut](https://github.com/gromnitsky/wordnut), but in both cases it doesn’t look like there
is much maintenance going on. I liked the idea of the former and the nice output
of the latter, and so I devised a package with what in my opinion is the
simplest and ideal Helm interface for WordNet. I can type what I want or just
hit <kbd>M-n</kbd> to pick up the word a point. Pressing <kbd>RET</kbd> on the desired candidate opens
a buffer with a detailed definition of the word. How much detail one needs is
customizable through `helm-wordnut-cmd-options`, but the default covers more than
enough already for my usual writings.

Given the existence of `helm-wordnet` and `wordnut`, I have not published
`helm-wordnut` on any package archive at the moment. If somebody else finds it a
better alternative to the existing solutions, do reach out and let me know. I’ll
see what I can do about convincing the MELPA masterminds to accept yet another
WordNet front-end.

## Notes

[^1]: The same can be said for my Italian, but let’s not dig too deep into my
    shortcomings please.
