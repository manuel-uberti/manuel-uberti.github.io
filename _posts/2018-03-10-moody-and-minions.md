---
layout:     post
title:      Beauty lies in the segments of the mode line
date:       2018-03-10
summary:    Prettify Emacs mode line with `moody` and `minions`.
categories: emacs
---

Since I basically spend much of my computing in Emacs, it’s no surprise I have
come to love customizing it. One Emacs facet I keep returning to is the mode
line.

To be honest, it’s hard to remember everything I tried on this little but very
useful tool. For a long while
[smart-mode-line](https://github.com/Malabarba/smart-mode-line/) has been my
go-to package. I considered
[powerline](https://github.com/milkypostman/powerline) briefly, but even
[spaceline](https://github.com/TheBB/spaceline) didn’t last long. Eventually,
I rolled my own spin and never looked for an external solution again.

Recently, [Jonas Bernoulli](https://github.com/tarsius) — of
[Magit](https://magit.vc) fame — unveiled two new packages expressly dedicated
to the mode line: [minions](https://github.com/tarsius/minions) and
[moody](https://github.com/tarsius/moody).

Ever being annoyed by the long list of minor modes taking up precious space?  Or
are you just tired of adding `:diminish` every time you install a new package
via `use-package`? `minions` to the rescue.

`minions` replaces the minor mode list with a customizable lighter, which upon
click reveals a menu with an entry for every minor mode available. You can
enable or disable a specific mode or explore its settings in a breeze. `minions`
reduces the clutter while making it easier to check on your minor modes.

On the other hand, `moody` is for the true artist. I love sparkling Emacs with
some beauty, but as much as
[sanityinc-tomorrow-night](https://github.com/purcell/color-theme-sanityinc-tomorrow)
helps me with that, I haven’t been able to accomplish something really
satisfying with the mode line before trying `moody`.

<figure>
    <img src="/images/modeline.png">
</figure>

Now look at that. It *feels* awesome.

The number `2` on the left is my little variation on the
[eyebrowse](https://manuel-uberti.github.io/emacs/2017/08/06/eyebrowse/) segment
I found in
[spaceline](https://github.com/TheBB/spaceline/blob/master/spaceline-segments.el).
