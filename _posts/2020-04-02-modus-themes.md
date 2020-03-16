---
layout:     post
title:      Light is right, alright?
date:       2020-03-16
summary:    My switch to `modus-operandi`.
categories: emacs
---

For a long, long while I was a happy [Solarized Light](https://github.com/bbatsov/solarized-emacs) user. Then, somehow,
I switched to Steve Purcell’s version of [Tomorrow Night](https://github.com/purcell/color-theme-sanityinc-tomorrow), and no other theme has
been able to take me away from that colour scheme. There were times where
I found myself looking for a new light theme, but the lack of a pleasant one[^1]
left me in the comfortable palettes of Tomorrow Night. I even put on MELPA what
at first looked like [the light I needed](https://github.com/manuel-uberti/doneburn-theme), but the dark side still kept me within
its reach.

Why didn’t I go back to Solarized Light if the experience had been so joyful?
I guess getting older distanced me somehow from the pleasures of a yellowish
background. Don’t get fooled by my grumpiness, though. It’s still one of the
best Emacs themes out there, so be sure to check it out if you are after a light
theme.

As for me, I’ve recently seen the light again. Protesilaos Stavrou open sourced
a couple of beautiful themes, `modus-vivendi` and `modus-operandi`, both available
at [modus-themes](https://gitlab.com/protesilaos/modus-themes), on ELPA, and on MELPA. I am sure the apt readers remember that
`modus-vivendi` first appeared on these pages right at the end of [Ripgrepping with
Helm: the mode-line](https://manuel-uberti.github.io/emacs/2020/03/01/helm-ripgrep-mode-line/), but since then `modus-operandi` has become the default.

<p align="center">
    <a href="https://raw.githubusercontent.com/manuel-uberti/manuel-uberti.github.io/master/images/modus-operandi.png" 
       target="_blank">
      <img src="/images/modus-operandi.png">
    </a>
</p>

What I found intriguing in Protesilaos’ work is the reasoning behind the themes.
I have never cared about the [Web Content Accessibility Guidelines](https://www.w3.org/WAI/standards-guidelines/wcag/), which is a
shame considering how poor my eyesight has been since I was a kid. The Modus
Themes are built with those guidelines in mind and it shows immediately. The
text is always readable, it doesn’t matter what face is applied over it.
It’s not only a matter of being able to read, but to always distinguish specific
contexts. For instance, comments are clearly separated from the rest of the code
without sacrificing legibility. I know this sounds reasonable and it may already
be the case for the theme you are currently using, but for my eyes it has never
been so easy. And yes, I did tend to prefer beauty over accessibility.

Another remarkable thing about the Modus Themes is that both variants cover a
wide range of Emacs packages, something not so axiomatic in the Emacs theme
world. Admittedly, I bothered Protesilaos with some issues about Helm and few
other packages, but he quickly tamed my wild sensibility. *“If a compromise is
ever necessary between aesthetics and accessibility, it shall always be made in
the interest of latter”*, the README points out. So far no complaints from the
aesthetics department, though.

Protesilaos explained his ideas in [My accessible Emacs themes](https://www.youtube.com/watch?v=gz8yifu6pGo), a valuable
presentation which you should watch before [My Modus Themes are in ELPA](https://www.youtube.com/watch?v=TIUXYDaq9AY).
Considering that I came from years with a dark background, the switch to a light
one has been less traumatic than it may seem. It could be due to the fact that
Solarized Light was there way before the dark age, why not? Nevertheless, the
new colour scheme is as simple and beautiful as it is effective. See you around,
Sith Lord.

## Notes ##

[^1]: An obvious matter of personal taste.
