---
layout:     post
title:      Motivate yourself
date:       2019-07-06
summary:    Display a message at Emacs startup.
categories: emacs
---

Life is such an ephemeral thing. When you really think about it, the time spent
on the planet we are destroying day after day is not that much, especially
compared to the entire history of humankind. Luckily enough, I can allow myself
to indulge in thoughts about life and time. I tend to avoid laziness as much as
possible, [saving spare
time](http://manuel-uberti.github.io/real-life/2019/04/10/digital-minimalism/)
for gratifying leisure: morning runs and basic workout, books,
[cinema](https://www.filmsinwords.eu/), collecting vinyls, occasional cooking.
I like to think of myself as someone who is not set on a predictable path for
the rest of his life.

Lassi Kortela’s [memento-mori](https://github.com/lassik/emacs-memento-mori) has
recently appeared on [MELPA](http://melpa.org/#/memento-mori). It’s a small
package which aims to display your age in the mode-line. The age is displayed
with two decimals and it gets updated after a few days once you turned on
`memento-mori-mode`. Thus you have a reminder of how long you have been on
Earth, a somehow inspirational indicator to question the time at your disposal.
We are entering philosophical worlds here.

However, I don’t like having too much stuff in my mode-line. I want it simple
and with only the information I need. That’s why I use
[minions](http://melpa.org/#/minions) to hide all the minor-modes. But I can
still use `memento-mori` to display a message on Emacs startup.

``` emacs-lisp
(defun mu-display-memento-mori ()
  "Display my current age by leveraging `memento-mori-age-string'."
  (interactive)
  (let ((first-name (car (s-split-words user-full-name)))
        (age (s-trim memento-mori-age-string))
        (msg "%s, you are %s, don't waste your time!"))
    (run-with-timer 2 nil
                    (lambda ()
                      (message msg first-name age)
                      (run-with-timer 3 nil
                                      (lambda ()
                                        (message nil)))))))
```

I am using [s](http://melpa.org/#/s) facilities to manipulate strings, but more
importantly the trick is using
[run-with-timer](https://doc.endlessparentheses.com/Fun/run-with-timer.html) as
explained on [this answer](https://emacs.stackexchange.com/a/28243/5514) on
Emacs.StackExchange. After two seconds, the message is shown, and three seconds
after being visible it will disappear.

Provided you set `memento-mori-birth-date` and activate `memento-mori-mode`,
adding the function to
[after-init-hook](https://doc.endlessparentheses.com/Var/after-init-hook.html)
does the rest.

Note that you need to activate [Lexical
Binding](https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Lexical-Binding.html#Using-Lexical-Binding)
in your init file to use `mu-display-memento-mori`[^1].

## Notes ##

[^1]: Thanks to Martin Buchmann for pointing this out.
