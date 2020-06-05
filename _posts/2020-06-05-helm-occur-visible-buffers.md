---
layout:     post
title:      Search only in visible buffers
date:       2020-06-05
summary:    The beauty of `helm-occur-visible-buffers`.
categories: emacs
---

Among the things that I dropped when I moved to Helm, [Swiper](https://github.com/abo-abo/swiper) was one of the
first ones to go. Actually, the built-in Isearch replaced Swiper first. Then
I found out about [CTRLF](https://github.com/raxod502/ctrlf), and now I have the best of both worlds: I can use CTRLF
for no-fuss searches and I can rely on `helm-occur` for a Swiper-like feeling.

However, I often find myself with two buffers in the same window, and sometimes
I want to restrict my searches to just those two buffers. CTRLF and `helm-occur`
are not suitable for this task, because they both work on a single buffer. And
[mu-helm-rg](https://www.manueluberti.eu/emacs/2020/03/13/helm-rg-refactoring/) is way too much for this, because it is best used in specific
directories or project-wise.

Luckily enough, Thierry Volpiatto has recently introduced
`helm-occur-visible-buffers` in Helm. Like the name says, this command runs
`helm-occur` on all visible buffers. The results are grouped by buffer, making
them really easy and clear to navigate. When run in one buffer,
`helm-occur-visible-buffers` behaves just like `helm-occur`, so if you have a key
binding for the latter you can safely point it to the former and retain the
functionality you were accustomed to.

I don’t know if my use-case is a niche one, but it’s good to know Helm got me
covered once again.


