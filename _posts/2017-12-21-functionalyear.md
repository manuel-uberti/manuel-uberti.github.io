---
layout:     post
title:      A year of functional programming
date:       2017-12-21
summary:    Notes on my professional experiences in 2017.
categories: programming
---

First things first: the title is a lie.

If you happen to be one of my passionate readers, you may recall I started
[working with
Clojure](https://manuel-uberti.github.io/programming/2017/03/12/anewjob/) on
April 1. So yes, not every month of the year has been devoted to functional
programming. I just needed something bold to pull you in, sorry.

Now, how does it feel having worked with Clojure for ~~almost~~ a year?

Here at [7bridges](https://7bridges.eu/) we had our fair share of projects. The
open source ones are just a selected few:
[clj-odbp](https://github.com/7bridges-eu/clj-odbp), a driver for OrientDB
binary protocol; [carter](https://github.com/7bridges-eu/carter), an SPA to show
how our driver works; [remys](https://github.com/7bridges-eu/remys), a little
tool to interact with MySQL databases via REST APIs. I also had the chance to
play with ArangoDB recently, and there were no problems building a sample
project to understand its APIs.

At home,
[boodle](https://manuel-uberti.github.io/programming/2017/11/26/boodle/) was
born to strengthen my ever-growing knowledge and do something useful for the
family.

When I started in the new office, the switch from professional Java to
professional Clojure was a bit overwhelming. New libraries, new tools, new
patterns, new ways of solving the same old problems, new problems to approach
with a totally different mindset. It all seemed too much.

Then, something clicked.

Having the same language on both client- and server-side helped me figure out
the matters at hand with a set of ideas I could easily reuse. Once I understood
the problem, I could look for the steps to solve it. Each step required a data
structure and the function to handle this data structure. The first time I used
`reduce-kv` because it was the *most natural* choice left a great smile on my
face.

There is still much to learn, though. Due to lack of experience with JavaScript,
my ClojureScript-fu needs to improve. I have come to appreciate unit testing,
but it’s time to put this love at work on my `.cljs` files too. I also
definitely want to know more about Clojure web applications security and
performances.

2017 has been a great year to be a functional programmer. My recent [liaison
with
Haskell](https://manuel-uberti.github.io/programming/2017/12/08/learninghaskell/)
is directing me more and more on my way. The functional programming way.
