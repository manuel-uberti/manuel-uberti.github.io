---
layout:     post
title:      Dutch Clojure Days 2018 round-up
date:       2018-04-23
summary:    Comments on my experience at the conference.
categories: programming
---

As you may recall, I had [enthusiastic expectations and
resolutions](https://manuel-uberti.github.io/programming/2018/01/24/dcd-expectations/)
for [Dutch Clojure Days](http://clojuredays.org/). My first Clojure-only
conference, my first proper face-to-face with the community. How could I not be
excited?

<div style="text-align: center">
    <figure>
        <img src="/images/dcd.jpg">
    </figure>
</div>

On Saturday 21st at 8:30 am sharp we were at the TQ building’s reception,
greeted by [Carlo Sciolla](https://twitter.com/skuro). A couple of words on the
venue: a simple but elegant building, close to Dam Square and right in front of
a fascinating flower market. The conference happened on the fourth floor, with a
balcony to enjoy the outstanding view on the city, and food and drinks for
everybody. My first, huge “thank you, DCD!” goes to the vegetarian option which
was palatable for a vegan, but let’s keep the cheering and the hand-clapping for
the end.

Eleven speakers were waiting for us. [Vijay
Kiran](https://twitter.com/vijaykiran) set the stage and the playful mood of the
day, leaving soon room to [Alex Yakushev](https://twitter.com/unlog1c). “Embrace
the JVM” was a talk to treasure. Observability, performance profiling, memory
inspection. I am by no means a JVM expert, however the tools Alex showed us will
definitely help me get a better understanding of the machinery behind Clojure.

[Simon Belak](https://twitter.com/sbelak) was up next talking about transducers
and statistical analysis. This was probably the hardest one for me. I haven’t
found a way to appreciate the value of transducers yet, and statistical analysis
is not my strongest skill. But I still appreciated the concept of sketch
algorithms and I will hunt histograms pretty soon.

[Srihari Sriraman](https://twitter.com/sriharisriraman) with “Practical
Generative Testing Patterns” blew my mind and, if you fancy ratings and such,
was the highlight of the day. We all know
[test.check](https://github.com/clojure/test.check) is good, but the approach of
Srihari to automation, seeding relevant data and testing plausible behaviours
left me eager to grab my keyboard and implement something similar.

After lunch we were treated to one more talk before the lightning
sessions. [Wilker Lúcio](https://twitter.com/wilkerlucio) explained the beauty
and easy-of-use of GraphQL, an interesting alternative to REST for better APIs.

The lightning talks kicked off with some magical REPL-debugging from [Valentin
Waeselynck](https://twitter.com/val_waeselynck). [scope-capture](https://github.com/vvvvalvalval/scope-capture)
looked promising, and I can only hope for an integration with CIDER. Dr Roland
Kay reminded us of the usefulness of clojure.spec, although if I had to base my
opinion of clojure.spec on his talk, it roughly looked like the type-system
Clojure is missing. No trolling intended. [Thomas van der
Veen](https://twitter.com/thomasvdv007) hit the MQTT broker pedal, mixing Java
and Clojure, but I am still not sure I got the purpose of the experiment aside
from the sake of learning. [Ray McDermott](https://twitter.com/thomasvdv007)
closed the lightning sessions with an amazing browser-driven, multi-user REPL he
is devising which can make live pair-programming scattered around the world a
breeze.

The last three talks reflected experiences of using Clojure for business. [Josh
Glover](https://twitter.com/jmglov), [Philip Mates](https://twitter.com/pmatey)
and [Pierre-Yves Ritschard](https://twitter.com/pyr) shared with us the journeys
of their companies and projects and how designing, developing, and testing have
only improved since their move to our beloved language.

Drinks followed before a bit of REPL-driven comedy courtesy of Ray
McDermott. Suffice it to say we sang the Clojure version of Bowie’s “Rebel
Rebel” aptly entitled “REPL REPL”. If you weren’t there, well, you don’t know
what you missed.

Dutch Clojure Days left me with the impression that the Clojure community is
alive and hard-working, and its heart is in the right place. Ideas flourish,
projects boom, boundaries get stretched. We can only be thankful to the DCD
staff for being able to set up such a pleasant event, asking us only to join
them to share our passion.
