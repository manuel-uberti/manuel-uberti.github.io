---
layout:     post
title:      Reflecting on my Java 8 experience
date:       2016-10-22
summary:    Thoughts about functional programming with Java 8 at work.
categories: programming
---

After four months of being back to Java for work, I would like to share my
impressions about Java 8. Mind you, this is my first time with Java 8 on a
project, so I do not consider myself an expert on the subject. What I can
promptly say, though, is this: I have *never* had so much fun using Java before.

One of the reasons I have been partly away from Java since 2012 is that I have
been more and more dragged towards functional programming (FP from now on). It
all started with my fascination with Emacs and Emacs Lisp, then Scheme and
Clojure hit my curiosity harder. Recently, Scala has been insinuating in my
brain. You can already see a pattern here, can’t you? FP stole my heart.

I knew Java 8 stormed the object-oriented (OO from now on) community with plenty
of FP material, but it took a new, big project and the
brilliant [Java 8 In Action](https://www.manning.com/books/java-8-in-action) to
convince me I could go more functional than I was hoping for.

The team I work with comes from a purely OO mindset, so I was a bit reluctant to
introduce FP ideas and solutions. I jumped right in the middle of the
development, with a large codebase already there before the time of my
arrival. The only hint of Java 8 I found was a single use of `forEach` with a
lambda expression in it.

I then began using lambda expressions, streams and the `Optional` class in
utility classes and enumerators I could quickly write unit tests for. The
benefits were immediately evident: my code was getting clearer and concise, and
programming in a declarative fashion made it easier to read and understand. My
aim exactly.

To the rest of the team it all looked like syntactic sugar at first. One team
member advised me to spread lambdas as little as possible. Fortunately, he soon
came to appreciate my efforts and suggested not to be afraid of refactoring code
smells if I felt like it.

As much as I like working with lambdas and streams, not to mention `LocalDate`,
the Java 8 feature that really shines in our codebase is `CompletableFuture`.

We have a bunch of operations that need to be asynchronous. Basically, they
collect data from different web services to then build a composite response for
the client. The first implementation of asynchronous operations was using
`Future` and `Callable`. It was working fine, but the logic behind error
handling was a mess of `try {…} catch {…}` copied and pasted in different
places. Moreover, the `Callable` classes were just `switch` statements bound to
get bigger and bigger.

`CompletableFuture` made everything better. I wrapped each asynchronous
operation in a `CompletableFuture`, removing the unneeded `Callable`. It is not
hard now to comprehend the logic behind the algorithm, because it is definitely
simpler than before. Error handling is just a matter of dealing with three
possible exceptions.

Java 8 is neither Scala nor Clojure, but as a functional programmer wannabe it
makes me appreciate Java again.
