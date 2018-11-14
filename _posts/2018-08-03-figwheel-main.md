---
layout:     post
title:      Moving to Figwheel Main
date:       2018-08-03
summary:    How I replaced `lein-figwheel` with `figwheel-main`.
categories: programming
---

If you are a Clojure/ClojureScript programmer, you know [Bruce
Hauman](https://github.com/bhauman) and chances are he is one of your personal
heroes. If you are not already in love with him, just watch his [awesome talk on
Figwheel](https://www.youtube.com/watch?v=j-kj2qwJa_E) and join the club.

[Figwheel](https://github.com/bhauman/lein-figwheel) changed the way I work with
ClojureScript with prompt live code reloading and REPL interaction, greatly
enhancing my web-application development.

Bruce pushed Figwheel further when he unveiled [Figwheel
Main](https://github.com/bhauman/figwheel-main), a complete re-write of Figwheel
compatible both with Leiningen and the new [Clojure CLI
tools](https://clojure.org/guides/deps_and_cli). I have resisted the urge to try
it for a while, but then I remembered I have my own battleground for
experimenting and learning Clojure wonders:
[boodle](https://github.com/manuel-uberti/boodle).

I have decided to stay with Leiningen for now, but I may move to a full
`clj` and `deps.edn` setup in the future.

The first thing to change in order to accommodate Figwheel Main is my
`project.clj`. A new version of ClojureScript is necessary:

``` clojure
:dependencies
[ ;; … more dependencies …
  [org.clojure/clojurescript "1.10.312"]
  ;; … more dependencies …
]
```

Then, new values for `:source-paths` and `:resource-paths`:

``` clojure
:source-paths ["src/clj" "src/cljs"]
:resource-paths ["target" "resources"]
```

As the Figwheel Main README suggests, let’s add a couple of useful aliases:

``` clojure
:aliases
  {"fig" ["trampoline" "run" "-m" "figwheel.main"]
   "build" ["trampoline" "run" "-m" "figwheel.main" "-b" "boodle"]}
```

And the actual Figwheel Main dependencies:

``` clojure
:profiles
  {:dev
   {:dependencies [[binaryage/devtools "0.9.10"]
                   [cider/piggieback "0.3.8"]
                   [com.bhauman/figwheel-main "0.1.4"]
                   [com.bhauman/rebel-readline-cljs "0.1.4"]
                   [figwheel-sidecar "0.5.16"]]
    :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}}
```

I still want [lein-cljsbuild](https://github.com/emezeske/lein-cljsbuild) for a
production-ready build, so I can just simplify the `:build` map:

``` clojure
:cljsbuild
  {:builds
   {:min
    {:source-paths ["src/cljs"]
     :compiler {:main boodle.core
                :output-to "target/public/cljs-out/boodle-main.js"
                :asset-path "target/public/cljs-out/boodle"
                :optimizations :simple
                :pretty-print false}}}}
```

With `project.clj` ready, I move to `boodle.cljs.edn`. This is what Figwheel
Main needs to know the entry point of my application:

``` clojure
{:main boodle.core}
```

Following the instructions, I also customize some options in
`figwheel-main.edn`:

``` clojure
{:watch-dirs ["src/cljs"]
 :css-dirs ["resources/public/css"]
 :open-url false}
```

Refer to the
[documentation](https://github.com/bhauman/figwheel-main/blob/master/doc/figwheel-main-options.md)
to see what other options are available.

The last change I have to make is in my index page. Figwheel Main outputs the
compiled JavaScript file in `target/public/cljs-out/boodle-main.js`, so I only
need to modify the reference to it:

``` clojure
(hiccup/include-js "cljs-out/boodle-main.js")
```

Now it’s enough to run `lein build` and `lein run` from the project root
directory.

However, since [CIDER](https://github.com/clojure-emacs/cider) is a vital
component of my daily workflow, I set it up in `.dir-locals.el` to run
`figwheel-main` on <kbd>C-c C-x j m</kbd>. This will automatically launch a
Clojure REPL and a ClojureScript REPL when CIDER starts:

``` emacs-lisp
(cider-default-cljs-repl . figwheel-main)
(cider-figwheel-main-default-options . ":boodle")
```

That’s it. Welcome Figwheel Main and, of course, a huge “thank you!” to Bruce Hauman for the
superb work.
 
If you want to know more, `boodle` is available on GitHub:
[https://github.com/manuel-uberti/boodle](https://github.com/manuel-uberti/boodle).
