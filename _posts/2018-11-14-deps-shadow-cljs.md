---
layout:     post
title:      Moving to deps.edn and shadow-cljs
date:       2018-11-14
summary:    From `lein` and `figwheel-main` to `deps.edn` and `shadow-cljs`.
categories: programming
---

Last summer I moved [boodle](https://github.com/manuel-uberti/boodle) from
Figwheel to [Figwheel
Main](https://manuel-uberti.github.io/programming/2018/08/03/figwheel-main/).
I was pretty satisfied with this setup, but the rise of
[shadow-cljs](http://shadow-cljs.org/) in the Clojure community has been
tickling my curiosity over and over again. While configuring `shadow-cljs`
I took the chance to set `lein` aside and finally explore `clj` and `deps.edn`,
another thing on my to-do list that has been waiting for me for some time.

Let’s start with `deps.edn`:

``` clojure
{:deps
 {
  ;; … Clojure dependencies …
 }

 :paths ["src/clj" "resources"]

 :aliases {:run {:main-opts ["-m" "boodle.core"]}
           :test {:extra-paths ["test/clj"]
                  :extra-deps {lambdaisland/kaocha {:mvn/version "0.0-266"}}}
           :outdated {:extra-deps {olical/depot {:mvn/version "1.4.0"}}
                      :main-opts ["-m" "depot.outdated.main"]}}}
```

One thing worth of notice here is `:paths`. I included the `resources` directory
in it because I serve it via `compojure`/`http-kit` and that’s where I put the
static assets used in my main index page. This solution works when I run
`mount.core/start` via REPL and when I use the `:run` alias from the command
line.

Speaking of aliases, they are invaluable. With `:run` I can use `clj -A:run`
instead of `lein run`, and with `clj -A:outdated -a outdated` I can easily check
for outdated dependencies in my `deps.edn` thanks to
[depot](https://github.com/Olical/depot). Unit tests are now handled by
[kaocha](https://github.com/lambdaisland/kaocha), and as per its instructions
I have a `bin/kaocha` file which picks up the `:test` alias and runs all my
tests.

I love the simplicity of `deps.edn`, it feels like a de-cluttered `project.clj`.

Configuring `shadow-cljs` was a bit more complicated. It took me a while to
understand how to properly integrate it with
[CIDER](https://github.com/clojure-emacs/cider). However, the manual is great
and the fine people on Slack’s `#shadow-cljs` (thanks Ryan Haywood and Thomas
Heller!) made it all nice and smooth.

This is my `shadow-cljs.edn`:

``` clojure
{:source-paths ["src/cljs"]
 :nrepl {:port 8777
         :middleware [refactor-nrepl.middleware/wrap-refactor]}
 :dependencies [
                 ;; … ClojureScript dependencies …
               ]
 :builds {:boodle {:target :browser
                   :output-dir "resources/public/js"
                   :asset-path "/js"
                   :modules {:main {:entries [boodle.core]}}}}}
```

Basic setup, straight from the manual pages. Notice that `:output-dir` refers to
the same `resources` directory configured in `deps.edn`. This tells
`shadow-cljs` to place the build output exactly where I need.

The other important change is in the CLJS libraries I use. `shadow-cljs` does
not support [CLJSJS](https://cljsjs.github.io/), so I had to install `pikaday`
and `moment` via `yarn`.

``` shell
$ yarn add pikaday moment
```

Since `boodle` relies on `re-frame`, I had to install three other libraries to
make `shadow-cljs` build my code happily:

``` shell
$ yarn add react react-dom create-react-class
```

In the namespace where I use them, the `:require` had to be changed:

``` clojure
(:require ;; … other requires …
          ["react-dom" :refer [findDOMNode]]
          ["pikaday" :as pikaday])
```

And the code had to be adapted:

``` clojure
(let [default-opts {:field (findDOMNode this)
                    ;; … other options …
                   }
      instance (pikaday. opts)]
    ;; … other ClojureScript code …
)
```

However, a project setup is not ready unless I can work on it via Emacs. These
are the steps I made to have a working CLJ/CLJS development environment. When
[this issue](https://github.com/clojure-emacs/cider/issues/2447) will be fixed
some of it may become unnecessary.

First, I added a couple of dependencies in `shadow-cljs.edn`:

``` clojure
:dependencies [;; … other dependencies …
               [cider/cider-nrepl "0.18.0"]
               [refactor-nrepl "2.4.0"]]
```

Then I removed everything Figwheel Main related from the `.dir-locals.el` file
located in the root directory of `boodle`:

``` emacs-lisp
((nil
  (cider-default-cljs-repl . shadow-select)
  (cider-pprint-fn . zprint)
  (cider-preferred-build-tool . clojure-cli)
  (cider-known-endpoints . (("hathaway" "localhost" "8777")))
  (cider-ns-refresh-after-fn . "mount.core/start")
  (cider-ns-refresh-before-fn . "mount.core/stop"))
 (emacs-lisp-mode
  (flycheck-disabled-checkers . "emacs-lisp-checkdoc")))
```

I ran `cider-jack-in-clj` to have a CLJ REPL and check I didn’t break something
on the Clojure side of `boodle`.

From a terminal, I used `shadow-cljs watch boodle` to have `shadow-cljs` build
`boodle` and offer me a nice nREPL server listening on port `8777`, the one
I specified in `shadow-cljs.edn`.

Back in CIDER, `cider-connect-cljs` brought up a REPL connected to `shadow-cljs`
nREPL server, and let me select the build I want it operating on (default is
`dev`, I just typed in `boodle`). Connecting to `boodle` via the browser
finalized the connection between my code and the CLJS environment.

A great journey. Once again, `boodle` proved fertile ground to understand the
Clojure ecosystem and play with it to learn something new. If you want more
details, the project is on [my GitHub](https://github.com/manuel-uberti/boodle).
