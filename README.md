# breakout-cljs

Breakout, implemented in ClojureScript.

## Gameplay

[![YouTube video](http://i.imgur.com/Lusaztg.gif)](https://www.youtube.com/embed/SNkytQ4Yv5k)

## Goals
This was created to play with a Clojure-esque style of state management for games. Inspiration was also taken from the Entity Component System design pattern when deciding on the game's architecture. 

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 
