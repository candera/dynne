# dynne

> "This is my assistant, the awful DYNNE," said Dr. Dischord.
> "You must forgive his appearance, for he really doesn't have any."

From _The Phantom Tollbooth_, by Norton Juster

A library for working with audio data. Supports manipulating, playing,
visualizing, and saving sounds. Sounds can be read in .wav and .mp3
formats, and saved in .wav format.

## Installation

Available on [Clojars](https://clojars.org/org.craigandera/dynne). Put
this in your `project.clj`:

```
[org.craigandera/dynne "0.4.0"]
```

## Concepts

The basic concept in dynne is the _sound_. A sound is the combination
of a duration, a number of channels, and a **deferred** way to get the
amplitudes of all the channels. There is a protocol,
`dynne.sampled-sound/SampledSound` that gives these capabilities
through its `duration`, `channels` and `chunks` functions,
respectively.

Calling `chunks` returns a Clojure seq of chunks. A _chunk_ is a seq
of Java primitive double arrays, one per channel. All arrays in the
chunk will be the same length. Amplitudes are represented as
double-precision floating point numbers, which will be clipped to -1.0
and 1.0 (inclusive) when a sound is saved (via _save_ ).

Constructors for sounds are given for WAV and MP3 files
( _read-sound_ ), and for arbitrary functions ( _fn-sound_ ).

Processor functions accept one or more sounds and return a new sound.
Combination is by functional composition, so combining two sounds does
not result in the underlying sound being sampled - computation of the
combined operation is deferred until the combined sound is sampled.

## Usage

```clojure
(require '[dynne.sampled-sound :refer :all])
;; Or, to use the API based on core.async
;; (require '[dynne.async-sound :refer :all])

;; Create a simple one-second, 440 Hz sine wave sound
(def s (sinusoid 1.0 440))

;; See what it looks like
(visualize s)

;; See what the first 0.01 seconds look like
(visualize (trim s 0 0.01))

;; Play it. Maybe turn down your volume a bit first. :)
(play s)

;; Define a new sound that fades `s` in over 0.5 seconds
(def s2 (fade-in s 0.5))

;; Visualize that
(visualize s2)

;; Get the double array holding the raw amplitude data of the first
;; channel of the first chunk. Note that we have to pass a smaple
;; rate. File-based sounds will be converted to this rate.
(ffirst (chunks s2 16000))
;; => #<double[] [D@53b7f3b2>

;; Build up a more complicated sound
(def l (-> (sinusoid 3.0 440)
           (fade-in 1.5)))
(def r (-> (square-wave 3.0 880)
           (fade-out 1.5)))
(def s3 (-> (->stereo l r)
            (pan 0.3)))

;; And play it
(play s3)

;; And save it as a 44.1 KHz WAV
(save s3 "sample.wav" 44100)

;; Load it back in
(def s4 (read-sound "sample.wav"))

;; read-sound also works with MP3 files, but we can only save to WAV

;; Make a sound of our own design: two seconds of stereo white noise
(def s5 (fn-sound 2.0 2 (fn ^double [^long c ^double t] (- (rand 2.0) 1.0))))

;; Play it too
(play (gain s5 0.1))
```

## History

### 0.4.0

Introduces a new namespace, `dynne.async-sound`, which has sounds
generate chunks in separate threads, with handoff between threads via
[core.async](https://github.com/clojure/core.async). This should
result in performance improvements for larger audio streams on systems
with multiple cores.

The new namespace is API-compatible with the `dynne.sampled-sound`, so
the only change required should be to switch to requiring the new
`dynne.async-sound` namespace.

### 0.3.0

Major overhaul. Deprecate `dynne.sound` namespace in favor of
`dynne.sampled-sound`, which uses a more efficient, chunked
representation of sounds leveraging Java native arrays for way higher
performance.

### 0.2.0

Performance improvements

### 0.1.0

Initial release


## FAQ

### Why did you build this?

I produce the
[Relevance podcast](http://thinkrelevance.com/blog/tags/podcast). We
talk about Clojure a lot, so I thought I would see if I could automate
the podcast production using Clojure. You can see the in-progress
product of that effort
[here](https://github.com/candera/podcastifier). Along the way, a
general-purpose audio processing library started to fall out of it. So
I busted it out into its own thing, which is now dynne.

### Why didn't you just use Overtone?

Mainly because I started out just playing around with the idea and at
some point crossed into being completely obsessed with writing my own
thing. I never even looked at whether
[Overtone](http://overtone.github.io/) can do what I need, because I
knew if I did, and it could, I would stop working on the problem, and
I was having a lot of fun.

In short: if Overtone does anything like this, you should use it. It
is likely to be much, much better than dynne.

## Appreciations

Thanks to [Stuart Sierra](https://github.com/stuartsierra) for helping
me with some of the weirder problems I hit, especially around compiler
errors.

Thanks to [Zach Tellman](https://github.com/ztellman) for his
[primitive-math](https://github.com/ztellman/primitive-math) library,
which was helpful in finding sub-optimal usages of numeric operations.

Thanks to [Rich Hickey](http://twitter.com/richhickey) for his
suggestion to rewrite dynne in terms of sequences of double arrays,
which led to a massive increase in performance.

Thanks to [Prismatic](http://getprismatic.com) for their
[hiphip](https://github.com/Prismatic/hiphip) library, which was really
helpful in writing the most out of the operations at the core of
dynne.

Thanks to [tommyettinger](https://github.com/tommyettinger) for his
[fork of hiphip](https://github.com/tommyettinger/hiphip-aot) that
works around an issue with AOT.

## License

Copyright © 2013 Craig Andera

Distributed under the Eclipse Public License, the same as Clojure.
