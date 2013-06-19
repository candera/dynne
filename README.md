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
[org.craigandera/dynne "0.1.0"]
```

## Concepts

The basic concept in dynne is the _sound_. A sound is the combination
of a duration, a number of channels, and a way to get the amplitude of
a particular channel at a particular point in time. The
_sample_ function is the preferred way to do this.

Amplitudes are represented as double-precision floating point numbers
between -1.0 and 1.0 (inclusive).

Constructors for sounds are given for WAV and MP3 files
( _read-sound_ ), and for arbitrary functions ( _sound_ ).

Processor functions accept one or more sounds and return a new sound.
Combination is by functional composition, so combining two sounds does
not result in the underlying sound being sampled - computation of the
combined operation is deferred until the combined sound is sampled.

## Usage

```clojure
(require '[dynne.sound :refer :all])

;; Create a simple one-second, 440 Hz sine wave sound
(def s (sinusoid 1.0 440))

;; See what it looks like
(visualize s)

;; See what the first 0.01 seconds look like
(visualize (trim s 0 0.01))

;; Play it.
(play s)

;; Define a new sound that fades `s` in over 0.5 seconds
(def s2 (fade-in s 0.5))

;; Visualize that
(visualize s2)

;; See what its value at time 0.25 is. The final argument is the
;; channel number, since sounds can be stereo or even multi-channel
(sample s2 0.25 0)
;; => 3.138066912872848E-14

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

;; Make a sound of our own design: two seconds of white noise
(def s5 (sound 2.0 (fn ^double [^double t] (rand))))
```

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
some point crossed into being completey obsessed with writing my own
thing. I never even looked at whether
[Overtone](http://overtone.github.io/) can do what I need, because I
knew if I did, and it could, I would stop working on the problem, and
I was having a lot of fun.

In short: if Overtone does anything like this, you should use it. It
is likely to be much, much better than dynne.

## License

Copyright © 2013 Craig Andera

Distributed under the Eclipse Public License, the same as Clojure.
