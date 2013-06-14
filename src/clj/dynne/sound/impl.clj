(ns dynne.sound.impl
  "Internal implementation details of the dynn.sound namespace. Use
  functions there in preference to working with these directly.")

(defprotocol Sound
  "Defines a notional sound."
  (channels [sound] "Returns the number of channels in the sound")
  (duration [sound] "Returns the duration of the sound in seconds.")
  (amplitudes [sound t c] "A function of a time in seconds `t` and the channel number `c` that
  returns a double representing the amplitidue of that channel at that
  point in time."))

