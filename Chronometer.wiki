== Chronometer ==

{{{(make-chronometer)}}}

Returns a new chronometer object.

{{{(chronometer-start! <chronometer>)}}}

{{{(chronometer-stop! <chronometer>)}}}

{{{(chronometer-reset! <chronometer>)}}}

resets <chronometer> time to zero.

{{{(chronometer-accuracy <chronometer>)}}} ;; optional

{{{(chronometer-time <chronometer>)}}}

measures the elapsed accumulated time in real number of seconds between the starting times and stop times.