== Port Operations ==

This is a proposal for the WG2 "port operations" work item.  It provides useful iterators over generic ports, and ways to combine and alias ports.  It is mostly based on Chicken's ports unit and CL's streams facilities.

== Port iterators ==

These procedures do not check that ''source'' and ''sink'' are actually ports, so ''reader'' and ''writer'' may perform arbitrary operations as long as they can be invoked as `(`''reader''` `''source''`)` and `(`''writer''` `''value''` `''sink''`)`, respectively.  

`(port-for-each `''iterator''` `''reader''` `''source''`)`

Applies ''iterator'' to the successive results of calling ''reader'' on ''source'' until it returns an EOF object (not including the EOF object), discarding the results.  Returns an undefined value.

`(port-map `''iterator''` `''reader''` `''source''`)`

Applies ''iterator'' to the successive results of calling ''reader'' on ''source'' until it returns an EOF object (not including the EOF object), returning a list of the collected results.

`(port-fold `''iterator''` `''reader''` `''source''` `''seed'' ...`)`

Applies ''iterator'' to the successive results of calling ''reader'' on ''source'', passing the ''seed'' values as additional arguments to ''iterator''. The results returned by ''iterator'' become the new ''seed'' values. When ''reader'' returns an EOF object, the last results of ''iterator'' are returned.

`(port-unfold `''terminate?''` `''iterator''` `''writer''` `''sink''` `''seed'' ...`)`

Uses ''iterator'' to generate the sequence of values `seed`, `(iterator seed)`, `(iterator (iterator seed))`, and so on.  Uses ''writer'' to output the values to ''sink''.  ''Terminate?'' is invoked before calling ''iterator'' each time; if it returns `#f`, the unfolding terminates.  Returns an undefined value.

`(copy-port `''source''` `''sink''` `''reader''` `''writer''`)`

Reads all remaining data from ''source'' using the procedure ''reader'' and writes it to ''sink'' using the procedure ''writer''. ''Reader'' defaults to `read-char` and ''writer'' to `write-char`.  Returns an undefined value.

== Special ports ==

Broadcast ports, concatenated ports, and alias ports are mutually disjoint types, and are disjoint from other types of Scheme objects.

`(make-broadcast-port `''output-port'' ...`)`

Returns a custom output port that emits everything written into it to the ''output-ports''.  An operation can only be performed on a broadcast port if it can be performed on all the ''output-ports''.  Closing the broadcast port does not close any of the ''output-ports''.  If no ports are specified, the result serves as a sink that discards all its output.

`(broadcast-port? `''obj''`)`

Returns `#t` if ''obj'' is a broadcast port, and `#f` otherwise.

`(make-concatenated-port `''input-port'' ...`)`

Returns a custom input port that reads its input from each ''input-port'' until it returns an EOF object, then returns just one EOF object itself.  An operation can only be performed on a concatenated port if it can be performed on all the ''input-ports''.  Closing the concatenated port does not close any of the ''input-ports''.  If no ''input-ports'' are specified, the result serves as an empty source.

`(concatenated-port? `''obj''`)`

Returns `#t` if ''obj'' is a concatenated port, and `#f` otherwise.

`(make-alias-port `''parameter''`)`

Returns a port which passes any port operations invoked on it to the current value of the dynamic variable ''parameter''.

`(alias-port? `''obj''`)`

Returns `#t` if ''obj'' is an alias port, and `#f` otherwise.

