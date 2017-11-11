## Ephemerons

Snapshot of [MIT Scheme 9.1 documentation](http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Ephemerons.html):

<!--
This manual documents MIT/GNU Scheme 9.1.

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
    1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
    2005, 2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute
    of Technology

     Permission is granted to copy, distribute and/or modify this
     document under the terms of the GNU Free Documentation License,
     Version 1.2 or any later version published by the Free Software
     Foundation; with no Invariant Sections, with no Front-Cover Texts
     and no Back-Cover Texts.  A copy of the license is included in the
     section entitled ``GNU Free Documentation License.''
   -->
<style type="text/css"><!--
  pre.display { font-family:inherit }
  pre.format  { font-family:inherit }
  pre.smalldisplay { font-family:inherit; font-size:smaller }
  pre.smallformat  { font-family:inherit; font-size:smaller }
  pre.smallexample { font-size:smaller }
  pre.smalllisp    { font-size:smaller }
  span.sc    { font-variant:small-caps }
  span.roman { font-family:serif; font-weight:normal; } 
  span.sansserif { font-family:sans-serif; font-weight:normal; } 
--></style>

<h3 class="section">10.8 Ephemerons</h3>

<p><a name="index-ephemeron-_0028defn_0029-1571"></a><a name="index-ephemeron_002c-broken-1572"></a><a name="index-broken-ephemeron-1573"></a>An <dfn>ephemeron</dfn> is an object with two weakly referenced components
called its <dfn>key</dfn> and <dfn>datum</dfn>.  The garbage collector drops an
ephemeron's references to both key and datum, rendering the ephemeron
<dfn>broken</dfn>, if and only if the garbage collector can prove that
there are no strong references to the key.  In other words, an
ephemeron is broken when nobody else cares about its key.  Note that
an ephemeron's reference to its datum may be dropped even if the datum
is still reachable; all that matters is whether the key is reachable. 
Once broken, ephemerons never cease to be broken; setting the key or
datum of a broken ephemeron with <code>set-ephemeron-key!</code> or
<code>set-ephemeron-datum!</code> has no effect.

   <p>Ephemerons are considerably heavier-weight than weak pairs, because
garbage-collecting ephemerons is more complicated than
garbage-collecting weak pairs.  Each ephemeron needs five words of
storage, rather than the two words needed by a weak pair.  However,
while the garbage collector spends more time on ephemerons than on
other objects, the amount of time it spends on ephemerons scales
linearly with the number of live ephemerons, which is how its running
time scales with the total number of live objects anyway.

<div class="defun">
&mdash; procedure: <b>ephemeron?</b><var> object<a name="index-ephemeron_003f-1574"></a></var><br>
<blockquote><p><a name="index-type-predicate_002c-for-ephemeron-1575"></a>Returns <code>#t</code> if <var>object</var> is a ephemeron; otherwise returns
<code>#f</code>. 
</p></blockquote></div>

<div class="defun">
&mdash; procedure: <b>make-ephemeron</b><var> key datum<a name="index-make_002dephemeron-1576"></a></var><br>
<blockquote><p><a name="index-construction_002c-of-ephemeron-1577"></a>Allocates and returns a new ephemeron, with components <var>key</var> and
<var>datum</var>. 
</p></blockquote></div>

<div class="defun">
&mdash; procedure: <b>ephemeron-broken?</b><var> ephemeron<a name="index-ephemeron_002dbroken_003f-1578"></a></var><br>
<blockquote><p>Returns <code>#t</code> if the garbage collector has dropped
<var>ephemeron</var>'s references to its key and datum; otherwise returns
<code>#f</code>. 
</p></blockquote></div>

<div class="defun">
&mdash; procedure: <b>ephemeron-key</b><var> ephemeron<a name="index-ephemeron_002dkey-1579"></a></var><br>
&mdash; procedure: <b>ephemeron-datum</b><var> ephemeron<a name="index-ephemeron_002ddatum-1580"></a></var><br>
<blockquote><p><a name="index-selection_002c-of-ephemeron-component-1581"></a><a name="index-component-selection_002c-of-ephemeron-1582"></a>These return the key or datum component, respectively, of
<var>ephemeron</var>.  If <var>ephemeron</var> has been broken, these operations
return <code>#f</code>, but they can also return <code>#f</code> if that is the
value that was stored in the key or value component. 
</p></blockquote></div>

<div class="defun">
&mdash; procedure: <b>set-ephemeron-key!</b><var> ephemeron object<a name="index-set_002dephemeron_002dkey_0021-1583"></a></var><br>
&mdash; procedure: <b>set-ephemeron-datum!</b><var> ephemeron object<a name="index-set_002dephemeron_002ddatum_0021-1584"></a></var><br>
<blockquote><p>These set the key or datum component, respectively, of <var>ephemeron</var>
to <var>object</var> and return an unspecified result.  If <var>ephemeron</var>
is broken, neither of these operations has any effect. 
</p></blockquote></div>

   <p>Like <code>weak-pair/car?</code>, <code>ephemeron-broken?</code> must be used with
care.  <code>Ephemeron-broken?</code> on an ephemeron guarantees that any
prior call to <code>ephemeron-key</code> or <code>ephemeron-datum</code> on the
same ephemeron yielded the key or datum that was stored in the
ephemeron, but it makes no guarantees about subsequent calls to
<code>ephemeron-key</code> or <code>ephemeron-datum</code>.  Thus, the correct
idiom to fetch an ephemeron's key and datum and use them if the
ephemeron is not broken is

<pre class="example">     (let ((key (ephemeron-key ephemeron))
           (datum (ephemeron-datum ephemeron)))
       (if (ephemeron-broken? ephemeron)
           ... <span class="roman">broken case</span> ...
           ... <span class="roman">code using </span><var>key</var><span class="roman"> and </span><var>datum</var> ...))
</pre>
