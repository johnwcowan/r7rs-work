<h2 id="Integersetprocedures">Integer set procedures</h2><p>

The elements of an integer set are exact integers.  The procedures for creating and manipulating integer sets are the same as those for <a href="http://srfi.schemers.org/srfi-113/srfi-113.html">SRFI 113 sets</a>, except that <code>set</code> is replaced by <code>integer-set</code> in their names.  However, <code>set-comparator</code> has no equivalent for integer sets, and in <code>integer-set</code>, <code>integer-set-unfold</code>, <code>integer-set-map</code>, and <code>integer-set-copy</code> the <em>comparator</em> argument is omitted.</p>

<p>Although sets subsume integer sets, providing them separately allows for increased implementation efficiency.  There are two Chicken eggs that provide integer sets: <a href="http://wiki.call-cc.org/eggref/4/iset#integer-sets"><code>iset</code></a>, on which this SRFI is based, which uses bit vectors (and underlying them, bytevectors); and <a href="http://wiki.call-cc.org/eggref/4/cis"><code>cis</code></a>, which uses lists of integer intervals.</p>


<h3 id="Additionalintegersetprocedures">Procedures without SRFI-13 analogues</h3>

<p><code>(integer-set-min </code><em>integer-set</em><code>)</code></p><p><code>(integer-set-max </code><em>integer-set</em><code>)</code></p><p>

Returns the smallest or largest integer in <i>integer-set</i>, or <code>#f</code> if there is none.

</p><p><code>(integer-set-open-interval </code><em>integer-set low high</em><code>)</code>
</p><p><code>(integer-set-closed-interval </code><em>integer-set low high</em><code>)</code>
</p><p><code>(integer-set-open-closed-interval </code><em>integer-set low high</em><code>)</code>
</p><p><code>(integer-set-closed-open-interval </code><em>integer-set low high</em><code>)</code>
<p>

Procedures that return a subset of <em>integer-set</em> contained in the interval from <em>low</em>
to <em>high</em>.  The interval may be open, closed, open below and closed above, or open above and
closed below.

</p><p><code>(integer-set-range= </code><em>integer-set k</em><code>)</code>
</p><p><code>(integer-set-range&lt; </code><em>integer-set k</em><code>)</code>
</p><p><code>(integer-set-range&lt;= </code><em>integer-set k</em><code>)</code>
</p><p><code>(integer-set-range> </code><em>integer-set k</em><code>)</code>
</p><p><code>(integer-set-range>= </code><em>integer-set k</em><code>)</code>
<p>

Procedures that return an integer set containing the elements of <em>integer-set</em> that are equal
to, less than, less than or equal to, greater than, or greater than or equal to <em>k</em>.
Note that the result <code>integer-set-range=</code> contains at most one element.

</p><p><code>(integer-set-delete-min! </code><em>integer-set</em><code>)</code></p>
<p><code>(integer-set-delete-max! </code><em>integer-set</em><code>)</code></p><p>

Linear update procedures that return two values: the smallest or largest integer in <em>integer-set</em> or <code>#f</code> if there is none, and a set which contains all the elements of <em>set</em> except the integer being returned.

</p>

## SRFI 113 procedures not in Chicken iset

Not useful: member, element-comparator, comparator, replace, replace!, partition!

Useful: disjoint?, find, count, remove, remove!, partition, set<?, set>?
