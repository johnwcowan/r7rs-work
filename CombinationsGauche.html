<p>This SRFI implements several useful procedures of
combinations, permutations and related operations.
</p>

<h3>Set operations</h3>

<p>Sets in the following procedures are in
the sense of SRFI 1, that is, lists.
Some of these procedures have two variants: a procedure without
star (e.g. <code>permutations</code>) treats all elements in the given
set distinct, while a procedure with star (e.g. <code>permutations*</code>)
considers duplication.  The procedures with star take a <var>pred</var>
argument that is used to test equality.
</p>

<dl>
<dt><a name="index-permutations"></a><code>permutations</code> <em>set</em></dt>
<dt><a name="index-permutations_002a"></a><code>permutations*</code> <em>pred set</em></dt>
<dd><p>Returns a list of all permutations of a list <var>set</var>.
</p>
<div class="example">
<pre class="example">(permutations '(a b c))
  &rArr; ((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))

(permutations '(a a b))
  &rArr; ((a a b) (a b a) (a a b) (a b a) (b a a) (b a a))

(permutations* '(a a b))
  &rArr; ((a a b) (a b a) (b a a))
</pre></div>

<p>The number of possible permutations explodes if <var>set</var> has
more than several elements.  Use with care.  If you want to process
each permutation at a time, consider <code>permutations-for-each</code> below.
</p></dd></dl>

<dl>
<dt><a name="index-permutations_002dfor_002deach"></a><code>permutations-for-each</code> <em>proc set</em></dt>
<dt><a name="index-permutations_002a_002dfor_002deach"></a><code>permutations*-for-each</code> <em>proc pred set</em></dt>
<dd><p>For each permutation of a list <var>set</var>, calls <var>proc</var>.
Returns an undefined value.
</p></dd></dl>

<dl>
<dt><a name="index-combinations"></a><code>combinations</code> <em>set n</em></dt>
<dt><a name="index-combinations_002a"></a><code>combinations*</code> <em>pred set n</em></dt>
<dd><p>Returns a list of all possible combinations of <var>n</var> elements out
of a list <var>set</var>.
</p>
<div class="example">
<pre class="example">(combinations '(a b c) 2)
  &rArr; ((a b) (a c) (b c))

(combinations '(a a b) 2)
  &rArr; ((a a) (a b) (a b))

(combinations* '(a a b) 2)
  &rArr; ((a a) (a b))
</pre></div>

<p>Watch out for the explosion of combinations when <var>set</var> is large.
</p></dd></dl>

<dl>
<dt><a name="index-combinations_002dfor_002deach"></a><code>combinations-for-each</code> <em>proc set n</em></dt>
<dt><a name="index-combinations_002a_002dfor_002deach"></a><code>combinations*-for-each</code> <em>proc pred set n</em></dt>
<dd><p>Calls <var>proc</var> for each combination of <var>n</var> elements out of <var>set</var>.
Returns an undefined value.
</p></dd></dl>

<dl>
<dt><a name="index-power_002dset"></a><code>power-set</code> <em>set</em></dt>
<dt><a name="index-power_002dset_002a"></a><code>power-set*</code> <em>pred set</em></dt>
<dd><p>Returns power set (all subsets) of a list <var>set</var>.
</p>
<div class="example">
<pre class="example">(power-set '(a b c))
  &rArr; (() (a) (b) (c) (a b) (a c) (b c) (a b c))

(power-set* '(a a b)
  &rArr; (() (a) (b) (a a) (a b) (a a b))
</pre></div>
</dd></dl>

<dl>
<dt><a name="index-power_002dset_002dfor_002deach"></a><code>power-set-for-each</code> <em>proc set</em></dt>
<dt><a name="index-power_002dset_002a_002dfor_002deach"></a><code>power-set*-for-each</code> <em>proc pred set</em></dt>
<dd><p>Calls <var>proc</var> for each subset of <var>set</var>.
</p></dd></dl>

<dl>
<dt><a name="index-power_002dset_002dbinary"></a><code>power-set-binary</code> <em>set</em></dt>
<dd><p>Returns power set of <var>set</var>, like <code>power-set</code>, but in different order.
<code>Power-set-binary</code> traverses subset space in depth-first order,
while <code>power-set</code> in breadth-first order.
</p>
<div class="example">
<pre class="example">(power-set-binary '(a b c))
  &rArr; (() (c) (b) (b c) (a) (a c) (a b) (a b c))
</pre></div>
</dd></dl>

<dl>
<dt><a name="index-cartesian_002dproduct"></a><code>cartesian-product</code> <em>list-of-sets</em></dt>
<dt><a name="index-cartesian_002dproduct_002dright"></a><code>cartesian-product-right</code> <em>list-of-sets</em></dt>
<dd><p>Returns a cartesian product of sets in <var>list-of-sets</var>.
<code>Cartesian-product</code> construct the result in left fixed order
(the rightmost element varies first), while
<code>cartesian-product-right</code> in right fixed order
(the leftmost element varies first).
</p>
<div class="example">
<pre class="example">(cartesian-product '((a b c) (0 1)))
  &rArr; ((a 0) (a 1) (b 0) (b 1) (c 0) (c 1))

(cartesian-product-right '((a b c) (0 1)))
  &rArr; ((a 0) (b 0) (c 0) (a 1) (b 1) (c 1))
</pre></div>
</dd></dl>

<dl>
<dt><a name="index-group_002dlist"></a><code>group-list</code> <em>list [[|key [ test ]]
 ]</em></dt>
<dd><p>Groups consecutive elements in a list <var>list</var> which
have the common key value.  A key value of an element is
obtained by applying the procedure <var>key</var> to the element;
the default procedure is <code>identity</code>.
For each element in <var>list</var>, <var>key</var> is applied exactly once.
The equal-ness of keys are compared by <var>test</var> procedure,
whose default is <code>eqv?</code>.
</p>
<div class="example">
<pre class="example">(group-list '(1 1 1 2 3 4 4 2 2 3 1 1 3))
  &rArr; ((1 1 1) (2) (3) (4 4) (2 2) (3) (1 1) (3))

(group-list '(1 1 1 2 3 4 4 2 2 3 1 1 3)
                :key (cut modulo &lt;&gt; 2)))
  &rArr; ((1 1 1) (2) (3) (4 4 2 2) (3 1 1 3))

(group-list '#(&quot;a&quot; &quot;a&quot; &quot;b&quot; &quot;b&quot; &quot;c&quot; &quot;d&quot; &quot;d&quot;)
                :test string=?)
  &rArr; ((&quot;a&quot; &quot;a&quot;) (&quot;b&quot; &quot;b&quot;) (&quot;c&quot;) (&quot;d&quot; &quot;d&quot;))

(group-list &quot;aabbcdd&quot;
                :test char=?)
  &rArr; ((#\a #\a) (#\b #\b) (#\c) (#\d #\d))
</pre></div>

</dd></dl>

<dl>
<dt><a name="index-permute"></a><code>permute</code> <em>vector permuter [[|fallback ]]</em></dt>
<dd><p>Returns a newly created vector, in which
the elements are permuted from <var>src</var> according to <var>permuter</var>.
</p>
<p><var>Permuter</var> is a list of exact integers.  When the <var>k</var>-th element
of <var>permuter</var> is <var>i</var>, the <var>k</var>-th element of the result
is <code>(ref <var>src</var> <var>i</var>)</code>.   Therefore, the size of the result
list is the same as the size of <var>permuter</var>.  <var>Permuter</var>
can be any kind of list, unrelated to the type of <var>src</var>.
</p>
<p>The same index <var>i</var> can appear more than once
in <var>permuter</var>.
</p>
<div class="example">
<pre class="example">(permute '#(a b c d) '(3 2 0 1))     &rArr; #(d c a b)
(permute '#(a b c d) '(0 2))         &rArr; #(a c)
(permute '#(a b c d) '(0 0 1 1 2 2)) &rArr; #(a a b b c c)
</pre></div>

<p>If an integer in <var>permuter</var> is out of the valid range for an index
of <var>vector</var>, then <var>fallback</var> is returned.
</p>
<div class="example">
<pre class="example">(permute '#(a b c) '(3 2 1 0) 'foo) &rArr; #(foo c b a)

(permute &quot;!,HWdelor&quot; #(2 5 6 6 7 1 -1 3 7 8 6 4 0) #\space)
  &rArr; &quot;Hello, World!&quot;
</pre></div>
</dd></dl>

<h3>Random operations</h3>

These procedures use <code>default-random-source</code> from
<a href="http://srfi.schemers.org/srfi-27/srfi-27.html">SRFI 27</a>.

<dl>
<dt><a name="index-permutations_002dof"></a><code>make-permutation-generator</code><i> vector</i></dt>
<dd><p>Returns a generator that yields random permutations of <var>vector</var>.
</p>
<table><tr><td>&nbsp;</td><td><pre class="example">(generator-&gt;list (make-permutation-generator '(1 2 3)) 3)
 &rArr; ((1 2 3) (2 3 1) (3 2 1))

(generator-&gt;list (make-permutation-generator &quot;abc&quot;) 3)
 &rArr; (&quot;cba&quot; &quot;cba&quot; &quot;cab&quot;)
</pre></td></tr></table>
</dd></dl>

<dl>
<dt><a name="index-combinations_002dof"></a><code>make-combination-generator</code><i> size vector</i></dt>
<dd><p>Returns a generator that yields vectors of <var>size</var> elements
randomly picked from <var>vector</var>.
</p>
<table><tr><td>&nbsp;</td><td><pre class="example">(generator-&gt;list (make-combination-generatorh 2 #(a b c)) 5)
 &rArr; (#(a c) #(a b) #(a c) #(b a) #(a c))

(generator-&gt;list (make-combination-generator 2 '#(a b c)) 5)
 &rArr; (#(a c) #(b c) #(c b) #(b a) #(b c))
</pre></td></tr></table>
</dd></dl>


<dl>
<dt><a name="index-shuffle"></a><code>shuffle</code> <em>vector [[|random-source ]]</em></dt>
<dd><p>Returns a new vector of the same size as <var>vector</var>,
in which elements are randomly permuted.
</p>
<div class="example">
<pre class="example">(shuffle '#(a b c d e))  &rArr; #(e b d c a)
</pre></div>
</dd></dl>






