<p>This SRFI provides utilities related to prime numbers.
</p>

<a name="Generating-primes"></a>
<h3 class="subheading">Generating primes</h3>

<dl>
<dt><a name="index-primes"></a><code>primes</code></dt>
<dd><p>Returns a fresh SRFI 127 lazy sequence of primes.  It is useful when
you need primes for a short period of time &mdash; if you don&rsquo;t keep
a reference to the returned sequence, it will be garbage
collected after you&rsquo;re done with the primes.
(Note that the calculation of a prime number needs the
sequence of primes from the beginning, 
so even if your code only keeps a reference
into the middle of the sequence, the entire sequence will be stored &mdash; you
have to release all
references in order to allow the sequence to be garbage collected.)
</p>
<p>On the other hand,
each sequence returned by <code>primes</code> is computed individually,
duplicating computation.  
</p>
<p>The rule of thumb is: if you use primes repeatedly throughout
the program, invoke <code>primes</code> and store its value in
a global variable, and you&rsquo;ll save computation.
If you need primes just once, invoke <code>primes</code>
at the point they are needed and abandon the result,
and you&rsquo;ll save space.
</p>
<table><tr><td>&nbsp;</td><td><pre class="example">;; show 10 prime numbers from 100-th one.
(lseq-take (lseq-drop (primes) 100) 10)
 &rArr; (547 557 563 569 571 577 587 593 599 601)
</pre></td></tr></table></dd></dl>

<a name="Testing-primality"></a>
<h3 class="subheading">Testing primality</h3>

<dl>
<dt><a name="index-small_002dprime_003f"></a><code>small-prime?</code><i> n</i></dt>
<dd><p>For positive integers 
less than <code>small-prime-bound</code>, this procedure
determines if <var>n</i> is prime or not, quickly and deterministically.
If <var>n</var> is greater than or equal to this bound, this procedure returns <code>#f</code>.
</p>
<p>This can be used to quickly filter out known primes; it never returns
<code>#t</code> on composite numbers, but it may return <code>#f</code> on
sufficiently large prime numbers).
The Miller-Rabin test below can tell if the input is definitely composite,
but it may return <code>#t</code> on some composite numbers.
</p></dd></dl>

<dl>
<dt><a name="index-_002asmall_002dprime_002dbound_002a"></a><code>small-prime-bound</code> [Variable]</dt>
<dd><p>For all positive integers below this value
<code>small-prime?</code> can determine whether it is a prime or not.
</p></dd></dl>


<dl>
<dt><a name="index-miller_002drabin_002dprime_003f"></a><code>miller-rabin-prime?</code><i> n [[|num-tests ]]</i></dt>
<dd><p>Check if an exact integer <var>n</var> is a prime number, using
the probabilistic <a href="https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test">Miller-Rabin algorithm</a>, where <var>n</var> must be greater than 1.
If this procedure returns <code>#f</code>,
<var>n</var> is a composite number.  If this procedure returns <code>#t</code>,
<var>n</var> is <em>likely</em> a prime, but there&rsquo;s a small probability
that it is a false positive.
</p>
<p>Note that if <var>n</var> is smaller than <code>small-prime-bound</code>, the algorithm is
deterministic; if it returns <code>#t</code>, <var>n</var> is certainly a prime.
</p>
<p>If <var>n</var> is greater than or equal to 
<code>small-prime-bound</code>, we use a probabilistic test.
We choose a random base integer
to perform the Miller-Rabin test up to <var>num-tests</var> (7 times by default).
The probability
of returning <code>#t</code> for a composite number
is at most <code>(expt 4 (- num-tests))</code>.
</p></dd></dl>

<dl>
<dt><a name="index-bpsw_002dprime_003f"></a><code>bpsw-prime?</code><i> n</i></dt>
<dd><p>Check if an exact integer <var>n</var> is a prime number, using
(<a href="http://www.trnicely.net/misc/bpsw.html">the Baillie-PSW primality test</a>).
It is deterministic,
and is known to return a definitive answer for all numbers less than 2<sup>64</sup>.
For larger integers this can return <code>#t</code> on a composite number,
although no such number has been found yet.  This procedure never returns <code>#f</code>
on a prime number.
</p>
<p>This test is slower than Miller-Rabin but fast enough for casual use,
so it is handy when you want a definitive answer below the above range.
</p></dd></dl>

<a name="Factorization"></a>
<h3 class="subheading">Factorization</h3>

<dl>
<dt><a name="index-naive_002dfactorize"></a><code>naive-factorize</code><i> n [[|divisor-limit ]]</i></dt>
<dd><p>Factorize a positive integer <var>n</var> by trying to divide it into
all primes up to <code>(sqrt n)</code>.  Returns a list of prime factors,
smallest first.
</p>
<table><tr><td>&nbsp;</td><td><pre class="example">(naive-factorize 142857)
  &rArr; (3 3 3 11 13 37)
</pre></td></tr></table>

<p>Although this is a pretty naive method, this works well as long as
any of <var>n</var>&rsquo;s factors are not larger than about 10<sup>7</sup>.
</p>

<table><tr><td>&nbsp;</td><td><pre class="example">(naive-factorize 3644357367494986671013))
  &rArr; (10670053 10670053 32010157)
</pre></td></tr></table>
<p>If <var>n</var> includes any larger prime factors,
the performance becomes abysmal.</p>
<p>Alternatively, providing the <var>divisor-limit</var> argument specifies
the upper bound of the prime number to be tried.  If it is given,
<code>naive-factorize</code> returns a factor <var>f</var> unchanged if it can&rsquo;t be
divided by any primes less than or equal to <var>divisor-limit</var>.
So, the last element of the returned list may be a composite number.
This is useful for excluding trivial factors before applying more sophisticated
factorizing algorithms.
</p>
<table><tr><td>&nbsp;</td><td><pre class="example">(naive-factorize 825877877739 1000)
  &rArr; (3 43 6402154091)

;; whereas
(naive-factorize 825877877739)
  &rArr; (3 43 4591 1394501)
</pre></td></tr></table>

<p>The procedure also memoizes the results on smaller values of <var>n</var> to make
things faster.
</p></dd></dl>

<dl>
<dt><a name="index-mc_002dfactorize"></a><code>mc-factorize</code><i> n</i></dt>
<dd><p>Factorize a positive integer <var>n</var> using the algorithm
described in 
R. P. Brent, <a href="http://maths-people.anu.edu.au/~brent/pub/pub051.html">
An improved Monte Carlo factorization algorithm, BIT 20 (1980), 176-184</a>.
</p>
<p>This one is capable of handling much larger factors than
<code>naive-factorize</code>, somewhere around 10<sup>20</sup> or so.
</p>
<p>Since this method is probabilistic, the execution time may vary
on the same <var>n</var>.  But it will always return the definitive
results as long as every prime factor of <var>n</var> is smaller than an
implementation-specified limit.  If <var>n</var> contains a prime factor greater than
the limit, the procedure may loop forever.
</p></dd></dl>


<a name="Miscellaneous"></a>
<h3 class="subheading">Miscellaneous</h3>

<dl>
<dt><a name="index-jacobi"></a><code>jacobi</code><i> a n</i></dt>
<dd><p>Calculates the 
<a href="http://en.wikipedia.org/wiki/Jacobi_symbol">Jacobi symbol</a> <code>(<var>a</var>/<var>n</var>)</code>.
</p></dd></dl>

<dl>
<dt><a name="index-totient"></a><code>totient</code><i> n</i></dt>
<dd><p><a href="https://en.wikipedia.org/wiki/Euler%27s_totient_function">
Euler&rsquo;s totient function</a> of the nonnegative integer <var>n</var>.
</p>
<p>The current implementation relies on <code>mc-factorize</code> above,
so it may take a very long time if <var>n</var> contains large prime factors.
</p></dd></dl>
