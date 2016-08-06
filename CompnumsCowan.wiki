The compnums are the subset of the inexact complex numbers whose real part and imaginary part are both flonums.
This library is based on parts of FlonumsCowan lifted to compnums.  A few procedures have been added from the C99 `<complex.h>` library.

== Specification ==

{{{
#!html
<p>
This section uses <i>cx</i>, <i>cx<sub>1</sub></i>, <i>cx<sub>2</sub></i>, etc., as
parameter names for compnum arguments.</p>
<p>
}}}


=== Constructors ===

{{{
#!html
<p></p>
<div align=left><tt>(<a name="node_idx_952"></a>compnum<i> x</i>)</tt></div>
<p>
The value returned is the compnum that is numerically closest to
<i>x</i>.  This procedure can be used to transform any complex number,
exact or inexact, to a compnum.
<p>
</p>
<p></p>
<p>
</p>
<p></p>
<div align=left><tt>(<a name="node_idx_982"></a>cxmake-rectangular<i> <i>fl<sub>1</sub></i> <i>fl<sub>2</sub></i> </i>)</tt>/div>
<p>
The value returned is the compnum whose real part is the flonum <i>fl<sub>1</sub></i> and whose imaginary part is the flonum <i>fl<sub>2</sub></i></p>
<p></p>
<p></p>
<div align=left><tt>(<a name="node_idx_982"></a>cxmake-polar<i> <i>fl<sub>1</sub></i> <i>fl
<sub>2</sub></i> </i>)</tt></div>
<p>
The value returned is the compnum that is numerically closest to the complex number whose magnitude is the flonum <i>fl<sub>1</sub></i>
and whose angle is the flonum <i>fl<sub>2</sub></i></p>
<p></p>
}}}

=== Predicates ===

{{{
#!html
<p></p>
<div align=left><tt>(<a name="node_idx_950"></a>compnum?<i> obj</i>)</tt></div>
<p>
Returns <tt>#t</tt> if <i>obj</i> is a compnum, <tt>#f</tt> otherwise.
</p>
<p></p>
<p>
</p>
<div align=left><tt>(<a name="node_idx_954"></a>cx=?<i> <i>cx<sub>1</sub></i> <i>cx<sub>2</sub></i> <i>cx<sub>3</sub></i> <tt>...</tt></i>)</tt></div>

<p>
This procedure returns <tt>#t</tt> if its arguments are numerically equal.</p>
<p>
</p>

<div align=left><tt>(<a name="node_idx_966"></a>cxzero?<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_976"></a>cxfinite?<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_978"></a>cxinfinite?<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_980"></a>cxnan?<i> cx</i>)</tt></div>
<p>
These numerical predicates test a compnum for a particular property,
returning <tt>#t</tt> or <tt>#f</tt>.
The <tt>cxzero?</tt> procedure tests whether
it is <tt>cx=?</tt> to <tt>0.0+0.0i</tt>,
<tt>cxfinite?</tt> tests whether both the real and the imaginary parts
are not an infinity and not a NaN,
<tt>cxinfinite?</tt> tests whether either the real part or the imaginary part or both is an infinity, and
<tt>cxnan?</tt> tests whether either the real part or the imaginary part or both is a NaN.</p>
<p>
</p>

}}}

=== Arithmetic operations ===

{{{
#!html

<p></p>
<div align=left><tt>(<a name="node_idx_986"></a>cx+<i> <i>cx<sub>1</sub></i> <tt>...</tt></i>)</tt></div>

<div align=left><tt>(<a name="node_idx_988"></a>cx*<i> <i>cx<sub>1</sub></i> <tt>...</tt></i>)</tt></div>
<p>
These procedures return the compnum sum or product of their compnum
arguments.  In general, they should return the compnum that best
approximates the mathematical sum or product.  (For implementations
that represent compnums using IEEE binary floating point, the
meaning of &ldquo;best&rdquo; is defined by the IEEE standards.)</p>
<p>
</p>

<div align=left><tt>(<a name="node_idx_994"></a>cx/<i> <i>cx<sub>1</sub></i> <i>cx<sub>2</sub></i> <tt>...</tt></i>)</tt></div>

<div align=left><tt>(<a name="node_idx_996"></a>cx/<i> cx</i>)</tt></div>
<p>
With two or more arguments, these procedures return the compnum
difference or quotient of their compnum arguments, associating to the
left.  With one argument, however, they return the additive or
multiplicative compnum inverse of their argument.  In general, they
should return the compnum that best approximates the mathematical
difference or quotient.  (For implementations that represent compnums
using IEEE binary floating point, the meaning of &ldquo;best&rdquo; is
reasonably well-defined by the IEEE standards.)</p>
<p>
</p>

<p>
For undefined quotients, <tt>cx/</tt> behaves as specified by the
IEEE standards.</p>
<p>
</p>

<div align=left><tt>(<a name="node_idx_998"></a>cxreal-part<i> cx</i>)</tt></div>
<p>
This procedure returns the real part of <i>cx</i> as a flonum.
</p>
<p>
</p>

<div align=left><tt>(<a name="node_idx_998"></a>cximag-part<i> cx</i>)</tt></div>
<p>
This procedure returns the imaginary part of <i>cx</i> as a flonum.
</p>
<p>
</p>

<div align=left><tt>(<a name="node_idx_998"></a>cxangle<i> cx</i>)</tt></div>
<p>
This procedure returns the flonum that is the nearest approximation to the angle of <i>cx</i>.
</p>
<p>
</p>

<div align=left><tt>(<a name="node_idx_998"></a>cxabs<i> cx</i>)</tt></div>
<div align=left><tt>(<a name="node_idx_998"></a>cxmagnitude<i> cx</i>)</tt></div>
<p>
These procedures return the flonum that is the nearest approximation to the absolute value (magnitude)
of <i>cx</i>.
</p>
<p></p>

<div align=left><tt>(<a name="node_idx_998c"></a>cxconj<i> cx</i>)</tt></div>
<p>
This procedure returns the complex conjugate
of <i>cx</i>. The result has the same real part as the real part of <i>cx</i>; its imaginary part
is the negation of the imaginary part of <i>cx</i>.
</p>
<p></p>

<div align=left><tt>(<a name="node_idx_998p"></a>cxproj<i> cx</i>)</tt></div>
<p>
This procedure returns the Riemann projection
of <i>cx</i>. This is the same as <i>cx</i> unless <i>cx</i> has an infinite real part
or imaginary part.  In this case, the result is a compnum whose real part is 0.0 and whose
imaginary part is either 0.0 or -0.0, depending on the sign of the imaginary part of <i>cx</i>.
</p>
<p></p>

<div align=left><tt>(<a name="node_idx_998g"></a>cxsignum<i> cx</i>)</tt></div>
<p>
This procedure returns a complex number whose phase is the same as <i>z</i> but whose magnitude is 1,
unless <i>z</i> is zero, in which case it returns <i>z</i>.
</p>
<p></p>

}}}

=== Transcendental functions ===

{{{
#!html

<p></p>
<div align=left><tt>(<a name="node_idx_1024"></a>cxexp<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1026"></a>cxlog<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1028"></a>cxlog<i> <i>cx<sub>1</sub></i> <i>cx<sub>2</sub></i></i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1030"></a>cxsin<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1032"></a>cxcos<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1032i"></a>cxcis<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1034"></a>cxtan<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1036"></a>cxasin<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1038"></a>cxacos<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1040"></a>cxatan<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1030h"></a>cxsinh<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1032h"></a>cxcosh<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1034h"></a>cxtanh<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1036h"></a>cxasinh<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1038h"></a>cxacosh<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1040h"></a>cxatanh<i> cx</i>)</tt></div>

<div align=left><tt>(<a name="node_idx_1040c"></a>cxcis<i> cx</i>)</tt></div>

<p>These procedures compute the usual transcendental functions.  
The <tt>cxexp</tt> procedure computes the base-<em>e</em> exponential of <i>cx</i>.
The <tt>cxlog</tt> procedure with a single argument computes the natural logarithm of
<i>cx</i> (not the base ten logarithm); <tt>(cxlog <i>cx<sub>1</sub></i>
<i>cx<sub>2</sub></i>)</tt> computes the base-<i>cx<sub>2</sub></i> logarithm of <i>cx<sub>1</sub></i>.
The <tt>cxsin</tt>, <tt>cxcos</tt>, <tt>cxtan</tt>,
<tt>cxasin</tt>, <tt>cxacos</tt>, <tt>cxatan</tt>,
The <tt>cxsin</tt>, <tt>cxcosh</tt>, <tt>cxtanh</tt>,
<tt>cxasinh</tt>, <tt>cxacosh</tt>, and <tt>cxatanh</tt> procedures compute
the sine, cosine, tangent, arcsine,
arccosine, arctangent, and their hyperbolic analogues respectively.
Finally, the <tt>cxcis</tt> function computes the compnum whose real part is the cosine of <i>cx</i>
and whose imaginary part is the sine of <i>cx</i>.</p>
<p>
See the corresponding section of R<sup>7</sup>RS-small for the underlying
mathematical operations.</p>
<p>
Implementations that use IEEE binary floating-point arithmetic 
should follow the relevant standards for these procedures.</p>
<p>
</p>


<p></p>
<div align=left><tt>(<a name="node_idx_1044"></a>cxsqrt<i> cx</i>)</tt></div>
<p>
Returns the principal square root of <i>cx</i>.</p>
<p>
</p>

<p></p>
<div align=left><tt>(<a name="node_idx_1046"></a>cxexpt<i> <i>cx<sub>1</sub></i> <i>cx<sub>2</sub></i></i>)</tt></div>
<p>
The <tt>cxexpt</tt> procedure returns <i>cx<sub>1</sub></i> raised to the power <i>cx<sub>2</sub></i>.  If <i>cx<sub>1</sub></i> is
negative and <i>cx<sub>2</sub></i> is not an integer, the result may be a
NaN, or may be some unspecified compnum.  If <i>cx<sub>1</sub></i> is zero, then
the result is zero.
</p>
}}}


 