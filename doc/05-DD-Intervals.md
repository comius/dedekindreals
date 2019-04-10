---
title: DedekindReals Intervals Detailed Design
author:
- Ivo List
status: Draft
changes:
- 2019-04-03: Initial draft
...

# References

-  A. Bauer and P. Taylor, The Dedekind reals in abstract Stone duality,
  Mathematical Structures in Computer Science **19** (2009) 757-838, doi: 10.1017/S0960129509007695 [DEDRAS]

[DEDRAS]: http://paultaylor.eu/ASD/dedras/


# Introduction

# Relevant ASD

The content of this chapter are excerpts copied from [DEDRAS]:

*Definition 2.3* It is very useful to generalise the notion of Dedekind cut by dropping the locatedness condition.
Instead of almost touching, and so representing a single real number a ∈ R, such
a pseudo-cut corresponds classically to a closed interval [d,u] ≡ R \ (D ∪ U). We sometimes
weaken the other conditions too, allowing D ≡ ∅ and d ≡ −∞, or U ≡ ∅ and u ≡ +∞.

*Definition 2.10* We can consider these **generalised intervals** as members of the interval domain, IR [Sco72b, ES98].
The order relation is traditionally defined as reverse inclusion of closed intervals,
and so directed joins are given by intersection.

*Definition 7.6* The **unbounded interval domain** is the closed subspace IR<sup>∞</sup> ⊂ R × R that is
co-classified by ∇, so it consists of those (δ,υ) for which ∇(δ,υ) is false (Remark 4.11), i.e. which
are rounded and disjoint. Using Lemma 5.9, IR<sup>∞</sup> ⊂ Σ<sup>Q</sup> × Σ<sup>Q</sup> is defined by the nucleus
E<sub>rd</sub> Φ(δ,υ) ≡ ∇(δ&#770;, υ&#780;) ∨ Φ(δ&#770;, υ&#780;).

*Definition 7.8* The **bounded interval predomain** is the open subspace IR ⊂ IR<sup>∞</sup> classified by
B, so it consists of those (δ,υ) for which B(δ,υ) is true. By Lemma 5.9, IR ⊂ Σ<sup>Q</sup> ×Σ<sup>Q</sup> is defined
by the nucleus

E<sub>rdb</sub> Φ(δ,υ) ≡ ∇(δ&#770;, υ&#780;) ∨ Φ(δ&#770;, υ&#780;) ∧ B(δ&#770;, υ&#780;),
which may be bracketed either way, since ∇ ⩽ B.

*Theorem 7.14* E<sub>1</sub> is a nucleus (Definition 5.5), and (δ,υ) is admissible for it (Definition 5.7) iff
it is rounded, bounded and disjoint, so IR ≅    {Σ<sup>Σ<sup>Q</sup> ×Σ<sup>Q</sup></sup>| E<sub>1</sub> }.


*Proposition 9.9* If (Q,<) has an order-reversing automorphism (−) then so does R, and this
has a unique fixed point (0):

⊝ (δ,υ) ≡ ( λd. υ(−d), λu. δ(−u) )

0 ≡ (λd. d < −d, λu. − u < u).


*Definition 10.1* For d ≤ u : R, the open and closed intervals (d,u),[d,u] ⊂ R are

  1. the open subspace classified by (λx. d < x < u) ≡ (υ<sub>d</sub> ∧ δ<sub>u</sub> ) : Σ<sup>R</sup> , and
  2. the closed subspace co-classified by (λx. x < d ∨ u < x) ≡ (δ<sub>d</sub> ∨ υ<sub>u</sub> ) : Σ<sup>R</sup> respectively.

  More generally, for any rounded pair (δ,υ),

  3. (υ,δ) ⊂ R is the open subspace classified by υ ∧ δ : Σ<sup>R</sup>, and
  4. [δ,υ] ⊂ R is the closed subspace co-classified by δ ∨ υ : Σ<sup>R</sup>,
where we expect the pseudo-cut (δ,υ) to be located (maybe overlapping or back-to-front) in the
first case and disjoint in the second.

*Remark 11.6* Now we have a diagram that combines those in Remarks 2.13 and 6.9,

*Remark 11.8* The plan, for each arithmetic operation * (+ or ×), is

  1. first to define some operation ⍟ on Σ<sup>Q</sup> × Σ<sup>Q</sup> that extends the given operation ∗ on the
   rationals, in the sense that the rectangle above commutes, i.e. for each q,r : Q,
   
   (δ<sub>q</sub> ,υ<sub>q</sub>) ⍟ (δ<sub>r</sub> ,υ<sub>r</sub> ) = (δ<sub>q⍟r</sub> ,υ<sub>q⍟r</sub> ) : Σ<sup>Q</sup> × Σ<sup>Q</sup> ;

*Notation 11.10* For (δ,υ),(ε,τ) : Σ<sup>Q</sup> × Σ<sup>Q</sup>, let
(δ,υ) ⊕ (ε,τ) ≡ ( λa. ∃de. (a < d + e) ∧ δd ∧ εe, λz. ∃ut. (u + t < z) ∧ υu ∧ τt )

*Remark 11.11*
x ⋐ y ≡ [x&#818;,x&#773;] ⊂ (y&#818;,y&#773;) ≡ y&#818; < x&#818; ∧  x&#773;  < y&#773;

x ⍟ y ⋐  w ⇔  ∃x' y' . x ⋐ x' ∧ y ⋐ y' ∧ x' ⍟  y' ⋐  w

*Lemma 11.13* Addition (Notation 11.10) takes intervals (i.e. rounded, bounded and disjoint
pseudo-cuts) to intervals, ⊕ : IR × IR → IR.

*Lemma 12.1* Kaucher multiplication [a,z] ≡ [d,u] ⊗ [e,t] is defined by

<pre>
                  d ≤ 0             0 ≤ d  
[d,u]⊗[e,t]  u ≤ 0    u ≥ 0    u ≤ 0    u ≥ 0  
        
0 ≤ e 0 ≤ t [dt, ue] [dt, ut] [de, ue] [de, ut] 
      t ≤ 0 [ut, ue]  [0,0]    [q,p]   [de, dt]  
        
e ≤ 0 0 ≤ t [dt, de]  [p,q]    [0,0]   [ue, ut] 
      t ≤ 0 [ut, de] [ue, de] [ut, dt] [ue, dt]
</pre>

where p ≡ min(dt,ue) ≤ 0 and q ≡ max(de,ut) ≥ 0. It agrees with ordinary multiplication in Q
when d = u and e = t, and with Moore’s operation (Definition 2.3) when d ≤ u and e ≤ t. Also,
a is a monotone function of d and e and an antitone one of u and t, and vice versa for z. 

*Theorem 13.4* R is an ordered field, in which x<sup>−1</sup> is defined for x ≠ 0 by

>  (δ,υ)<sup>−1</sup> ≡
>    ( λd. ∃u. υu ∧ ((du < 1 ∧ δ0) ∨ (1 < du ∧ d < 0)),
>      λu. ∃d. δd ∧ ((du < 1 ∧ u > 0) ∨ (1 < du ∧ υ0)) ) 


*Remark 13.5* We do not need to consider 0 this time, but since

   ∃du. (δ,υ)<sup>−1</sup> (d,u) ⇒ δ0 ∨ υ0,

the value in any illegitimate case, including 0<sup>−1</sup> and (δ<sub>−1</sub> ,υ <sub>1</sub> ) <sup>−1</sup> , is (⊥,⊥).
This denotes the interval [−∞,+∞].

More generally, the reciprocal of a general (and possibly back-to-front) interval with endpoints
is given by

<pre>
[d,u]<sup>−1</sup>    d < 0    d = 0    d > 0
u < 0   [u<sup>−1</sup>, d<sup>−1</sup>] [u<sup>−1</sup>, −∞]  [+∞,−∞]
u = 0    [−∞,+∞]  [−∞,+∞]  [+∞, d<sup>−1</sup>]
u > 0    [−∞,+∞]  [−∞,+∞]  [u<sup>−1</sup>, d<sup>−1</sup>],
</pre>

# Intervals

Interval case class represents a general interval with rational endpoints. There are no restrictions on the endpoints
 (d<u, d=u or d<u). We're keeping only roundedness (not bound, disjoint nor located)

Endpoints can also be positive and negative infinity, we must be take to handle such cases correctly.
 
 
## Requirement: interval operations

**Interval arithmetic operations are proper extensions of arithmetic operations on rational.**
**Proper extension: x ⍟ y ⋐  w ⇔  ∃x' y' . x ⋐ x' ∧ y ⋐ y' ∧ x' ⍟  y' ⋐  w**
  
*Rationale:* Correctness.  

*Verification:* Test the property on random values (including all combinations of special values) 
for all operations.


## Requirement: string representation of intervals

**Intervals shall be printed with minimal unnecessary information:**

  - **when endpoints are equal, print interval as a number**
    - example [1,1] = "1"
  - **when endpoints are of different magnitude or sign, print both numbers, limiting precision to p**, 
     examples p = 2:
       - "[1.1e10, -1]",
       - "[-1,2]"
  - **when endpoints are of same magnitude and sign**
    **print numbers until the difference of printed numbers has given precision**
    **and print same part of the number only once**, examples p = 2: 
      - [123001,124005]="12[3000,4000]",
      - [0.00123,0.00134] = "0.001[23,34]",
      - [0.4999266, 0.5000125] = "0.[499926,500012]"  
     

*Rationale:* Usability. Upstream modules compute real numbers, so the focus is on intervals with small width.

*Verification:* Test listed cases.

# Possible extensions

  -  
