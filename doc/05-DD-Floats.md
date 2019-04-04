---
title: DedekindReals Floats Detailed Design
author:
- Ivo List
status: Draft
changes:
- 2019-03-26: Initial draft
...

# References

-  A. Bauer and P. Taylor, The Dedekind reals in abstract Stone duality,
  Mathematical Structures in Computer Science **19** (2009) 757-838, doi: 10.1017/S0960129509007695 [DEDRAS]

[DEDRAS]: http://paultaylor.eu/ASD/dedras/


# Introduction

Package floats contains Floats 'module signature' - containing constructors and signature of 'Float' type.

We require values of Float type to be dense linear order extended with infinities (but instead of NaNs, exceptions
shall be thrown). Arithmetic operation don't need to be exact - i.e. they are rounded to given precision.

BigDecimalFloats is a concrete implementation based on Java BigDecimal type.

# Relevant ASD

The content of this chapter are excerpts copied from [DEDRAS]:


*Axiom 4.1* ASD has base type N, and products...

*Axiom 4.2* equality on discreete types...

*Axiom 4.3* N is generated from 0, variables, successor...

*Definition 4.12* If the diagonal subspace, X⊂ X×X, is open or closed then we call X discrete
or Hausdorff respectively.

*Definition 4.14* A space X that admits existential or universal quantification is called overt or
compact respectively.

*Axiom 4.15* N is overt.
 
*Definition 6.1* A **dense linear order without endpoints** is an overt Hausdorff object Q
(Definitions 4.12 and 4.14) with an open binary relation < that is, for p,q,r : Q,

  1. transitive and interpolative (dense): (p < r)⇔ (∃q. p < q < r)
  2. extrapolative (without endpoints): (∃p. p < q) ⇔ ⊤ ⇔ (∃r. q < r)
  3. linear (total or trichotomous): (p ≠ q) ⇔ (p < q) ∨ (q < p).

*Lemma 6.3* ... The usual order on N induces another (well founded) order ≺ on Q that we call **simplicity**.

*Examples 6.4* Such countable Q may consist of

  1. all fractions n/m with m ≠ 0, where "simpler" fractions have smaller denominators;
  2. dyadic or decimal fractions k/2^n and k/10^n ;
  3. finite continued fraction expansions; or
  4. roots of polynomials with integer coefficients, where the notion of simplicity is given by the degree 
     and coefficients of the polynomials;

Lemma 6.16 Let q 0 < q 1 < ··· < q n be a strictly ascending finite sequence of rationals, and let
(δ,υ) be rounded, located and bounded, with δq 0 and υq n . Then this (pseudo-)cut belongs to at
least one of the overlapping open intervals (q 0 ,q 2 ), (q 1 ,q 3 ), ..., (q n−2 ,q n ).


*Axiom 11.1* Q is a discrete, densely linearly ordered (Definition 6.1) commutative ring.

*Axiom 11.2* Q obeys the **Archimedean principle**, for p,q : Q, q > 0 ⇒ ∃n:Z. q(n − 1) < p < q(n + 1).

*Remark 11.3* ... We could weaken the properties of addition and multiplication on Q ... to the form of the four
 ternary relations a < d + e,  u + t < z, a < d × u, u × t < z,

# Design decisions

Main consumer of Floats is Intervals module.

## Extending with infinities

BigDecimal satisfies properties of dense linear order without endpoints. By adding infinite values we break this
contract. 

Infinities are useful on the next level when doing interval arithmetics. Without them Intevals module would need to
take over handling special cases: \top, \bottom interval, which are now expressed as [-inf, inf] and [inf, -inf] and
half open intervals [a,inf], [-inf, b] (and additional two cases), making altogether 6+1 cases. A basic arithmetic ops
would therefore need cases-analysis for a large number of combinations.

Another arguments is that some libraries (like MPFR) already have infinities included. In such case wrapping the library
into our implementation would be easier.

Infinities are also (ab)used for extrapolation.

## Exceptions instead of NaNs

Instead of having a NaN value, an exception shall be thrown for the library. This decision is done, because it doesn't
have any meaning to have intervals with NaN endpoints. In such cases we want this to be \top or \bottom interval.

# Floats

## Comparisons

**Coverage:** compareTo, max, min, equals.


Is provided by equals, and compareTo methods.

## Arithmetics

Arithmetics is rounded. For every arithmetic operation user needs to provide desired resulting precision. Precision
also defines simplicity order [DEDRAS, 6.3].

### Requirement: result of arithmetic operations

**The result of addition, subtraction, multiplication, and division on regular numbers shall be properly rounded.**
**Proper rounding towards floor/ceiling means that:**

 - **the result has given precision**
 - **the result is lower (respectively greater) that the precise result** and
 - **the result is greatest (respectively) least such representable number in given precision.**

*Rationale:* Given precision, because we need to control memory footprint. Lower/upper bound in needs for correct
computation. Greatest/least is not strictly necessary and we could drop it in favour of faster computation.

*Verification:* No matter the underlying implementation we can test the bounds by increasing given precision and
adding/subtracting on ulp. For BigDecimal implementation we know that addition, subtraction, multiplication with
precision big enough eventually returns proper result. Division may be tested using multiplication.  

*Source:* efficiency, correct result

*Coverage:* add, subtract, multiply, divide


### Requirement: limits of arithmetic operations

**The result of addition, subtraction, multiplication, and division when one or both of the arguments is infinite**
**shall return correct limit when it exists or throw a NaNException otherwise. Similarly division by zero.** 
 
*Rationale:* Returning correct limit simplifies interval arithmetic and still guarantee correct results.
When a limit does not exists an exception is thrown that needs to be handled by the module using Floats. We don't want
incorrect result to be hidden in a low-level module.   

*Verification:* Test in parallel operations on integers (which we may trust).
For regular values use small integers, i.e. -2...2 and for infinities big integers -100, 100. Test pair of all values.
When operation on integers produces results less that -100 or more than 100 so shall the Floats operation. 
When operation on integers produces smaller results different that 0, so shall the Floats operation.  
When operation on integers fail, so shall the operation on Floats.

*Source:* correct result

*Coverage:* edge cases of add, subtract, multiply, divide

### Requirement: negation and signum

TODO

**Result of negation shall be additive inverse. In case of infinities is shall change the sign of infinity.**

*Rationale:* TODO

*Verification:* Test that a + a.negate == 0. Test infinities separately.


## Interpolation and extrapolation


**Coverage**: split, trisect

Floats module provide ASD's extended type Q, called rationals (but not necessary fractions). The type is extended with
infinities.

Lemma 6.16





## String conversions

**Coverage:** valueOf(String, precision), toString()

We can construct Floats from ints or strings, using static method Floats.valueOf.


## Embedding of integers

**Coverage:** valueOf(Long)


## Special values

**Coverage** ZERO, posInf, negInf, isPosInf, isNegInf, isZero, isRegularNumber, signToInfity


TODO Successor can be constructed using add, but we might have problems specifying precision.

 the usual arithmetic operations on N, Z and Q
 





# BigDecimalFloats


# BigDecimal 

Floats are constructed from product of two natural numbers. BigDecimal implementation in Java is a product of BigInteger
intVal an integer called scale. Together the represent a number of form intVal * 10^-scale (example 6.4/2).

Scale parameter is checked for overflows and underflows and throws ArithmeticException. Code of BigDecimal is further
optimized for intVals that fit into long.

There is no infinities or NaNs in BigDecimal or BigInteger.

BigInteger store sign separately to magnitude, which is an array of ints. It is a pure Java implementation.

# Examples


# Possible extensions

- Implementation using MPFR.






