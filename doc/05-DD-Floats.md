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

*Axiom 4.2* equality on discrete types...

*Axiom 4.3* N is generated from 0, variables, successor...

*Definition 4.12* If the diagonal subspace, X⊂X×X, is open or closed then we call X discrete
or Hausdorff respectively.

*Definition 4.14* A space X that admits existential or universal quantification is called overt or
compact respectively.

*Axiom 4.15* N is overt.
 
*Definition 6.1* A **dense linear order without endpoints** is an overt Hausdorff object Q
(Definitions 4.12 and 4.14) with an open binary relation < that is, for p,q,r : Q,

  1. transitive and interpolative (dense): (p < r) ⇔ (∃q. p < q < r)
  2. extrapolative (without endpoints): (∃p. p < q) ⇔ ⊤ ⇔ (∃r. q < r)
  3. linear (total or trichotomous): (p ≠ q) ⇔ (p < q) ∨ (q < p).

*Lemma 6.3* ... The usual order on N induces another (well founded) order ≺ on Q that we call **simplicity**.

*Examples 6.4* Such countable Q may consist of

  1. all fractions n/m with m ≠ 0, where "simpler" fractions have smaller denominators;
  2. dyadic or decimal fractions k/2^n and k/10^n ;
  3. finite continued fraction expansions; or
  4. roots of polynomials with integer coefficients, where the notion of simplicity is given by the degree 
     and coefficients of the polynomials;

*Lemma 6.16* Let q<sub>0</sub> < q<sub>1</sub> < ··· < q<sub>n</sub> be a strictly ascending finite sequence of 
rationals, and let (δ,υ) be rounded, located and bounded, with δq<sub>0</sub> and υq<sub>n</sub>.
Then this (pseudo-)cut belongs to at least one of the overlapping open intervals (q<sub>0</sub> ,q<sub>2</sub>),
 (q<sub>1</sub>, q<sub>3</sub>), ..., (q<sub>n−2</sub>, q<sub>n</sub>).


*Axiom 11.1* Q is a discrete, densely linearly ordered (Definition 6.1) commutative ring.

*Axiom 11.2* Q obeys the **Archimedean principle**, for p,q : Q, q > 0 ⇒ ∃n:Z. q(n − 1) < p < q(n + 1).

*Remark 11.3* ... We could weaken the properties of addition and multiplication on Q ... to the form of the four
 ternary relations a < d + e,  u + t < z, a < d × u, u × t < z,

# Design decisions

Main consumer of Floats is Intervals module.

## Extending with infinities

BigDecimal satisfies properties of dense linear order without endpoints. By adding infinite values we break this
contract. 

Infinities are useful on the next level when doing interval arithmetics. Without them Intervals module would need to
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

Floats implement dense linear order like Definition 6.2, but with endpoints. 
Linearity and transitivity is covered in this section.
Interpolations and extrapolations are covered in a following section.

### Requirement: linearity

**Float values shall be linear.**

*Rationale:* Linearity is assumed by upstream software units and is needed for correctness. 

*Verification:* It is linear, because compareTo method always returns without exceptions. Verify by code inspection. 


### Requirement: transitivity

**CompareTo method shall be transitive, i.e. if a < b and b < c then a < c.**

*Rationale:* Transitivity is assumed by upstream software units and is needed for correctness. 

*Verification:* Generate random a,b,c and verify the property holds.


### Requirement: equals

**The equals method shall return true iff compareTo method returns 0.**

*Rationale:* Prevent accidental mistakes (Java BigDecimal equals compares also precision).

*Verification:* By code inspection.

**Coverage:** compareTo, equals


### Requirement: min and max

**CompareTo operation shall be consistent with min and max operation, i.e. a,b >= min(a,b) and a,b <= max(a,b).**
**Max(a,b), Min(a,b) \in {a,b}** 

*Rationale:* Correctness.

*Verification:* Verify consistency on random numbers as well as on special values.

### Requirement: signum
**CompareTo(0) operation shall be consistent with signum.**

*Rationale:* Correctness.

*Verification:* Verify on random numbers as well as on special values.


## Arithmetics

Arithmetics is rounded. For every arithmetic operation user needs to provide desired resulting precision. Precision
also defines simplicity order [DEDRAS, 6.3].

### Requirement: rounding result of arithmetic operations

**The result of addition, subtraction, multiplication, and division on regular numbers shall be properly rounded.**
**Proper rounding towards floor/ceiling means that:**

 - **the result has given precision**
 - **the result is lower (respectively greater) that the precise result** and
 - **the result is greatest (respectively) least such representable number in given precision.**

*Rationale:* Given precision, because we need to control memory footprint. Lower/upper bound in needs for correct
computation. Greatest/least is not strictly necessary and we could drop it in favor of faster computation.

*Verification:* No matter the underlying implementation we can test the bounds by increasing given precision and
adding/subtracting an ulp. For BigDecimal implementation we assume that addition, subtraction, multiplication with
precision big enough eventually returns proper result. Division may be tested using multiplication.  

*Source:* efficiency, correct result

*Coverage:* add, subtract, multiply, divide (rounding)


### Requirement: limits of arithmetic operations

**The result of addition, subtraction, multiplication, and division when one or both of the arguments is infinite**
**shall return correct limit when it exists or throw an ArithmeticException otherwise.**
**Similarly division by zero  throws exception.** 
 
*Rationale:* Returning correct limit simplifies interval arithmetic and still guarantees correct results.
When a limit does not exists an exception is thrown that needs to be handled by the module using Floats. We don't want
incorrect result to be hidden in a low-level module.   

*Verification:* Test float operation in parallel with operations on integers (which we may trust).
For regular values use small integers, i.e. -2...2 and for infinities big integers -100, 100. Test pairs of all values.
Test that float and integer operations produce same result or fail at the same time. Special cases that need to be 
handled separately are multiplication of zero and infinity and division of two infinite values.

*Source:* correct result

*Coverage:* edge cases of add, subtract, multiply, divide

### Requirement: negation

**Result of negation shall be additive inverse. In case of infinities is shall change the sign of infinity.**

*Rationale:* Correctness.

*Verification:* Verify that a + a.negate == 0. Test infinities separately.

*Coverage*: Negation

## Interpolation and extrapolation

Split function covers both interpolation and extrapolation. First is covered when operands are regular numbers. Second
is covered when one of the operands is infinity.

### Requirement: Interpolation

**Split function on regular numbers a < b, shall return x, such that a < x < b.**

**When numbers:**
  
  - **have different sign return 0.**
  - **are of the same magnitude the returned value shall be near average.**
  - **are of different magnitude, return a number that is half the bigger magnitude.**

*Rationale:* Interpolative property of Definition 6.1. We further optimize the return value on space, that is returning
the number with given property and smallest precision. When numbers are of the same magnitude, it is in interest of
upstream modules to split it evenly (without any additional knowledge). 

*Verification:* generate random numbers a < b and verify that a < split(a,b) < b. Reduce the precision of the returned
value and verify it no longer satisfies that property.

**Coverage**: split/interpolation

### Requirement: Extrapolation

**Split function shall:**

  - **when both operands are infinite return 0**
  - **on -inf, b return, x < b**
  - **on a, inf, return a < x.**
  
**Returned number x shall have double magnitude than the input number.**

*Rationale:* Extrapolative property of Definition 6.1. Furthermore we double the magnitude ...  

*Verification:*

**Coverage**: split/extrapolation

### Requirement: Trisection

**Trisect function of a,b,p shall return x,y, such that a<x<y<b and that x,y are epsilon in given precision apart.** 

*Rationale:* Number x<y are used upstream by Lemma 6.16. We optimize on the evenness of splitting the a and b.  

*Verification:* 

**Coverage**: trisect


## String conversions

**Coverage:** valueOf(String, precision), toString()

We can construct Floats from ints or strings, using static method Floats.valueOf.


## Embedding of integers

**Coverage:** valueOf(Long)


## Special values

**Coverage** ZERO, posInf, negInf, isPosInf, isNegInf, isZero, isRegularNumber, signToInfity


# BigDecimalFloats


Floats are constructed from product of two natural numbers. BigDecimal implementation in Java is a product of BigInteger
intVal an integer called scale. Together the represent a number of form intVal * 10^-scale (example 6.4/2).

Scale parameter is checked for overflows and underflows and throws ArithmeticException. Code of BigDecimal is further
optimized for intVals that fit into long.

There is no infinities or NaNs in BigDecimal or BigInteger.

BigInteger store sign separately to magnitude, which is an array of ints. It is a pure Java implementation.

# Examples


# Possible extensions

- Implementation using MPFR.
- Implementation using doubles (caveat rounding modes in Java)
