---
title: DedekindReals Approximations Newton Detailed Design
author:
- Ivo List
status: Draft
changes:
- 2019-04-28: Initial draft
...

# References

-  A. Bauer and P. Taylor, The Dedekind reals in abstract Stone duality,
  Mathematical Structures in Computer Science **19** (2009) 757-838, doi: 10.1017/S0960129509007695 [DEDRAS]

[DEDRAS]: http://paultaylor.eu/ASD/dedras/


# Introduction


# Relevant ASD


# Newton

Newton provides a better approximations of formulas using 1D constraint sets.

We need evaluation of reals, which is done in interval arithmetics. Additionally we need estimation of the derivative,
which is also extended to intervals.

## Automatic Differentiation

For evaluation of reals it should hold eval(expr) <= expr and eval'(expr) <= expr'.

Forward automatic differentiation is used.

### Requirement: automatic differentiation

**\forall e: [a,b]. \forall x:[a,b]. f(x) <= f(e)+f'([a,b])**
**where <= is ment in interval sense.**

*Rationale:*

*Verification:* Test with random values for given interval. Verify the property holds.

## Constraint set

Variables appearing in expression are coming from cuts, existential or universal quantification and are approximated
the same way as with simple approximations.

Formulas \Sigma^R are approximated using constraint sets, which describe open sets with list of open intervals. Each
constraint set also has the domain where it is valid.

Lower approximation may also be called inner approximation and gives us information where formula definitely hold.

For upper approximation we may either use open set where the formula might hold or open set where it definitely doesn't 
hold. Design decision to use where it definitely doesn't hold.

Constants, 'And' and 'or' are lifted to intersection and union operations respectively for both upper and lower
approximants.

Quantifiers are approximated using the formula under quantifier.

### Requirement: intersection

### Requirement: Union

### Requirement: supermum and infinimum

### Requirement: approximate complement


## Approximation of inequalities

Less is approximated using the derivative.

0 < f(x) when x \in [a,b]

is approximated with:

0 < f(xm) + f'([a,b])(x-xm)

where xm = (a+b)/2

### Requirement: Inequalities approximation






