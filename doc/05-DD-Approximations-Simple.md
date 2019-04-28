---
title: DedekindReals Approximations Detailed Design
author:
- Ivo List
status: Draft
changes:
- 2019-04-26: Initial draft
...

# References

-  A. Bauer and P. Taylor, The Dedekind reals in abstract Stone duality,
  Mathematical Structures in Computer Science **19** (2009) 757-838, doi: 10.1017/S0960129509007695 [DEDRAS]

[DEDRAS]: http://paultaylor.eu/ASD/dedras/


# Introduction


# Relevant ASD


# Simple

Two approximations, lower and upper are given for reals and for formula.

For reals lower and upper approximations are intervals, s.t. lower(expr) <= expr <= upper(expr)

Arithmetic expressions are evaluated using interval arithmetics. For lower approximant we round downwards in the lattice
and for upper approximant we round upwards. Due to properties (already verified) of interval arithmetics, the 
lower(expr) <= expr <= upper(expr) holds.

For cuts upper and lower approximations are coming from the upstream/evaluation module.

Variables appearing in expression are coming from cuts, existential or universal quantification.

They are approximated as follows:

- cut: current upper and lower approximation from upstream module
- exists (x) : [a,b]: lower(x) = [(a+b)/2,(a+b)/2]     upper(x) = [b,a]
- forall (x) : [a,b]: lower(x) = [a,b]                 upper(x) = [(a+b)/2,(a+b)/2]

Formulas are approximated using booleans, s.t. lower(formula) <= formula <= upper(formula)

Constants, 'And' and 'or' are lifted using regular boolean operations.

Quantifiers are approximated using the formula under quantifier.

Less is approximated comparing lower and upper endpoint in the lower and upper approximation.


## Requirement: Expressions lower and upper approximation

**lower(expr) <= expr <= upper(expr)**
  
*Rationale:*

*Verification:* Verify arithmetics rounding direction (using precision where rounding is necessary).
  
## Requirement: Formulas lower and upper approximation

**lower(formula) <= formula <= upper(formula)**

*Rationale:*

*Verification:* Verify bounded universal quantifier and bounded and unbounded existential quantifier.



