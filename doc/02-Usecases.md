---
title: DedekindReals Use-cases 
author:
- Ivo List
status: Draft
changes:
- 2019-03-21: Initial draft
...

# Introduction

Actors are:

 - mathematicians with background in:
   - constructive reals
   - analysis
   - interval arithmetics
   - numerical methods
 - physicists
 - computer scientists
 - other software
   - simulation applications

## Computation and expressiveness

We would like to describe the real number without providing a method to compute their
approximations.

1. A mathematician describes/enters the real number as:
   - lower cut = $x<0 \lor x² < 2$, upper cut = $0 < x \land 2 < x²$
   - integrate the area where $x²+y² < 1$
   - minimum of a function $f(x) = x(1-x)$ on the interval $[0,1]$

2. A name for the described number is provided

2. A precision of the output is entered, e.g. 10 decimal places

3. The result with required precision is returned

Actor: mathematician

### Arithmetic

We would like to compute with real number:

 - basic arithmetic: addition, subtraction, multiplication, division
 - square roots


### Computing with algebraic numbers


### Finding zero

### Finding maximum value of a function


### Logistic map

### Pendulum

### Beam Optics

### Control of precision propagation

## Correctness

We would like the solutions to:
- be always within the resulting interval
- converge

## Benchmarking



## Interfaces

### Simple Library Interface

### Calculator GUI

