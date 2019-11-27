---
title: DedekindReals Development Plan 
author:
- Ivo List
status: Draft
changes:
- 2019-03-26: Initial draft
...

# Introduction

DedekindReals is a library implementing efficient computation with Dedekind reals based on Abstract Stone Duality.

# Development process

The development of the library involves tasks described in the following sub-chapters. The tasks are not followed
in successive (i.e. waterfall) fashion, but arbitrary verticals per unit might be done in parallel.

Before the documentation and library are released the stages are reviewed in order
respecting the inputs (also the sub-chapters follow this ordering).

## Usecases
Inputs: various  
Output: [Usecases](02-Usecases.md)

Usecases are collected from users of the library:

- mathematicians
- physicists
- software developers


## Requirements
Input: [Usecases](02-Usecases.md)  
Output: [Requirements](03-Requirements.md)

Source of the requirements are use-cases. The requirements shall be written in a way they are:

 - not open for interpretation
 - non-conflicting with other requirements
 - traceable (source and rationale)
 - verifiable (verification)

## Architecture and library interface
Input: Requirements  
Outputs:

 - [Architecture](04-01-Architecture.md)
 - [Library Interface](04-02-LibraryInterface.md) 
 
The architecture breaks down the library into smaller software units. It shall describe the interfaces among the units.
The architecture shall argue that all requirements can be satisfied. 

The DedekindReals library has an interface that is used by other software systems.
This interface is described in a separate document.

## Unit Designs
Input: [Architecture](04-01-Architecture.md)  
Output: Design per unit

The design of the unit shall include 'unit requirements', those are requirements refined through architecture.
Unit requirements are more detailed, but they shall satisfy the same rules as the requirements.

## Unit Implementation
Input: Design per unit  
Outputs:

  - Implementation of the unit
  - Implementation of the unit tests

The unit shall be implemented so that it satisfies the unit requirements given in unit design document. 

There is no additional document for unit test plan, but the test cases are documented inline with 
the implementation of unit tests. Test cases shall be traceable to the unit requirements.

## Unit Verification
Input: Implementation  
Outputs:

  - Report from unit tests
  - Code Reviews

Software units are verified by executing unit tests and by code review.

The code reviewer in the process verifies that the software unit and tests are:

- following the design document
- sufficiently documented
- following coding guidelines
- tests sufficiently verify the requirements (e.g. edge cases are covered)
  
## Integration Testing



## Software System testing
Input: [Requirements](03-Requirements.md)  
Output: Test implementation

The test plan is also automated/implemented. It shall be traceable to the Requirements.

## Release

Release notes.

# Maintenance process

# Configuration management process
