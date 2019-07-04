/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals

import org.junit.Assert
import org.junit.Test

import com.github.comius.RoundingContext
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Real
import com.github.comius.reals.newton.AutomaticDifferentiation
import com.github.comius.reals.syntax.Var

/**
 * Unit tests for Approximations - simple.
 *
 * Environment: no specific test environment needs to be set up. Java/Scala provide everything in default installation.
 */
class ApproximationsTest {
  // Imports implicit conversions to Dyadic, to formulas.
  import com.github.comius.reals.syntax.Real._

  /**
   * Verifies if approximation of real expression rounded to 2 decimal places gives expected lower and upper
   * approximants.
   *
   * @param e the expression
   * @param expected expected result
   */
  private def test(e: Real, expected: Interval): Unit = {
    val a = AutomaticDifferentiation.approximate(e)(Context(new RoundingContext(0, 2)))
    // println(s"$a ,$expected")
    Assert.assertTrue(
      s"Lower ${a.lower} is below expected value $expected for real $e",
      a.lower.d.compareTo(expected.d) <= 0 && a.lower.u.compareTo(expected.u) >= 0)
    Assert.assertTrue(
      s"Upper ${a.upper} is above expected value $expected for real $e",
      expected.d.compareTo(a.upper.d) <= 0 && expected.u.compareTo(a.upper.u) >= 0)
  }

  /**
   * Tests 'Expressions lower and upper approximation'
   */
  @Test
  def testArithmetics(): Unit = {
    test(Const(420) + Const(5), Interval(425, 425))
    test(Const(420) - Const(5), Interval(415, 415))
    test(Const(423) * Const(2), Interval(846, 846))
    test(Const(423) / Const(3), Interval(141, 141))
  }

  /**
   * Verifies if approximation of given formula gives expected result.
   *
   * @param f the formula
   * @param result expected result
   */
  private def test(f: Formula, result: Boolean): Unit = {
    val a = AproximateSimple.approximate(f)(Context(new RoundingContext(0, 2)))
    Assert.assertEquals(s"Formula $f", Approximation(result, result), a)
  }

  /**
   * Verifies if approximation of given formula needs further refinement.
   *
   * @param f the formula
   */
  private def testna(f: Formula): Unit = {
    val a = AproximateSimple.approximate(f)(Context(new RoundingContext(0, 2)))
    Assert.assertEquals(s"Formula $f", Approximation(false, true), a)
  }

  /**
   * Test existential quantifier lower and upper approximation.
   */
  @Test
  def testExists(): Unit = {
    test(Exists("x", 2, 4, Const(0) < "x"), true)
    test(Exists("x", 2, 4, Const(1) < "x"), true)
    test(Exists("x", 2, 4, Const(2) < "x"), true)
    testna(Exists("x", 2, 4, Const(3) < "x"))
    test(Exists("x", 2, 4, Const(4) < "x"), false)
    test(Exists("x", 2, 4, Const(5) < "x"), false)

    // Verify linear around 0
    test(Exists("x", D.negInf, D.ZERO, Const(0) < "x"), false)
    test(Exists("x", D.negInf, D.ZERO, Var("x") < 0), true)
    test(Exists("x", D.ZERO, D.posInf, Const(0) < "x"), true)
    test(Exists("x", D.ZERO, D.posInf, Var("x") < 0), false)

    // Verify upper half,  linear positive and negative
    test(Exists("x", 42, D.posInf, Var("x") < 42), false)
    test(Exists("x", 42, D.posInf, Const(42) < "x"), true)
    test(Exists("x", -42, D.posInf, Var("x") < -42), false)
    test(Exists("x", -42, D.posInf, Const(-42) < "x"), true)

    // Verify lower half, linear positive and negative
    test(Exists("x", D.negInf, 42, Var("x") < 42), true)
    test(Exists("x", D.negInf, 42, Const(42) < "x"), false)
    test(Exists("x", D.negInf, -42, Var("x") < -42), true)
    test(Exists("x", D.negInf, -42, Const(-42) < "x"), false)

    // Verify rounded values
    testna(Exists("x", D.negInf, 422, Const(420) + Const(5) < "x"))
    test(Exists("x", D.negInf, 420, Const(420) + Const(5) < "x"), false)

    testna(Exists("x", 1, 3, Const(2) < "x"))
    testna(Exists("x", 0, 1, "0.5" < "x" * (Const(1) - "x")))
  }

  /**
   * Test universal quantifier lower and upper approximation.
   */
  @Test
  def testForall() = {
    test(Forall("x", 2, 4, Const(0) < "x"), true)
    test(Forall("x", 2, 4, Const(1) < "x"), true)
    testna(Forall("x", 2, 4, Const(2) < "x"))
    test(Forall("x", 2, 4, Const(3) < "x"), false)
    test(Forall("x", 2, 4, Const(4) < "x"), false)
    test(Forall("x", 2, 4, Const(5) < "x"), false)

    // Verify linear around 0
    test(Forall("x", -1, 0, Const(0) < "x"), false)
    testna(Forall("x", -1, 0, Var("x") < 0))
    testna(Forall("x", 0, 1, Const(0) < "x"))
    test(Forall("x", 0, 1, Var("x") < 0), false)

    // Verify upper half,  linear positive and negative
    test(Forall("x", 40, 41, Var("x") < 42), true)
    test(Forall("x", 40, 41, Const(42) < "x"), false)
    testna(Forall("x", -43, -42, Var("x") < -42))
    test(Forall("x", -41, -40, Const(-42) < "x"), true)

    // Verify lower half, linear positive and negative
    testna(Forall("x", 41, 42, Var("x") < 42))
    test(Forall("x", 41, 42, Const(42) < "x"), false)
    test(Forall("x", -44, -43, Var("x") < -42), true)
    test(Forall("x", -44, -42, Const(-42) < "x"), false)

    // Verify rounded values
    testna(Forall("x", 420, 422, Const(420) + Const(5) < "x"))
    testna(Forall("x", 429, 431, Const(420) + Const(5) < "x"))
    test(Forall("x", 431, 432, Const(420) + Const(5) < "x"), true)
  }
}
