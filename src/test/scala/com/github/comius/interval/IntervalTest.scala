/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.interval

import org.junit.Test
import com.github.comius.floats.Floats.{ impl => D }
import org.junit.Assert.assertEquals
import com.github.comius.RoundingContext
import org.junit.Ignore

class IntervalTest {
  private[this] def e(d: D.T): Interval = {
    Interval(d, d)
  }

  val precision = 10
  val r = new RoundingContext(0, precision)

  /**
   * Tests multiplication of 0,+/- 1 with +/- inf and +/- inf with +/- inf using intervals.
   */
  @Test
  def testMultiplication(): Unit = {
    assertEquals(e(D.posInf), e(D.posInf).multiply(e(D.posInf), r))
    assertEquals(e(D.negInf), e(D.posInf).multiply(e(D.negInf), r))
    assertEquals(e(D.negInf), e(D.negInf).multiply(e(D.posInf), r))
    assertEquals(e(D.posInf), e(D.negInf).multiply(e(D.negInf), r))
    assertEquals(e(D.posInf), e(D.posInf).multiply(e(D.ONE), r))
    assertEquals(e(D.negInf), e(D.posInf).multiply(e(D.ONE.negate), r))
    assertEquals(e(D.negInf), e(D.negInf).multiply(e(D.ONE), r))
    assertEquals(e(D.posInf), e(D.negInf).multiply(e(D.ONE.negate), r))
    assertEquals(Interval(D.negInf, D.posInf), e(D.negInf).multiply(e(D.ZERO), r))
    assertEquals(Interval(D.negInf, D.posInf), e(D.posInf).multiply(e(D.ZERO), r))
    assertEquals(Interval(D.negInf, D.posInf), e(D.ZERO).multiply(e(D.negInf), r))
    assertEquals(Interval(D.negInf, D.posInf), e(D.ZERO).multiply(e(D.posInf), r))
  }

  /**
   * Not a test. It only outputs results of special cases.
   */
  @Test @Ignore
  def printMultiplicationTable(): Unit = {
    for {
      e <- List(D.negInf, D.ONE.negate, D.ZERO, D.ONE, D.posInf)
      t <- List(D.negInf, D.ONE.negate, D.ZERO, D.ONE, D.posInf)
      d <- List(D.negInf, D.ONE.negate, D.ZERO, D.ONE, D.posInf)
      u <- List(D.negInf, D.ONE.negate, D.ZERO, D.ONE, D.posInf)
    } {
      print(multiplyLakayev(d, u, e, t, r).replaceFirst("^-", "'-") + "\t")
      if (d == D.posInf && u == D.posInf) print("\n")
    }
  }

  def multiplyLakayev(d: D.T, u: D.T, e: D.T, t: D.T, r: RoundingContext): String = {
    def max(a: String, b: String): String =
      if (a == b) a else if (a == "Inf" || b == "Inf") "Inf" else "max(" + a + ", " + b + ")"

    def min(a: String, b: String): String =
      if (a == b) a else if (a == "-Inf" || b == "-Inf") "-Inf" else "min(" + a + " " + b + ")"

    def mul(a: D.T, b: D.T, inf: D.T, as: String, bs: String): String = {
      if (a.isRegularNumber() && b.isRegularNumber()) {
        val m = a.multiply(b, r.down); if (m.isRegularNumber() && !m.isZero()) as + "*" + bs else m.toString
      } else {
        inf.toString
      }
    }

    def add(a: String, b: String): String = {
      if (a == "0") b else if (b == "0") a else a + "+" + b
    }

    def lower = {
      val a1 = if (d.signum > 0 && e.signum > 0) mul(d, e, D.posInf, "d", "e") else "0"
      val a2 = if (u.signum < 0 && t.signum < 0) mul(u, t, D.posInf, "u", "t") else "0"
      val a3 = if (u.signum >= 0 && e.signum <= 0) mul(u, e, D.negInf, "u", "e") else "0"
      val a4 = if (d.signum <= 0 && t.signum >= 0) mul(d, t, D.negInf, "d", "t") else "0"
      add(max(a1, a2), min(a3, a4))
    }

    def upper = {
      val b1 = if (u.signum >= 0 && t.signum >= 0) mul(u, t, D.posInf, "u", "t") else "0"
      val b2 = if (d.signum <= 0 && e.signum <= 0) mul(d, e, D.posInf, "d", "e") else "0"
      val b3 = if (d.signum > 0 && t.signum < 0) mul(d, t, D.negInf, "d", "t") else "0"
      val b4 = if (u.signum < 0 && e.signum > 0) mul(u, e, D.negInf, "u", "e") else "0"

      add(max(b1, b2), min(b3, b4))
    }

    lower + "," + upper
  }

}
