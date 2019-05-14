/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals

import org.junit.Test
import com.github.comius.floats.Floats.{ impl => D }
import org.junit.Assert.assertEquals
import com.github.comius.RoundingContext

class IntervalTest {
  private[this] def e(d: D.T): Interval = {
    Interval(d, d)
  }

  /**
   * Tests multiplication of 0,+/- 1 with +/- inf and +/- inf with +/- inf using intervals.
   */
  @Test
  def testMultiplication(): Unit = {
    val precision = 10
    val r = new RoundingContext(0, precision)

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
}
