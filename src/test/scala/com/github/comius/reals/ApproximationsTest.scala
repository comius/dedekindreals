package com.github.comius.reals

import org.junit.Test
import com.github.comius.reals.syntax.Exists
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.syntax.Const
import com.github.comius.RoundingContext
import org.junit.Assert
import com.github.comius.reals.BisectionApproximations.Approximation
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Forall

class ApproximationsTest {
  import syntax.Real._
  
  def test(f: Formula, result: Boolean) = {
    val a = BisectionApproximations.approximate(f)(Context(new RoundingContext(0, 2)))
    Assert.assertEquals(s"Formula $f", Approximation(result, result), a)
  }

  def testna(f: Formula) = {
    val a = BisectionApproximations.approximate(f)(Context(new RoundingContext(0, 2)))
    Assert.assertEquals(s"Formula $f", Approximation(false, true), a)
  }

  @Test
  def testExists() = {
    test(Exists('x, 2, 4, Const(0) < 'x), true)
    test(Exists('x, 2, 4, Const(1) < 'x), true)
    test(Exists('x, 2, 4, Const(2) < 'x), true)
    testna(Exists('x, 2, 4, Const(3) < 'x))
    test(Exists('x, 2, 4, Const(4) < 'x), false)
    test(Exists('x, 2, 4, Const(5) < 'x), false)

    // Verify linear around 0
    test(Exists('x, D.negInf, D.ZERO, Const(0) < 'x), false)
    test(Exists('x, D.negInf, D.ZERO, 'x < 0), true)
    test(Exists('x, D.ZERO, D.posInf, Const(0) < 'x), true)
    test(Exists('x, D.ZERO, D.posInf, 'x < 0), false)

    // Verify upper half,  linear positive and negative
    test(Exists('x, 42, D.posInf, 'x < 42), false)
    test(Exists('x, 42, D.posInf, Const(42) < 'x), true)
    test(Exists('x, -42, D.posInf, 'x < -42), false)
    test(Exists('x, -42, D.posInf, Const(-42) < 'x), true)

    // Verify lower half, linear positive and negative
    test(Exists('x, D.negInf, 42, 'x < 42), true)
    test(Exists('x, D.negInf, 42, Const(42) < 'x), false)
    test(Exists('x, D.negInf, -42, 'x < -42), true)
    test(Exists('x, D.negInf, -42, Const(-42) < 'x), false)

    // Verify rounded values
    testna(Exists('x, D.negInf, 422, Const(420) + Const(5) < 'x))
    test(Exists('x, D.negInf, 420, Const(420) + Const(5) < 'x), false)
    
    testna(Exists('x, 1, 3, Const(2) < 'x))
    testna(Exists('x, 0, 1, "0.5" < 'x * (Const(1) - 'x)))
  }
  
  @Test
  def testForall() = {
    test(Forall('x, 2, 4, Const(0) < 'x), true)
    test(Forall('x, 2, 4, Const(1) < 'x), true)
    testna(Forall('x, 2, 4, Const(2) < 'x))
    test(Forall('x, 2, 4, Const(3) < 'x), false)
    test(Forall('x, 2, 4, Const(4) < 'x), false)
    test(Forall('x, 2, 4, Const(5) < 'x), false)

    // Verify linear around 0
    test(Forall('x, -1, 0, Const(0) < 'x), false)
    testna(Forall('x, -1, 0, 'x < 0))
    testna(Forall('x, 0, 1, Const(0) < 'x))
    test(Forall('x, 0, 1, 'x < 0),false)

    // Verify upper half,  linear positive and negative
    test(Forall('x, 40, 41, 'x < 42), true)
    test(Forall('x, 40, 41, Const(42) < 'x), false)
    testna(Forall('x, -43, -42, 'x < -42))
    test(Forall('x, -41, -40, Const(-42) < 'x), true)

    // Verify lower half, linear positive and negative
    testna(Forall('x, 41, 42, 'x < 42))
    test(Forall('x, 41, 42, Const(42) < 'x), false)
    test(Forall('x, -44, -43, 'x < -42), true)
    test(Forall('x, -44, -42, Const(-42) < 'x), false)

    // Verify rounded values
    testna(Forall('x, 420, 422, Const(420) + Const(5) < 'x))
    testna(Forall('x, 429, 431, Const(420) + Const(5) < 'x))
    test(Forall('x, 431, 432, Const(420) + Const(5) < 'x), true)
  }
}