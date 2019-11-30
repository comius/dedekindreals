/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.floats

import java.math.MathContext
import java.math.RoundingMode

import scala.reflect.ClassTag

import org.junit.Test

import com.github.comius.floats.BigDecimalFloats.BigDecimalFloat
import com.github.comius.floats.Floats.{ impl => D }

/**
 * Unit tests for Floats.
 *
 * Floats are tested trough the Floats interface. Currently BigDecimal implementation is tested, but also any other
 * implementation could be tested similarly.
 *
 * Environment: no specific test environment needs to be set up. Java/Scala provide everything in default installation.
 *
 */
// scalastyle:off magic.number
// scalastyle:off regex
class FloatsTest {

  /** An integer representing the infinity. */
  val inf = 100

  // Next two arrays represent first and second operand for int operation and third array is the same operand for
  // Float operation. Operation can be addition, subtraction, multiplication or division.
  val testValuesInt1 = Array(inf + 5, -inf - 5, 0, 1, -1)
  val testValuesInt2 = Array(inf + 10, -inf - 10, 0, 1, -1)

  implicit val implClass: ClassTag[D.T] = ClassTag(classOf[BigDecimalFloat])
  val testValuesDyadic = Array(D.posInf, D.negInf, D.ZERO, D.valueOf(1), D.valueOf(-1))

  /** Normalizes infinities. Anything bigger or smaller is truncated to infinity. */
  def normalize(a: Int): Int = {
    if (a >= inf) {
      inf
    } else if (a <= -inf) {
      -inf
    } else {
      a
    }
  }

  /**
   * Tests limits of given binary arithmetic operation.
   *
   * @param op
   *            the arithmetic operation on Floats
   * @param intOp
   *            the same arithmetic operation on integers
   * @param opStr
   *            the string describing the operation
   */
  def testLimits(op: (D.T, D.T) => D.T, intOp: (Int, Int) => Int, opStr: String): Unit = {
    for { i <- 0 until testValuesInt1.length } {
      for { j <- 0 until testValuesInt1.length } {
        // Computes value using integers and embeds it into Floats
        val cprime: Option[D.T] =
          try {
            val ai = testValuesInt1(i)
            val bi = testValuesInt2(j)
            val ci: Int = normalize(intOp(ai, bi))
            val maxResult = 2

            if (ci >= inf) {
              Some(D.posInf)
            } else if (ci <= -inf) {
              Some(D.negInf)
            } else if (ci > maxResult || ci < -maxResult) {
              None
            } else {
              Some(D.valueOf(ci))
            }
          } catch {
            case e: ArithmeticException =>
              None
          }

        // The operation on floats
        val a = testValuesDyadic(i);
        val b = testValuesDyadic(j);
        try {
          val c: D.T = op(a, b);
          val logMessage = a + opStr + b + " = " + c + ": expected NaN";

          // Verifies if the result matches
          assert(cprime.isDefined, logMessage);
          assert(c == cprime.get, logMessage);
        } catch {
          case e: ArithmeticException =>
            val logMessage = a + opStr + b + " = NaN: " + cprime;

            // Verifies both results are NaN
            assert(cprime.isEmpty, logMessage);
        }
      }
    }

  }

  /**
   * Tests limits of arithmetic operations.
   */
  @Test
  def testLimits(): Unit = {
    val precision = 10

    // Test addition
    testLimits((a, b) => a.add(b, new MathContext(precision, RoundingMode.CEILING)), (a, b) => a + b, " + ");

    // Test subtraction
    testLimits((a, b) => a.subtract(b, new MathContext(precision, RoundingMode.CEILING)), (a, b) => a - b, " - ");

    val isInf = (a: Int) => Math.abs(normalize(a)) == inf;

    // Multiplication
    // Special case multiplication inf and 0
    val multiply = (a: Int, b: Int) =>
      if ((isInf(a) && b == 0) || (isInf(b) && a == 0)) {
        throw new ArithmeticException()
      } else { a * b }
    testLimits((a, b) => a.multiply(b, new MathContext(precision, RoundingMode.CEILING)), multiply, " * ");

    // Division
    // Special case division of infinities
    val divide = (a: Int, b: Int) => if (isInf(a) && isInf(b)) throw new ArithmeticException() else a / b;
    testLimits((a, b) => a.divide(b, new MathContext(precision, RoundingMode.CEILING)), divide, " / ");
  }

  def getPrec(a: Int): Int = {
    var p = 10
    if (a == 0) return p;
    var aa = a
    while ((aa % 2) == 0) {
      aa = aa / 2
      p = p - 1;
    }
    p
  }

  /**
   * Empirical tests of trisection.
   *
   * First test splits at 1/4 and 1/4 - assumption is that we can keep precision lower, to be more efficient.
   *
   * Second test splits at 1/2-eps and 1/2+eps - assumption is that we are more efficient by having less iterations.
   *
   * Third test splits at 2 numbers with lowest precision, at least it tries.
   *
   * Conclusion: Second test always wins by having less iteration, because other tests have tail iterations with highest
   * precision, which don't balance with initial iterations at lower precision.
   */
  def testTrisection(): Unit = {
    val s = Integer.parseInt("00001010100", 2);
    var a = 0
    var b = Integer.parseInt("100000000000", 2);
    var i0 = 0;
    var psum = 0;
    while (b - a > 2) {
      var a1 = (3 * a + b) / 4
      var b1 = (a + 3 * b) / 4

      if (a1 < s) a = a1;
      if (s < b1) b = b1;
      println(s" iteration $i0 precision $a1 $b1 ${getPrec(a1)} ${getPrec(b1)}")
      psum += getPrec(a1) + getPrec(b1);
      i0 += 1
    }
    println(s"$a $b $s")
    println(psum);
  }

  def testTrisection2(): Unit = {
    val s = Integer.parseInt("00001010100", 2);
    var a = 0
    var b = Integer.parseInt("100000000000", 2);
    var i = 0;
    var psum = 0;
    while (b - a > 2) {
      val a1 = (a + b) / 2 - 1
      val b1 = (a + b) / 2 + 1
      if (a1 < s) a = a1;
      if (s < b1) b = b1;
      println(s" iteration $i precision $a1 $b1  ${getPrec(a1)} ${getPrec(b1)}")
      psum += getPrec(a1) + getPrec(b1);
      i += 1
    }
    println(s"$a $b $s")
    println(psum);
  }

  def testTrisection3(): Unit = {
    val s = Integer.parseInt("00001010100", 2);
    var a = 0
    var b = Integer.parseInt("100000000000", 2);
    var i0 = 0;
    var psum = 0;
    var p = b
    while (b - a > 2) {
      var a1 = (a + b) / 2
      var b1 = (a + b) / 2
      while (a1 - p <= a && b1 + p >= b) p /= 2
      a1 = a1 - p
      b1 = b1 + p

      if (a1 < s) a = a1;
      if (s < b1) b = b1;
      println(s" iteration $i0 precision $a1 $b1 ${getPrec(a1)} ${getPrec(b1)} $p")
      psum += getPrec(a1) + getPrec(b1);
      i0 += 1
    }
    println(s"$a $b $s")
    println(psum);
  }
}

