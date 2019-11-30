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

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.interval.Interval

/**
 * Unit tests for Floats - property based.
 *
 * Environment: no specific test environment needs to be set up. Java/Scala provide everything in default installation.
 */
@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class FloatsSpec extends Properties("Floats") {
  import FloatsSpec._

  /**
   * Implicitly defines arbitrary float, which is used in forAll tests when no generator is given.
   */
  implicit def arbFloat: Arbitrary[D.T] = Arbitrary { genFloat }

  /*
   * Testing linearity.
   */
  property(s"linearity") = forAll {
    (a: D.T, b: D.T) =>
      // Verifies compareTo and equals don't throw exception and terminates
      a.compareTo(b)
      b.compareTo(a)
      val x = a.equals(b)

      // Verifies the results are consistent.
      x == (a.compareTo(b) == 0) && x == (b.compareTo(a) == 0) && (x == b.equals(a))
  }

  /*
   * Testing transitivity.
   */
  property(s"transitivity") = forAll {
    (a: D.T, b: D.T, c: D.T) =>
      implicit val ordering = Ordering[D.T] { _.compareTo(_) }
      val s = List(a, b, c).sorted

      // Verifies transitivity holds.
      s(0).compareTo(s(1)) <= 0 && s(1).compareTo(s(2)) <= 0 && s(0).compareTo(s(2)) <= 0
  }

  /*
   * Testing min and max.
   */
  property(s"MinMax") = forAll {
    (a: D.T, b: D.T) =>
      val min = a.min(b)
      val max = a.max(b)

      // Verifies consistency of compareTo and min, max functions.
      min == b.min(a) && max == b.max(a) &&
        (min == a || min == b) &&
        (max == a || max == b) &&
        (min.compareTo(a) <= 0 && min.compareTo(b) <= 0) &&
        (max.compareTo(a) >= 0 && max.compareTo(b) >= 0)
  }

  /*
   * Testing signum.
   */
  property(s"signum") = forAll {
    // Verifies consistency with compareTo zero.
    (a: D.T) => a.signum() == a.compareTo(D.ZERO)
  }

  /*
   * Testing negation.
   */
  property(s"negate") = forAll(FloatsSpec.genRegularFloat) {
    (a: D.T) =>
      // Verifies a + (-a) == 0
      a.negate.add(a, MathContext.UNLIMITED) == D.ZERO
  }
  assert(D.posInf.negate == D.negInf)
  assert(D.negInf.negate == D.posInf)

  /*
   * Testing rounding for arithmetic operations
   */
  val add: (D.T, D.T, MathContext) => D.T = _.add(_, _)
  val subtract: (D.T, D.T, MathContext) => D.T = _.subtract(_, _)
  val mult: (D.T, D.T, MathContext) => D.T = _.multiply(_, _)
  val divide: (D.T, D.T, MathContext) => D.T = _.divide(_, _)

  for { (opDesc, op) <- Map("Add" -> add, "Subtract" -> subtract, "Multiply" -> mult, "Divide" -> divide) } {
    property(s"rounding${opDesc}") = forAll(genRegularFloat, genRegularFloat) {
      (a: D.T, b: D.T) =>
        (opDesc != "Divide" || b != D.ZERO) ==> // Omitting division by zero
          {
            val precise =
              try {
                op(a, b, MathContext.UNLIMITED)
              } catch {
                case e: ArithmeticException =>
                  // Special case for division which can't be computed precisely
                  val largePrecision = 1000
                  op(a, b, new MathContext(largePrecision, RoundingMode.HALF_EVEN))
              }

            val propList =
              // Going through different precisions in [1,100]
              for (precision <- 1 to 100) yield {
                val rUp = new MathContext(precision, RoundingMode.CEILING)
                val rDown = new MathContext(precision, RoundingMode.FLOOR)
                val cUp = op(a, b, rUp)
                val cDown = op(a, b, rDown)

                (
                  // Verifies result rounded up is above precise result.
                  (cUp.compareTo(precise) >= 0)
                  :| s"Rounding up is not above precise result ${cUp} < ${precise}" &&

                  // Verifies result rounded up - ULP is below precise result.
                  (cUp.subtract(D.valueOfEpsilon(precision), rDown).compareTo(precise) <= 0)
                  :| s"Subtracting ULP is not below precise result" &&

                  // Verifies result rounded down is below precise result.
                  (cDown.compareTo(precise) <= 0)
                  :| s"Rounding down is not below precise result ${cDown} > ${precise}" &&

                  // Verifies result rounded down + ULP is above precise result.
                  (cDown.add(D.valueOfEpsilon(precision), rUp).compareTo(precise) >= 0)
                  :| s"Adding ULP is not above precise result")
              }
            propList.reduce(_ && _)
          }
    }
  }

  /*
   * Testing interpolation and extrapolation.
   */
  property("interpolation") = forAll(genRegularFloat, genRegularFloat) {
    (a: D.T, b: D.T) =>
      // Only test on a < b.
      (!a.equals(b)) ==> {
        val (a2, b2) = if (a.compareTo(b) <= 0) (a, b) else (b, a)
        val c = a2.split(b2)

        // Verifies a < split(a,b) < b
        a2.compareTo(c) < 0 && c.compareTo(b2) < 0
      }
  }

  property("extrapolation") = forAll(genRegularFloat) {
    (a: D.T) =>
      val x = D.negInf.split(a)
      val y = a.split(D.posInf)

      // Verifies split(-inf,a)<a  and a < split(a,+inf).
      x.compareTo(a) < 0 && a.compareTo(y) < 0
  }

  /*
   * Testing trisection.
   */
  val precision = 10
  property("trisection") = forAll(genRegularFloat, genRegularFloat) {
    (a: D.T, b: D.T) =>
      // Only test on a < b.
      (!a.equals(b)) ==> {
        val (a2, b2) = if (a.compareTo(b) <= 0) (a, b) else (b, a)
        val (c1, c2) = a2.trisect(b2, precision)

        (a2.compareTo(c1) < 0 && c1.compareTo(c2) < 0 && c2.compareTo(b2) < 0) :| s"Failed result ($c1,$c2)"
      }
  }
}

/**
 * Provides generators for Floats.
 */
object FloatsSpec {
  /**
   * Generates a regular float (without infinities).
   *
   * Uses scalacheck's generator for BigDecimal. Scalacheck's generator generates 3 precisions: 32, 64, and 128 bit.
   * It generates 4 groups of numbers:
   *   - whole, i.e. BigInteger (frequency 5)
   *   - small, i.e. n/d where n and d are random long (frequency 10)
   *   - large, i.e. BigInteger * 10^n, when n is from [-300,300] (frequency 10)
   *   - specific big decimals: 0,1,-1,1e300,1e-300 (frequency 5)
   *
   * BigInts are generated from: 0, 1, -1, max/min int/long +/- 0/1, product of 1/2/3/4 random longs.
   */
  def genRegularFloat: Gen[D.T] =
    for {
      a <- arbitrary[scala.math.BigDecimal]
    } yield D.valueOf(a.underlying().toString(), new MathContext(a.underlying().precision()))

  /**
   * Generates arbitrary float (with infinities).
   */
  def genFloat: Gen[D.T] = Gen.frequency((5, genRegularFloat), (1, D.posInf), (1, D.negInf))

  /**
   * Generates a regular float on [0,1].
   */
  def gen01Float: Gen[D.T] =
    for {
      d <- Gen.choose(0, Long.MaxValue).map(x => if (x == 0) 1 else x)
      n <- Gen.choose(0, d)
    } yield D.valueOf(n).divide(D.valueOf(d.toString, MathContext.DECIMAL128), MathContext.DECIMAL128)

  /**
   * Generates a regular float on interval.
   */
  def genFloatInterval(i: Interval): Gen[D.T] =
    for {
      f <- gen01Float
    } yield f.multiply(i.d, MathContext.DECIMAL128).add(
      D.ONE.subtract(f, MathContext.DECIMAL128).multiply(i.u, MathContext.DECIMAL128), MathContext.DECIMAL128)
}
