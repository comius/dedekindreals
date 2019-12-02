/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.interval
import java.math.MathContext
import java.math.RoundingMode

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Prop.exists
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import com.github.comius.RoundingContext
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.floats.FloatsSpec
import com.github.comius.reals.TestUtil.forall

/**
 * Unit tests for Intervals.
 *
 * Environment: no specific test environment needs to be set up. Java/Scala provide everything in default installation.
 */
@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class IntervalSpec extends Properties("Interval") {
  import IntervalSpec._

  val precision = 10
  val r = new RoundingContext(0, precision)

  // Verifies multiplication is commutative.
  property("multiplyCommutative") = forAll {
    (a: Interval, b: Interval) =>
      (a.multiply(b, r) == b.multiply(a, r)) :| s"${a.multiply(b, r)} != ${b.multiply(a, r)}"
  }

  // Verifies dualities does't hold. / They actually don't on infinities.
  property("multiplyDualityDoesntHold") = exists { (a: Interval) =>
    exists { (b: Interval) =>
      (a.flip.multiply(b.flip, r.swap()).flip != a.multiply(b, r))
    }
  }

  // Verifies that if we're multiplying two proper intervals, we get a proper interval (and to improper give improper one)
  property("multiplyGivesSameDirection") = forAll { (a: Interval, b: Interval) =>
    val ad = a.d.compareTo(a.u)
    val bd = b.d.compareTo(b.u)
    math.abs(ad - bd) <= 1 ==> {
      val ab = a.multiply(b, r)
      val abd = ab.d.compareTo(ab.u)
      (math.abs(ad - abd) <= 1 && math.abs(bd - abd) <= 1) :| s"$ab $ad $bd $abd"
    }
  }

  // Verifies results Kaucher are equal to results Lakayev
  property("multiplyKaucherEqualsLakayev") = forAll {
    (a: Interval, b: Interval) =>
      val lakayev = a.multiplyLakayev(b, r)
      val kaucher = a.multiplyKaucher(b, r)
      (lakayev == kaucher) :|
        s"Lakayev ${lakayev} != Kaucher ${kaucher}"
  }

  /**
   * Checks that interval {@code i1} approximates interval {@code i2}.
   *
   * Special case are infinite endpoints which approximate them-selves.
   *
   * @param i1 interval
   * @param i2 interval
   * @return {@code i1} << {@code i2}
   */
  private def approx(i1: Interval, i2: Interval) = {
    i1.d.compareTo(i2.d) < 0 && i2.u.compareTo(i1.u) < 0
  }

  private def leq(i1: Interval, i2: Interval) = {
    i1.d.compareTo(i2.d) <= 0 && i2.u.compareTo(i1.u) <= 0
  }

  private def makeApprox(i: Interval, eps: D.T, inf: D.T): Interval = {
    Interval(
      if (!i.d.isPosInf) i.d.subtract(eps, mc) else inf,
      if (!i.u.isNegInf) i.u.add(eps, mc) else inf.negate())
  }

  private def makeUpperApprox(i: Interval, eps: D.T, inf: D.T): Interval = {
    Interval(
      if (!i.d.isNegInf) i.d.add(eps, mc) else inf,
      if (!i.u.isPosInf) i.u.subtract(eps, mc) else inf.negate())
  }

  val bigPrecision = 500
  val inf = D.valueOfEpsilon(-10)
  val bigInf = D.valueOfEpsilon(-500)
  private val eps = D.valueOfEpsilon(bigPrecision) // small eps
  private val eps2 = D.valueOfEpsilon(precision) // big eps
  private val mc = new MathContext(0, RoundingMode.UNNECESSARY)

  /**
   * Checks extension property in (=>).
   * There is an approximation of each operand that is also approximation of the result.
   *
   * @param op The operation to test.
   * @param x First operand
   * @param y Second operand
   *
   * @return property, that tests this
   */
  def checkExtensionOuter(op: (Interval, Interval) => Interval)(x: Interval, y: Interval): Prop =
    {
      val xy = op(x, y)
      val w = makeApprox(xy, eps2, inf)

      val xp = makeApprox(x, eps, bigInf)
      val yp = makeApprox(y, eps, bigInf)

      val xpyp = op(xp, yp)

      (leq(xpyp, xy) :| s"not monotone $xp*$yp=$xpyp <= $x*$y=$xy"
        && (!approx(w, xy) || approx(w, xpyp)) :| s" not $w << $xp*$yp=$xpyp <= $x*$y=$xy")
    }

  /**
   * Checks extension property in (<=). The operation on values operands approximate is also approximated by the result.
   *
   * @param op The operation to test.
   * @param x First operand
   * @param y Second operand
   *
   * @return property, that tests this
   */
  def checkExtensionInner(op: (Interval, Interval) => Interval)(xp: Interval, yp: Interval): Prop =
    {
      val xpyp = op(xp, yp)
      val w = makeApprox(xpyp, eps2, inf)

      val x = makeUpperApprox(xp, eps, inf)
      val y = makeUpperApprox(yp, eps, inf)

      val xy = op(x, y)

      // Verify intervals approximated by xp and yp that are 'only a bit above'
      leq(xpyp, xy) :| s"not monotone $xpyp <= $xy" &&
        !approx(w, xpyp) ||
        (approx(w, xy) :| s" not $w << $xy" &&

          // Verify for all intervals x,y generated randomly, approximated by xp and yp
          forAll(approxInterval(xp))((x: Interval) =>
            forAll(approxInterval(yp))((y: Interval) => {
              val xy = op(x, y)
              leq(xpyp, xy) :| s"not monotone $xpyp <= $xy" &&
                approx(w, xy) :| s"not $w << $xy"
            })))
    }

  /*
   * Tests extension property for all binary arithmetic operations.
   */
  private val rinf = new RoundingContext(0, 0)
  private val hugePrecision = 100
  private val rh = new RoundingContext(0, hugePrecision)

  private val add: (Interval, Interval) => Interval = _.add(_, rinf)
  private val subtract: (Interval, Interval) => Interval = _.subtract(_, rinf)
  private val multiply: (Interval, Interval) => Interval = _.multiply(_, rinf)
  private val divide: (Interval, Interval) => Interval = _.divide(_, rh)

  for ((opDesc, op) <- Map("Add" -> add, "Subtract" -> subtract, "Multiply" -> multiply, "Divide" -> divide)) {

    // Tests extension in (=>) direction with arbitrary random intervals.
    property(s"${opDesc}ExtensionOuter") = forAll(checkExtensionOuter(op)(_, _))

    // Tests extension in (=>) direction with ALL special intervals (endpoints are special values).
    property(s"${opDesc}ExtensionOuterSpecialValues") =
      forall(specialIntervals, specialIntervals)(checkExtensionOuter(op)(_, _))

    // Tests extension in (<=) direction with arbitrary random intervals.
    property(s"${opDesc}ExtensionInner") = forAll(checkExtensionInner(op)(_, _))

    // Tests extension in (<=) direction with ALL special intervals (endpoints are special values).
    property(s"${opDesc}ExtensionInnerSpecialValues") =
      forall(specialIntervals, specialIntervals)(checkExtensionInner(op)(_, _))

  }

  /*
   * Tests extension property on inverse.
   */
  def checkExtensionOuter(op: Interval => Interval)(x: Interval): Prop =
    {
      val xy = op(x)
      val w = makeApprox(xy, eps2, inf)

      val xp = makeApprox(x, eps, bigInf)

      val xpyp = op(xp)

      (leq(xpyp, xy) :| s"not monotone $xp=$xpyp <= $x=$xy"
        && (!approx(w, xy) || approx(w, xpyp)) :| s" not $w << $xp=$xpyp <= $x=$xy")
    }

  def checkExtensionInner(op: Interval => Interval)(xp: Interval): Prop =
    {
      val xpyp = op(xp)
      val w = makeApprox(xpyp, eps2, inf)

      val x = makeUpperApprox(xp, eps, inf)

      val xy = op(x)

      // Verify intervals approximated by xp and yp that are 'only a bit above'
      leq(xpyp, xy) :| s"not monotone $xpyp <= $xy" &&
        !approx(w, xpyp) ||
        (approx(w, xy) :| s" not $w << $xy" &&

          // Verify for all intervals x,y generated randomly, approximated by xp and yp
          forAll(approxInterval(xp))((x: Interval) => {
            val xy = op(x)
            leq(xpyp, xy) :| s"not monotone $xpyp <= $xy" &&
              approx(w, xy) :| s"not $w << $xy"
          }))
    }

  property("inverseExtensionOuter") =
    forAll(checkExtensionOuter((_: Interval).inverse(rh))(_))

  // Tests extension in (<=) direction with arbitrary random intervals.
  property("inverseExtensionToLeft") =
    forAll(checkExtensionInner((_: Interval).inverse(rh))(_))

  property("inverseExtensionToLeftOnSpecial") =
    Prop.all((for { i <- specialIntervals }
      yield checkExtensionInner((_: Interval).inverse(rh))(i)): _*)

  property("inverseExtensionOuterOnSpecial") =
    Prop.all((for { i <- specialIntervals }
      yield checkExtensionOuter((_: Interval).inverse(rh))(i)): _*)

}

/**
 * Provides generators for Interval.
 */
object IntervalSpec {
  /**
   * Implicitly defines arbitrary Interval, which is used in forAll tests when no generator is given.
   */
  implicit def arbInterval: Arbitrary[Interval] =
    Arbitrary {
      for {
        d <- FloatsSpec.genFloat
        u <- FloatsSpec.genFloat
      } yield Interval(d, u)
    }

  private val mc = new MathContext(0, RoundingMode.UNNECESSARY)

  /**
   * Generates floats approximating xd below.
   *
   * @param xd value to approximate
   * @return approximation
   */
  def approxAbove(xd: D.T): Gen[D.T] =
    for {
      d <- FloatsSpec.genRegularFloat
    } yield if (xd == D.negInf) d else xd.add(d.abs(), mc)

  /**
   * Generates floats approximating xu below.
   *
   * @param xu value to approximate
   * @return approximation
   */
  def approxBelow(xu: D.T): Gen[D.T] =
    for {
      u <- FloatsSpec.genRegularFloat
    } yield if (xu == D.posInf) u else xu.subtract(u.abs, mc)

  /**
   * Generates intervals that are approximated by x.
   *
   * @param x value that approximates
   * @return approximatee
   */
  def approxInterval(x: Interval): Gen[Interval] =
    for {
      d <- approxAbove(x.d)
      u <- approxBelow(x.u)
    } yield Interval(d, u)

  /** Stream of special Floats */
  val specialFloats = List(D.negInf, D.posInf, D.ZERO, D.ONE, D.ONE.negate())

  /** Stream of special Intervals */
  val specialIntervals =
    for {
      d <- specialFloats
      u <- specialFloats
    } yield Interval(d, u)
}
