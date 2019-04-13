package com.github.comius.reals
import java.math.MathContext
import java.math.RoundingMode

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Prop.Result
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import com.github.comius.RoundingContext
import com.github.comius.floats.FloatSpec
import com.github.comius.floats.Floats.{ impl => D }

/**
 * Unit tests for Intervals.
 *
 * Environment: no specific test environment needs to be set up. Java/Scala provide everything in default installation.
 */
@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class IntervalSpec extends Properties("Interval") {
  import IntervalSpec._

  val r = new RoundingContext(0, 10)

  // Verifies multiplication is comutative.
  property("multiplyComutative") = forAll {
    (a: Interval, b: Interval) =>
      (a.multiply(b, r) == b.multiply(a, r)) :| s"${a.multiply(b, r)} != ${b.multiply(a, r)}"
  }

  // Verifies Kaucher multiplication is commutative. (TODO problems)
  property("multiplyKaucherComutative") = forAll {
    (a: Interval, b: Interval) =>
      (a.multiplyKaucher(b, r) == b.multiplyKaucher(a, r)) :|
        s"${a.multiplyKaucher(b, r)} != ${b.multiplyKaucher(a, r)}"
  }

  // Verifies dualities hold. / They actually don't on infinities.
  property("multiplyDuality") = forAll {
    (a: Interval, b: Interval) =>
      (a.flip.multiply(b.flip, r.swap()).flip == a.multiply(b, r))
  }

  // Verifies Kaucher multiplication is equal to Lakayev (TODO problems with infinities).
  property("multiplyKaucherEqualsLakayev") = forAll {
    (a: Interval, b: Interval) =>
      val lakayev = a.multiplyLakayev(b, r)
      val kaucher = a.multiplyKaucher(b, r)
      (lakayev == kaucher) :|
        s"Lakayev ${lakayev} != Kaucher ${kaucher}"
  }

  /**
   * Checks that interval {@code i2} approximates interval {@code i1}.
   *
   * Special case are infinite endpoints which approximate them-selves.
   *
   * @param i1 interval
   * @param i2 interval
   * @return {@code i1} >> {@code i2}
   */
  def approx(i1: Interval, i2: Interval) = {
    (i1.d.compareTo(i2.d) > 0 || (i1.d == i2.d && !i1.d.isRegularNumber())) &&
      (i2.u.compareTo(i1.u) > 0 || (i1.u == i2.u && !i1.u.isRegularNumber()))
  }

  val eps = D.valueOfEpsilon(1000)
  val eps2 = D.valueOfEpsilon(10)
  val mcd = new MathContext(0, RoundingMode.FLOOR)
  val mcu = new MathContext(0, RoundingMode.CEILING)
  val rinf = new RoundingContext(0, 0)

  def checkExtensionRight(op: (Interval, Interval) => Interval)(x: Interval, y: Interval): Prop =
    {
      val xy = op(x, y)

      val xp = Interval(x.d.subtract(eps, mcd), x.u.add(eps, mcu))
      val yp = Interval(y.d.subtract(eps, mcd), y.u.add(eps, mcu))

      val xpyp = op(xp, yp)
      val w = Interval(xy.d.subtract(eps2, mcd), xy.u.add(eps2, mcu)) // xy >> w
      approx(xpyp, w) :| s" not ${xpyp} >> ${w}"
    }

  def checkExtensionLeft(op: (Interval, Interval) => Interval)(xp: Interval, yp: Interval): Prop =
    {
      val xpyp = op(xp, yp)
      val w = Interval(xpyp.d.subtract(eps2, mcd), xpyp.u.add(eps2, mcu))

      def liftD(x: D.T) = if (!x.isNegInf) x else FloatSpec.genRegularFloat.sample.get
      def liftU(x: D.T) = if (!x.isPosInf) x else FloatSpec.genRegularFloat.sample.get

      val x = Interval(liftD(xp.d.add(eps, mcd)), liftU(xp.u.subtract(eps, mcu)))
      val y = Interval(liftD(yp.d.add(eps, mcd)), liftU(yp.u.subtract(eps, mcu)))
      val xy = op(x, y)

      approx(xy, w) :| s" not ${xy} >> ${w}"
    }

  val add: (Interval, Interval) => Interval = _.add(_, rinf)
  val subtract: (Interval, Interval) => Interval = _.subtract(_, rinf)
  val multiply: (Interval, Interval) => Interval = _.multiply(_, rinf)
  val divide: (Interval, Interval) => Interval = _.divide(_, new RoundingContext(0, 10000))

  for ((opDesc, op) <- Map("Add" -> add, "Subtract" -> subtract, "Multiply" -> multiply, "Divide" -> divide)) {
    property(s"${opDesc}ExtensionToRight") = forAll(checkExtensionRight(op)(_, _))

    property(s"${opDesc}ExtensionToRightSpecialValues") =
      forall(specialIntervals, specialIntervals)(checkExtensionRight(op)(_, _))

    property(s"${opDesc}ExtensionToLeft") = forAll(checkExtensionLeft(op)(_, _))

    property(s"${opDesc}ExtensionToLeftSpecialValues") =
      forall(specialIntervals, specialIntervals)(checkExtensionLeft(op)(_, _))

  }

  // println(Interval(D.ONE, D.posInf).multiply(Interval(D.ZERO, D.ONE), r))
  // println(Interval(D.posInf, D.ONE).multiply(Interval(D.ONE, D.ZERO), r))
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
        d <- FloatSpec.genFloat
        u <- FloatSpec.genFloat
      } yield Interval(d, u)
    }

  /**
   * A special forall that tests all values (not just random ones).
   *
   * Implemented in a form of property, because aggregating Prop-s with && throws stack exception.
   */
  def forall[A1, A2](g1: Stream[A1], g2: Stream[A2])(f: (A1, A2) => Prop): Prop = Prop {
    prms0 =>
      var r = Result(Prop.True)
      for {
        gv1 <- g1
        gv2 <- g2
      } (
        r = r && f(gv1, gv2)(prms0))
      if (r.status == Prop.True) Result(Prop.Proof) else r
  }

  /** Stream of special Floats */
  val specialFloats = Stream(D.negInf, D.posInf, D.ZERO, D.ONE, D.ONE.negate())

  /** Stream of special Intervals */
  val specialIntervals =
    for {
      d <- specialFloats
      u <- specialFloats
    } yield Interval(d, u)
}