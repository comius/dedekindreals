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
import org.scalacheck.util.Pretty
import org.scalacheck.Gen

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

  // Verifies dualities does't hold. / They actually don't on infinities.
  property("multiplyDualityDoesnHold") = forAll {
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

  val eps = D.valueOfEpsilon(610)
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
      approx(xpyp, w) :| s" not ${xpyp} >> ${w} ${xpyp.d.compareTo(w.d)} ${w.u.compareTo(xpyp.u)}"
    }

  def checkExtensionRightForDivision(op: (Interval, Interval) => Interval)(x: Interval, y: Interval): Prop =
    {
      val xy = op(x, y)

      val xp = Interval(x.d.subtract(eps, mcd), x.u.add(eps, mcu))
      val yp =
        if (y.d == D.ZERO || y.u == D.ZERO) Interval(D.negInf, D.posInf)
        else Interval(y.d.subtract(eps, mcd), y.u.add(eps, mcu))

      val xpyp = op(xp, yp)
      val w = Interval(xy.d.subtract(eps2, mcd), xy.u.add(eps2, mcu)) // xy >> w
      approx(xpyp, w) :| s" not ${xpyp} >> ${w} ${x}/${y}=${x}*${y.inverse(new RoundingContext(0, 620))}>>${w},  ${x}>>${xp}, ${y}>>{$yp}, ${xpyp}<<${w}"
    }

  def checkExtensionLeft(op: (Interval, Interval) => Interval)(xp: Interval, yp: Interval): Prop =
    {
      val xpyp = op(xp, yp)
      // Go a bit below xp op yp, this handles a special case when result is precise, i.e [0,0]
      val w = Interval(xpyp.d.subtract(eps2, mcd), xpyp.u.add(eps2, mcu))
      assert(approx(xpyp, w))

      // Compute intervals only a bit above xp,yp
      def liftD(x: D.T) = if (!x.isNegInf) x else FloatSpec.genRegularFloat.sample.get
      def liftU(x: D.T) = if (!x.isPosInf) x else FloatSpec.genRegularFloat.sample.get

      val x = Interval(liftD(xp.d.add(eps, mcd)), liftU(xp.u.subtract(eps, mcu)))
      val y = Interval(liftD(yp.d.add(eps, mcd)), liftU(yp.u.subtract(eps, mcu)))

      // Verify intervals approximated by xp and yp that are 'only a bit above'
      (approx(op(x, y), w) :| s" not ${op(x, y)} >> ${w}" &&

        // Verify for all intervals x,y generated randomly, approximated by xp and yp
        forAll(approxInterval(xp))((x: Interval) =>
          forAll(approxInterval(yp))((y: Interval) =>
            approx(op(x, y), w) :| s"${op(x, y)}<<${w}")))
    }

  /*
   * Tests extension property for all arithmetic operations.
   */

  val add: (Interval, Interval) => Interval = _.add(_, rinf)
  val subtract: (Interval, Interval) => Interval = _.subtract(_, rinf)
  val multiply: (Interval, Interval) => Interval = _.multiply(_, rinf)
  val divide: (Interval, Interval) => Interval = _.divide(_, new RoundingContext(0, 320))

  for ((opDesc, op) <- Map("Add" -> add, "Subtract" -> subtract, "Multiply" -> multiply, "Divide" -> divide)) {

    if (op == divide) {
      // Division is tricky in (=>) direction.
      property(s"${opDesc}ExtensionToRight") = forAll(checkExtensionRight(op)(_, _))
      property(s"${opDesc}ExtensionToRightSpecialValues") =
        forall(specialIntervals, specialIntervals)(checkExtensionRight(op)(_, _))
    } else {
      // Tests extension in (=>) direction with arbitrary random intervals.
      property(s"${opDesc}ExtensionToRight") = forAll(checkExtensionRight(op)(_, _))
      
      // Tests extension in (=>) direction with ALL special intervals (endpoints are special values).
      property(s"${opDesc}ExtensionToRightSpecialValues") =
        forall(specialIntervals, specialIntervals)(checkExtensionRight(op)(_, _))
    }

    // Tests extension in (<=) direction with arbitrary random intervals.
    property(s"${opDesc}ExtensionToLeft") = forAll(checkExtensionLeft(op)(_, _))

    // Tests extension in (<=) direction with ALL special intervals (endpoints are special values).
    property(s"${opDesc}ExtensionToLeftSpecialValues") =
      forall(specialIntervals, specialIntervals)(checkExtensionLeft(op)(_, _))

  }
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

  val mc = new MathContext(0, RoundingMode.UNNECESSARY)

  /**
   * Generates floats approximating xd below.
   *
   * @param xd value to approximate
   * @return approximation
   */
  def approxAbove(xd: D.T): Gen[D.T] =
    for {
      d <- FloatSpec.genRegularFloat
    } yield if (xd == D.negInf) d else xd.add(d.abs(), mc)

  /**
   * Generates floats approximating xu below.
   *
   * @param xu value to approximate
   * @return approximation
   */
  def approxBelow(xu: D.T): Gen[D.T] =
    for {
      u <- FloatSpec.genRegularFloat
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

  /**
   * A special forall that tests all values (not just random ones).
   *
   * Implemented in a form of property, because aggregating Prop-s with && throws stack exception.
   */
  def forall[A1, A2](g1: Stream[A1], g2: Stream[A2])(f: (A1, A2) => Prop)(implicit pp1: A1 => Pretty, pp2: A2 => Pretty): Prop = Prop {
    prms0 =>
      def aggregate: Prop.Result = {
        var r = Result(Prop.True)
        for {
          gv1 <- g1
          gv2 <- g2
        } {
          r = r && f(gv1, gv2)(prms0)
          if (r.status != Prop.True)
            return r
              .addArg(Prop.Arg("ARG1", gv2, 0, gv2, pp2(gv2), pp2(gv2)))
              .addArg(Prop.Arg("ARG0", gv1, 0, gv1, pp1(gv1), pp1(gv1)))
        }
        return Result(Prop.Proof)
      }
      aggregate
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