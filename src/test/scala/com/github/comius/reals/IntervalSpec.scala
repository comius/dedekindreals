package com.github.comius.reals
import java.math.MathContext
import java.math.RoundingMode

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Prop.forAll

import org.scalacheck.Properties

import com.github.comius.RoundingContext
import com.github.comius.floats.Floats.{ impl => D }
import org.scalacheck.Gen

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class IntervalSpec extends Properties("Interval") {
  val r = new RoundingContext(0, 10)

  def in(a: D.T, i: Interval): Boolean = {
    a.compareTo(i.d) >= 0 && i.u.compareTo(a) >= 0
  }

  implicit def arbInterval: Arbitrary[Interval] =
    Arbitrary {
      for {
        a <- arbitrary[scala.math.BigDecimal]
        b <- arbitrary[scala.math.BigDecimal]
      } yield Interval(
        D.valueOf(a.underlying().toString(), new MathContext(a.underlying().precision())),
        D.valueOf(b.underlying().toString(), new MathContext(a.underlying().precision())))
    }

  property("addEndpointsIn") = forAll {
    (a: Interval, b: Interval) =>
      (a.d.compareTo(a.u) <= 0 && b.d.compareTo(b.u) <= 0) ==>
        {
          val c = a.add(b, r)
          val lower = a.d.add(b.d, r.down)
          val upper = a.u.add(b.u, r.up)
          in(lower, c) :| s"lower ${lower} not in ${c}" &&
            in(upper, c) :| s"upper ${upper} not in ${c}"
        }
  }

  property("subEndpointsIn") = forAll {
    (a: Interval, b: Interval) =>
      (a.d.compareTo(a.u) <= 0 && b.d.compareTo(b.u) <= 0) ==>
        {
          val c = a.subtract(b, r)
          val lower = a.d.subtract(b.u, r.down)
          val upper = a.u.subtract(b.d, r.up)
          in(lower, c) :| s"lower ${lower} not in ${c}" &&
            in(upper, c) :| s"upper ${upper} not in ${c}"
        }
  }

  property("multiplyComutative") = forAll {
    (a: Interval, b: Interval) =>
      (a.multiply(b, r) == b.multiply(a, r)) :| s"${a.multiply(b, r)} != ${b.multiply(a, r)}"
  }

  property("multiplyKaucherComutative") = forAll {
    (a: Interval, b: Interval) =>
      (a.multiplyKaucher(b, r) == b.multiplyKaucher(a, r)) :|
        s"${a.multiplyKaucher(b, r)} != ${b.multiplyKaucher(a, r)}"
  }

  property("multiplyDuality") = forAll {
    (a: Interval, b: Interval) =>
      (a.flip.multiply(b.flip, r.swap()).flip == a.multiply(b, r))
  }

  property("multiplyKaucherEqualsLakayev") = forAll {
    (a: Interval, b: Interval) =>
      (a.multiplyLakayev(b, r) == a.multiplyKaucher(b, r)) :|
        s"${a.multiplyLakayev(b, r)} != ${a.multiplyKaucher(b, r)}"
  }

  {
    def arbFloat: Arbitrary[D.T] =
      Arbitrary {
        for {
          a <- arbitrary[scala.math.BigDecimal]
        } yield D.valueOf(a.underlying().toString(), new MathContext(a.underlying().precision()))
      }

    implicit def arbFloatWithEndpoints: Arbitrary[D.T] =
      Arbitrary {
        Gen.frequency(
          (10, arbFloat.arbitrary),
          (1, Gen.const(D.posInf)), (1, Gen.const(D.negInf)))
      }
    
    /*implicit def nastyFloatWithEndpoints: Arbitrary[D.T] =
      Arbitrary {
        Gen.oneOf(D.negInf, D.posInf, D.ZERO, D.ONE, D.ONE.negate())
      }*/

    implicit def arbInterval: Arbitrary[Interval] =
      Arbitrary {
        for {
          a <- arbitrary[D.T]
          b <- arbitrary[D.T]
        } yield Interval(a, b)
      }

    def approx(i1: Interval, i2: Interval) = {
      (i1.d.compareTo(i2.d) > 0 || (i1.d == i2.d && !i1.d.isRegularNumber())) &&
        (i2.u.compareTo(i1.u) > 0 || (i1.u == i2.u && !i1.u.isRegularNumber()))
    }

    val eps = D.valueOfEpsilon(1000)
    val eps2 = D.valueOfEpsilon(10)
    val mcd = new MathContext(0, RoundingMode.FLOOR)
    val mcu = new MathContext(0, RoundingMode.CEILING)
    val rinf = new RoundingContext(0, 0)

    // Multiply extension direction (=>) only works when x and y are not inf, otherwise we can find w
    property("multiplyExtensionToRight") = forAll {
      (x: Interval, y: Interval) =>
        //   (x.d.compareTo(x.u) <= 0 && y.d.compareTo(y.u) <= 0) ==>
        {
          val xy = x.multiply(y, rinf)

          val xp = Interval(x.d.subtract(eps, mcd), x.u.add(eps, mcu))
          val yp = Interval(y.d.subtract(eps, mcd), y.u.add(eps, mcu))

          val xpyp = xp.multiply(yp, rinf)

          val w = Interval(xy.d.subtract(eps2, mcd), xy.u.add(eps2, mcu)) // xy >> w
          approx(xpyp, w) :| s" not ${xpyp} >> ${w}"
        }
    }

   /* val floats = List(D.negInf, D.posInf, D.ZERO, D.ONE, D.ONE.negate())
    for {
      d <- floats
      u <- floats
      e <- floats
      t <- floats
      au <- List(D.ONE, D.ZERO, D.ONE.negate())
      bu <- List(D.ONE, D.ZERO, D.ONE.negate())
    }*/
    
    property("multiplyExtensionToLeft") = forAll {
      (xp: Interval, yp: Interval) =>
        // (xp.d.compareTo(xp.u) < 0 && yp.d.compareTo(yp.u) < 0) ==>
        {
          val xpyp = xp.multiply(yp, rinf)

          def liftD(x: D.T) = if (!x.isNegInf) x else arbFloat.arbitrary.sample.get
          def liftU(x: D.T) = if (!x.isPosInf) x else arbFloat.arbitrary.sample.get

          val x = Interval(liftD(xp.d.add(eps, mcd)), liftU(xp.u.subtract(eps, mcu)))
          val y = Interval(liftD(yp.d.add(eps, mcd)), liftU(yp.u.subtract(eps, mcu)))
          val xy = x.multiply(y, rinf)

          (approx(xy, xpyp) || (xy == Interval(D.ZERO, D.ZERO) && xpyp == Interval(D.ZERO, D.ZERO))) :| s" not ${xy} >> ${xpyp}"

        }
    }



    //println(Interval(D.ONE, D.posInf).multiply(Interval(D.ZERO, D.ONE), r))
    //println(Interval(D.posInf, D.ONE).multiply(Interval(D.ONE, D.ZERO), r))
  }
}