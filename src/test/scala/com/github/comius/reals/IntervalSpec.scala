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
import org.scalacheck.Prop

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class IntervalSpec extends Properties("Interval") {
  val r = new RoundingContext(0, 10)

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
      val lakayev = a.multiplyLakayev(b, r)
      val kaucher = a.multiplyKaucher(b, r)
      (lakayev == kaucher) :|
        s"Lakayev ${lakayev} != Kaucher ${kaucher}"
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

  def checkExtensionRight(x: Interval, y: Interval): Prop =
    //   (x.d.compareTo(x.u) <= 0 && y.d.compareTo(y.u) <= 0) ==>
    {
      val xy = x.multiply(y, rinf)

      val xp = Interval(x.d.subtract(eps, mcd), x.u.add(eps, mcu))
      val yp = Interval(y.d.subtract(eps, mcd), y.u.add(eps, mcu))

      val xpyp = xp.multiply(yp, rinf)

      val w = Interval(xy.d.subtract(eps2, mcd), xy.u.add(eps2, mcu)) // xy >> w
      approx(xpyp, w) :| s" not ${xpyp} >> ${w}"
    }

  // Multiply extension direction (=>) only works when x and y are not inf, otherwise we can find w
  property("multiplyExtensionToRight") = forAll(checkExtensionRight(_, _))

  val floats = List(D.negInf, D.posInf, D.ZERO, D.ONE, D.ONE.negate())
  property("multiplyExtensionToRightSpecialValues") = {
    val all = for {
      d <- floats
      u <- floats
      e <- floats
      t <- floats
    } yield checkExtensionRight(Interval(d, u), Interval(e, t))
    all.reduce(_ && _)
  }

  def checkExtensionLeft(xp: Interval, yp: Interval): Prop =
    {
      val xpyp = xp.multiply(yp, rinf)
      val w = Interval(xpyp.d.subtract(eps2, mcd), xpyp.u.add(eps2, mcu))

      def liftD(x: D.T) = if (!x.isNegInf) x else arbFloat.arbitrary.sample.get
      def liftU(x: D.T) = if (!x.isPosInf) x else arbFloat.arbitrary.sample.get

      val x = Interval(liftD(xp.d.add(eps, mcd)), liftU(xp.u.subtract(eps, mcu)))
      val y = Interval(liftD(yp.d.add(eps, mcd)), liftU(yp.u.subtract(eps, mcu)))
      val xy = x.multiply(y, rinf)

      approx(xy, w) :| s" not ${xy} >> ${w}"
    }

  property("multiplyExtensionToLeft") = forAll(checkExtensionLeft(_, _))
  
  property("multiplyExtensionToLeftSpecialValues") = {
    val all = for {
      d <- floats
      u <- floats
      e <- floats
      t <- floats      
    } yield checkExtensionLeft(Interval(d, u), Interval(e, t))
    all.reduce(_ && _)
  }        
        
  //println(Interval(D.ONE, D.posInf).multiply(Interval(D.ZERO, D.ONE), r))
  //println(Interval(D.posInf, D.ONE).multiply(Interval(D.ONE, D.ZERO), r))

}