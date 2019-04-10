package com.github.comius.reals
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Prop.forAll

import com.github.comius.RoundingContext;

import org.scalacheck.Properties

import com.github.comius.floats.Floats.{ impl => D }
import java.math.MathContext

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



}