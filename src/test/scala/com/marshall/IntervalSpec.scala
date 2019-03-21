package com.marshall
import java.math.BigDecimal

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class IntervalSpec extends Properties("Interval") {
  val r = new RoundingContext(0, 10)

  implicit def arbInterval: Arbitrary[Interval] =
    Arbitrary {
      for {
        a <- arbitrary[scala.math.BigDecimal]
        b <- arbitrary[scala.math.BigDecimal]
      } yield Interval(a.underlying(), b.underlying())
    }

  property("addEndpointsIn") = forAll {
    (a: Interval, b: Interval) =>
      (a.x.compareTo(a.y) <= 0 && b.x.compareTo(b.y) <= 0) ==>
        {
          val c = a.add(b, r)
          val lower = a.x.add(b.x, r.down)
          val upper = a.y.add(b.y, r.up)
          in(lower, c) :| s"lower ${lower} not in ${c}" &&
            in(upper, c) :| s"upper ${upper} not in ${c}"
        }
  }
  
  property("subEndpointsIn") = forAll {
    (a: Interval, b: Interval) =>
      (a.x.compareTo(a.y) <= 0 && b.x.compareTo(b.y) <= 0) ==>
        {
          val c = a.subtract(b, r)
          val lower = a.x.subtract(b.y, r.down)
          val upper = a.y.subtract(b.x, r.up)
          in(lower, c) :| s"lower ${lower} not in ${c}" &&
            in(upper, c) :| s"upper ${upper} not in ${c}"
        }
  }

  property("multiplyComutative") = forAll {
    (a: Interval, b: Interval) => (a.multiply(b, r) == b.multiply(a, r)) :| s"${a.multiply(b, r)} != ${b.multiply(a, r)}"
  }

  property("multiplyDuality") = forAll {
    (a: Interval, b: Interval) => (a.swap.multiply(b.swap, r.swap()).swap == a.multiply(b, r))
  }
   
  def in(a: BigDecimal, i: Interval): Boolean = {
    a.compareTo(i.x) >= 0 && i.y.compareTo(a) >= 0
  }

}