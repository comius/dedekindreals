package com.github.comius.reals

import org.junit.runner.RunWith
import org.scalacheck.Gen
import java.math.MathContext
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.RoundingContext
import org.scalacheck.Prop.forAll
import com.github.comius.reals.syntax.Real._
import com.github.comius.reals.Approximate2D.Approximation
import org.scalacheck.Properties
import com.github.comius.reals.Approximate2D.Point
import org.scalacheck.Prop.BooleanOperators
import com.github.comius.reals.syntax.Const

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class Approximate2DSpec extends Properties("Approximate2d") {

  val gen01Float: Gen[D.T] =
    for {
      d <- Gen.choose(0, Long.MaxValue).map(x => if (x == 0) 1 else x)
      n <- Gen.choose(0, d)      
    } yield D.valueOf(n).divide(D.valueOf(d.toString, MathContext.DECIMAL128), MathContext.DECIMAL128)

  val i01 = Interval(D.ZERO, D.ONE)
  val cs0 = Approximate2D.ConstraintSet2D(i01, i01)
  val ctx0 = Context[VarDomain](new RoundingContext(0, 32))
  val m = MathContext.UNLIMITED
  
  property("lowerApproximate") = forAll(gen01Float, gen01Float, gen01Float) {
    (a: D.T, b: D.T, c: D.T) =>
      val (Approximation(l, u), s) = Approximate2D.refine(0 < 'x * a + 'y * b + c, cs0, 'x, 'y)(ctx0)
    forAll(gen01Float, gen01Float) {
        (x:D.T, y: D.T) =>
          
          l.isIn(Point(x, y)) == (x.multiply(a, m).add(y.multiply(b, m), m).add(c, m).compareTo(D.ZERO) < 0) &&
          u.isIn(Point(x, y)) == (x.multiply(a, m).add(y.multiply(b, m), m).add(c, m).compareTo(D.ZERO) > 0)
      }
  }
    
  val (Approximation(l, u), s) = Approximate2D.refine('y < 'x * (Const(1) - 'x), cs0, 'x, 'y)(ctx0)
  println(l)
  println(u)
  println(s)
  property("x1xApproximate") = 
    forAll(gen01Float, gen01Float) {
        (x:D.T, y: D.T) =>
          val m = MathContext.UNLIMITED
          l.isIn(Point(x, y)) ==> (x.multiply(D.ONE.subtract(x, m),m).compareTo(y) < 0)
              //&&
          //u.isIn(Point(x, y)) == (x.multiply(a, m).add(y.multiply(b, m), m).add(c, m).compareTo(D.ZERO) > 0)
     
  }   

}

