package com.github.comius.reals.newton

import org.junit.runner.RunWith
import org.scalacheck.Properties
import com.github.comius.floats.FloatSpec
import org.scalacheck.Arbitrary
import com.github.comius.reals.newton.ConstraintSet.MoreThan
import com.github.comius.reals.newton.ConstraintSet.LessThan
import com.github.comius.reals.newton.ConstraintSet.RealConstraint
import org.scalatest.prop.Generator
import org.scalacheck.Gen
import org.scalatest.prop.Generator
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.Interval
import org.scalatest.prop.Generator
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetAll
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetNone
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetList

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class ConstraintSetSpec extends Properties("ConstraintSet") {
  val d = Interval(D.negInf, D.posInf)

  def listConstraintSet: Gen[ConstraintSetList] =
    for {
      cl <- Gen.listOf(FloatSpec.genRegularFloat)
      if cl.size > 0
      c <- Gen.oneOf(0, 1)
    } yield ConstraintSetList(
      d,
      cl.sortWith(_.compareTo(_) <= 0).distinct.zipWithIndex.map {
        case (d, i) if i % 2 == c => MoreThan(d)
        case (d, _)               => LessThan(d)
      })

  implicit def arbConstraintSet: Arbitrary[ConstraintSet] = Arbitrary {
    Gen.frequency((1, ConstraintSetAll(d)), (1, ConstraintSetNone(d)), (5, listConstraintSet))
  }

  def in(d: D.T, c: ConstraintSet) = {
    c match {
      case _: ConstraintSetAll => true
      case _: ConstraintSetNone => false
      case ConstraintSetList(_, l) =>
        
        (l.zip(l.tail).exists {
          case (MoreThan(a), LessThan(b)) => a.compareTo(d) < 0 && d.compareTo(b) < 0
          case _ => false
        }) ||
        (l.head match {
          case LessThan(b) => d.compareTo(b) < 0
          case _ => false
        }) ||
        (l.last match {
          case MoreThan(a) => a.compareTo(d) < 0
          case _ => false
        })
    }
    
  }

  property("intesection") = forAll { (c1: ConstraintSet, c2: ConstraintSet) =>
    val u = c1.intersection(c2)    
    forAll(FloatSpec.genRegularFloat) { (d: D.T) =>
      ((in(d, c1) && in(d, c2)) == in(d, u)) :| s"intersection = $u ${in(d, c1)} && ${in(d, c2)} != ${in(d, u)}"
    }
  }
  
  property("union") = forAll { (c1: ConstraintSet, c2: ConstraintSet) =>
    val u = c1.union(c2)    
    forAll(FloatSpec.genRegularFloat) { (d: D.T) =>
      ((in(d, c1) || in(d, c2)) == in(d, u)):| s"union = $u, ${in(d, c1)}  || ${in(d, c2)} != ${in(d, u)}"
    }
  }
}