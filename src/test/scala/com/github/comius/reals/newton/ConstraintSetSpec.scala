package com.github.comius.reals.newton

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import com.github.comius.floats.FloatSpec
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.Interval
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetAll
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetList
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetNone
import com.github.comius.reals.newton.ConstraintSet.LessThan
import com.github.comius.reals.newton.ConstraintSet.MoreThan

/**
 * Unit tests for Constraint set.
 *
 * Environment: no specific test environment needs to be set up. Java/Scala provide everything in default installation.
 */
@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class ConstraintSetSpec extends Properties("ConstraintSet") {
  val d = Interval(D.negInf, D.posInf)

  /**
   * Generator for ConstraintSetList type of constraint set.
   *
   * Note: it filters equal values of endpoints, so that invalid case of MoreThan(a) LessThan(a) doesn't occur.
   * TODO: Include valid case LessThan(a), MoreThan(a).
   */
  private def listConstraintSet: Gen[ConstraintSetList] =
    for {
      cl <- Gen.listOf(FloatSpec.genRegularFloat)
      if cl.size > 0
      c <- Gen.oneOf(0, 1)
    } yield ConstraintSetList(
      d,
      cl.sortWith(_.compareTo(_) <= 0).distinct.zipWithIndex.map({
        case (d, i) if i % 2 == c => MoreThan(d)
        case (d, _)               => LessThan(d)
      }))

  /**
   * Generator for ConstraintSet, that also includes ConstraintSetAll and ConstraintSetNone with lower frequency.
   */
  private implicit def arbConstraintSet: Arbitrary[ConstraintSet] = Arbitrary {
    Gen.frequency((1, ConstraintSetAll(d)), (1, ConstraintSetNone(d)), (5, listConstraintSet))
  }

  /**
   * Verifying a point is inside constraint set.
   * 
   * @param d the point
   * @param c the constraint set
   * @return true if d is inside c
   */
  private def in(d: D.T, c: ConstraintSet): Boolean = c match {
    case _: ConstraintSetAll  => true
    case _: ConstraintSetNone => false
    case ConstraintSetList(_, l) =>
      val headMatch = l.head match {
        case LessThan(b) => d.compareTo(b) < 0
        case _           => false
      }

      val tailMatch = l.last match {
        case MoreThan(a) => a.compareTo(d) < 0
        case _           => false
      }

      val middleMatch = l.zip(l.tail).exists {
        case (MoreThan(a), LessThan(b)) => a.compareTo(d) < 0 && d.compareTo(b) < 0
        case _                          => false
      }

      headMatch || tailMatch || middleMatch
  }

  /**
   * Tests intersection.
   */
  property("intesection") = forAll { (c1: ConstraintSet, c2: ConstraintSet) =>
    val u = c1.intersection(c2)
    forAll(FloatSpec.genRegularFloat) { (d: D.T) =>
      ((in(d, c1) && in(d, c2)) == in(d, u)) :| s"intersection = $u ${in(d, c1)} && ${in(d, c2)} != ${in(d, u)}"
    }
  }

  /**
   * Tests union.
   */
  property("union") = forAll { (c1: ConstraintSet, c2: ConstraintSet) =>
    val u = c1.union(c2)
    forAll(FloatSpec.genRegularFloat) { (d: D.T) =>
      ((in(d, c1) || in(d, c2)) == in(d, u)) :| s"union = $u, ${in(d, c1)}  || ${in(d, c2)} != ${in(d, u)}"
    }
  }
}