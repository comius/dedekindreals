package com.github.comius.reals.newton

import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.Interval
import scala.collection.mutable.ListBuffer

abstract class ConstraintSet(val domain: Interval) {
  import ConstraintSet._

  def supremum(): D.T
  def infimum(): D.T

  private implicit def ord = new Ordering[(RealConstraint, Boolean)] {
    def compare(a: (RealConstraint, Boolean), b: (RealConstraint, Boolean)): Int = {
      val c = a._1.x.compareTo(b._1.x)
      if (c == 0)
        (if (a._1.isInstanceOf[LessThan]) 0 else 1) - (if (b._1.isInstanceOf[LessThan]) 0 else 1)
      else c
    }
  }

  def union(s: ConstraintSet): ConstraintSet = {
    require(domain == s.domain, "Union of diffrent domains")

    def union(l1: List[RealConstraint], l2: List[RealConstraint]): ConstraintSet = {
      val u0 = (l1.map((_, true)) ++ l2.map((_, false))).sorted

      val r = ListBuffer[RealConstraint]()
      var in1 = l1.head.isInstanceOf[LessThan]
      var in2 = l2.head.isInstanceOf[LessThan]

      for (t <- u0) t match {
        case (a: MoreThan, b) =>
          if (!(in1 || in2)) r.append(a)
          if (b) in1 = true else in2 = true
        case (a: LessThan, b) =>
          val prev = in1 || in2
          if (b) in1 = false else in2 = false
          if (prev && (!(in1 || in2))) r.append(a)
      }

      if (r.size == 0) {
        ConstraintSetAll(domain)
      } else {
        ConstraintSetList(domain, r.toList)
      }
    }

    (this, s) match {
      case (_: ConstraintSetAll, _) | (_, _: ConstraintSetNone)   => this
      case (_, _: ConstraintSetAll) | (_: ConstraintSetNone, _)   => s
      case (ConstraintSetList(_, l1), (ConstraintSetList(_, l2))) => union(l1, l2)
    }
  }

  def intersection(s: ConstraintSet): ConstraintSet = {
    require(domain == s.domain, "Intersection of diffrent domains")

    def intersection(l1: List[RealConstraint], l2: List[RealConstraint]): ConstraintSet = {
      val u0 = (l1.map((_, true)) ++ l2.map((_, false))).sorted

      val r = ListBuffer[RealConstraint]()
      var in1 = l1.head.isInstanceOf[LessThan]
      var in2 = l2.head.isInstanceOf[LessThan]

      for (t <- u0) t match {
        case (a: MoreThan, b) =>
          val prev = in1 && in2
          if (b) in1 = true else in2 = true
          if (!prev && in1 && in2) r.append(a)
        case (a: LessThan, b) =>
          if (in1 && in2) r.append(a)
          if (b) in1 = false else in2 = false
      }

      if (r.size == 0) {
        ConstraintSetNone(domain)
      } else {
        ConstraintSetList(domain, r.toList)
      }
    }

    (this, s) match {
      case (_: ConstraintSetAll, _) | (_, _: ConstraintSetNone)   => s
      case (_, _: ConstraintSetAll) | (_: ConstraintSetNone, _)   => this
      case (ConstraintSetList(_, l1), (ConstraintSetList(_, l2))) => intersection(l1, l2)
    }
  }

}

object ConstraintSet {
  sealed trait RealConstraint {
    val x: D.T
  }
  sealed trait Constraint
  case object All extends Constraint
  case object None extends Constraint
  case class MoreThan(val x: D.T) extends Constraint with RealConstraint
  case class LessThan(val x: D.T) extends Constraint with RealConstraint

  // Check if constraint lies in the domain
  private def sanitizeConstraint(domain: Interval, constraint: Constraint): Constraint = constraint match {
    case LessThan(x) =>
      if (x.compareTo(domain.d) <= 0) None
      else if (domain.u.compareTo(x) < 0) All
      else constraint
    case MoreThan(x) =>
      if (x.compareTo(domain.d) < 0) All
      else if (domain.u.compareTo(x) <= 0) None
      else constraint
    case All | None => constraint
  }

  /* def apply(domain: Interval, constraint: Constraint): ConstraintSet = {
      ConstraintSet(domain, List(sanitizeConstraint(domain, constraint)))
    }*/

  def apply(domain: Interval, xm: D.T, c1: Constraint, c2: Constraint): ConstraintSet = {
    val c1s = sanitizeConstraint(Interval(domain.d, xm), c1)
    val c2s = sanitizeConstraint(Interval(xm, domain.u), c2)

    (c1s, c2s) match {
      case (c1s: MoreThan, c2s: LessThan) => ConstraintSetList(domain, List(c1s, c2s))
      case (c1s: LessThan, c2s: MoreThan) => ConstraintSetList(domain, List(c1s, c2s))

      case (All, All)                     => ConstraintSetAll(domain)
      case (None, None)                   => ConstraintSetNone(domain)
      case (All, None)                    => ConstraintSetList(domain, List(LessThan(xm)))
      case (None, All)                    => ConstraintSetList(domain, List(MoreThan(xm)))

      case (c1s: MoreThan, All)           => ConstraintSetList(domain, List(c1s))
      case (c1s: LessThan, None)          => ConstraintSetList(domain, List(c1s))
      case (All, c2s: LessThan)           => ConstraintSetList(domain, List(c2s))
      case (None, c2s: MoreThan)          => ConstraintSetList(domain, List(c2s))

      case (c1s: MoreThan, None)          => ConstraintSetList(domain, List(c1s, LessThan(xm)))
      case (c1s: LessThan, All)           => ConstraintSetList(domain, List(c1s, MoreThan(xm)))
      case (None, c2s: LessThan)          => ConstraintSetList(domain, List(MoreThan(xm), c2s))
      case (All, c2s: MoreThan)           => ConstraintSetList(domain, List(LessThan(xm), c2s))

      case (c1s: LessThan, c2s: LessThan) => ConstraintSetList(domain, List(c1s, MoreThan(xm), c2s))
      case (c1s: MoreThan, c2s: MoreThan) => ConstraintSetList(domain, List(c1s, LessThan(xm), c2s))
    }
  }

  def apply(domain: Interval, b: Boolean): ConstraintSet = {
    if (b) ConstraintSetList(domain, List(MoreThan(domain.d))) else ConstraintSetList(domain, List(LessThan(domain.d)))
  }

  case class ConstraintSetAll(override val domain: Interval) extends ConstraintSet(domain) {
    override def supremum(): D.T = domain.u
    override def infimum(): D.T = domain.d
  }
  case class ConstraintSetNone(override val domain: Interval) extends ConstraintSet(domain) {
    override def supremum(): D.T = domain.d
    override def infimum(): D.T = domain.u
  }

  case class ConstraintSetList private (override val domain: Interval, constraints: List[RealConstraint]) extends ConstraintSet(domain) {
    require(constraints.size > 0, "empty constraint set")
    require(
      constraints.zip(constraints.tail).forall {
        case (LessThan(x), MoreThan(y)) => x.compareTo(y) <= 0
        case (MoreThan(x), LessThan(y)) => x.compareTo(y) < 0
        case _                          => false
      },
      s"LessThan and MoreThan are not exchanging or not ordered: $constraints")

    override def supremum(): D.T = constraints.last match {
      case LessThan(x0) => x0
      case MoreThan(_)  => domain.u
    }

    override def infimum(): D.T = constraints.head match {
      case LessThan(_)  => domain.d
      case MoreThan(x0) => x0
    }
  }

}