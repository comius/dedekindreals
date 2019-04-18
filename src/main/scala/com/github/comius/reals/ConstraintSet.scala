package com.github.comius.reals

import com.github.comius.floats.Floats.{ impl => D }

abstract class ConstraintSet(val domain: Interval) {
  import ConstraintSet._

  def supremum(): D.T
  def infimum(): D.T

  def union(s: ConstraintSet): ConstraintSet = {
    require(domain == s.domain, "Union of diffrent domains")

    def union(l1: List[RealConstraint], l2: List[RealConstraint]): List[RealConstraint] = {
      (l1, l2) match {
        case (Nil, _)                                     => l2
        case (_, Nil)                                     => l1

        case (a :: at, b :: bt) if a.x.compareTo(b.x) > 0 => union(l2, l1)

        case (LessThan(x) :: at, LessThan(y) :: _)        => union(at, l2)
        case (MoreThan(x) :: _, MoreThan(y) :: bt)        => union(l1, bt)

        case (a :: at, _)                                 => a :: union(at, l2)
      }
    }

    (this, s) match {
      case (_: ConstraintSetAll, _) | (_, _: ConstraintSetNone)   => this
      case (_, _: ConstraintSetAll) | (_: ConstraintSetNone, _)   => s
      case (ConstraintSetList(_, l1), (ConstraintSetList(_, l2))) => ConstraintSetList(domain, union(l1, l2))
    }
  }

  def intersection(s: ConstraintSet): ConstraintSet = {
    require(domain == s.domain, "Intersection of diffrent domains")

    def intersection(l1: List[RealConstraint], l2: List[RealConstraint]): List[RealConstraint] = {
      (l1, l2) match {
        case (Nil, _)                                     => l2
        case (_, Nil)                                     => l1

        case (a :: at, b :: bt) if a.x.compareTo(b.x) > 0 => intersection(l2, l1)

        case (LessThan(x) :: _, LessThan(y) :: bt)        => intersection(l1, bt)
        case (MoreThan(x) :: at, MoreThan(y) :: _)        => intersection(at, l2)

        case (a :: at, _)                                 => a :: intersection(at, l2)
      }
    }

    (this, s) match {
      case (_: ConstraintSetAll, _) | (_, _: ConstraintSetNone)   => s
      case (_, _: ConstraintSetAll) | (_: ConstraintSetNone, _)   => this
      case (ConstraintSetList(_, l1), (ConstraintSetList(_, l2))) => ConstraintSetList(domain, intersection(l1, l2))
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
        case (MoreThan(x), LessThan(y)) => x.compareTo(y) <= 0
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