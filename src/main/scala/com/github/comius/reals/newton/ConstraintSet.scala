package com.github.comius.reals.newton

import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.Interval

abstract class ConstraintSet(val domain: Interval) {
  import ConstraintSet._

  def supremum(): D.T
  def infimum(): D.T

  private def moreThan(a: RealConstraint, b: RealConstraint): Boolean = {
    a.x.compareTo(b.x) match {
      case 0 =>
        (a, b) match {
          case (_: MoreThan, _: LessThan) => true
          case _                          => false
        }
      case c => c > 0
    }
  }

  def union(s: ConstraintSet): ConstraintSet = {
    require(domain == s.domain, "Union of diffrent domains")

    def union(l1: List[RealConstraint], l2: List[RealConstraint]): ConstraintSet = {
      val acc = List.newBuilder[RealConstraint]
      var p1 = l1
      var p2 = l2
      var in1 = l1.head.isInstanceOf[LessThan]
      var in2 = l2.head.isInstanceOf[LessThan]

      while (p1 != Nil || p2 != Nil) {
        if (p1 == Nil || (p2 != Nil && moreThan(p1.head, p2.head))) {
          val px = p1; p1 = p2; p2 = px;
          val inx = in1; in1 = in2; in2 = inx;
        }
        p1.head match {
          case a: LessThan =>
            if (in1 && !in2) acc += a
            in1 = false
          case a: MoreThan =>
            if (!(in1 || in2)) acc += a
            in1 = true
        }
        p1 = p1.tail
      }

      val result = acc.result
      if (result.isEmpty) ConstraintSetAll(domain) else ConstraintSetList(domain, result)
    }

    (this, s) match {
      case (_: ConstraintSetAll, _) | (_, _: ConstraintSetNone) => this
      case (_, _: ConstraintSetAll) | (_: ConstraintSetNone, _) => s
      case (ConstraintSetList(_, l1), (ConstraintSetList(_, l2))) => {
        union(l1, l2)
      }
    }
  }

  def intersection(s: ConstraintSet): ConstraintSet = {
    require(domain == s.domain, "Intersection of diffrent domains")

    def intersection(l1: List[RealConstraint], l2: List[RealConstraint]): ConstraintSet = {
      val acc = List.newBuilder[RealConstraint]
      var p1 = l1
      var p2 = l2
      var in1 = l1.head.isInstanceOf[LessThan]
      var in2 = l2.head.isInstanceOf[LessThan]

      while (p1 != Nil || p2 != Nil) {
        if (p1 == Nil || (p2 != Nil && moreThan(p1.head, p2.head))) {
          val px = p1; p1 = p2; p2 = px;
          val inx = in1; in1 = in2; in2 = inx;
        }
        p1.head match {
          case a: LessThan =>
            if (in1 && in2) acc += a
            in1 = false
          case a: MoreThan =>
            if (!in1 && in2) acc += a
            in1 = true
        }
        p1 = p1.tail
      }

      val result = acc.result
      if (result.isEmpty) ConstraintSetNone(domain) else ConstraintSetList(domain, result)

    }

    (this, s) match {
      case (_: ConstraintSetAll, _) | (_, _: ConstraintSetNone)   => s
      case (_, _: ConstraintSetAll) | (_: ConstraintSetNone, _)   => this
      case (ConstraintSetList(_, l1), (ConstraintSetList(_, l2))) => intersection(l1, l2)
    }
  }

  def complement() = {
    this match {
      case _: ConstraintSetAll  => ConstraintSetNone(domain)
      case _: ConstraintSetNone => ConstraintSetAll(domain)
      case ConstraintSetList(_, l1) =>
        val l = l1.sliding(2).flatMap(_ match {
          case List(LessThan(a), b) if a != b.x => List(MoreThan(a))
          case List(MoreThan(a), b) if a != b.x => List(LessThan(a))
          case _                                => List()
        }).toList
        if (l.isEmpty) 
          ConstraintSetAll(domain)
        else
          ConstraintSetList(domain, l)
    }
  }

  def toIntervals(): List[Interval] = {
    this match {
      case _: ConstraintSetAll  => List(domain)
      case _: ConstraintSetNone => List()
      case ConstraintSetList(_, l1) =>
        val endpts = if (l1.head.isInstanceOf[LessThan]) {
          (domain.d :: l1.map(_.x)) :+ domain.u
        } else {
          l1.map(_.x) :+ domain.u
        }
        endpts.sliding(2).map { case List(a, b) => Interval(a, b) }.toList
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

  case class ConstraintSetList(override val domain: Interval, constraints: List[RealConstraint]) extends ConstraintSet(domain) {
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