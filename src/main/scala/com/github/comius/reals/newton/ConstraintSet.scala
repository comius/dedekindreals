package com.github.comius.reals.newton

import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.Interval

/**
 * A Constrains set on the given domain.
 *
 * The set contains list of 'more than' and 'less than' constraints. The constraints need to be exchanging and ordered
 * by value.
 *
 * Special cases of constraint sets are when whole domain is satisfied or none of it.
 *
 * @param domain the domain
 */
abstract sealed class ConstraintSet(val domain: Interval) {
  import ConstraintSet._

  /**
   * @return the supremum
   */
  def supremum(): D.T

  /**
   * @return the infimum
   */
  def infimum(): D.T

  /**
   * Comparint two constraints. Smaller values come before bigger. When values are equal LessThan is before MoreThan.
   *
   * @param a first constraint to compare
   * @param b second constraint to compare
   * @return true if a > b
   */
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

  /**
   * Computes union of two constraint sets on the same domain.
   *
   * @param s the constraint set to compute union with
   * @return the union
   */
  def union(s: ConstraintSet): ConstraintSet = {
    require(domain == s.domain, "Union of diffrent domains")

    /*
     * Computes union of two lists of constraints.
     *
     * Implementation is based on scanline algorithm. Imperative style works better than functional. In functional
     * style, because scala is an eager language, we cannot produce tail recursion without having an additional
     * parameter that acts as list accumulator.
     */
    def union(l1: List[RealConstraint], l2: List[RealConstraint]): ConstraintSet = {
      val acc = List.newBuilder[RealConstraint] // accumulator for the result
      var p1 = l1 // pointing to head of first list
      var p2 = l2
      var in1 = l1.head.isInstanceOf[LessThan] // is scanline over the first constraint set inside it
      var in2 = l2.head.isInstanceOf[LessThan]

      while (p1 != Nil || p2 != Nil) {
        // scanline should be over the first list, so swap them if necessary
        if (p1 == Nil || (p2 != Nil && moreThan(p1.head, p2.head))) {
          val px = p1; p1 = p2; p2 = px;
          val inx = in1; in1 = in2; in2 = inx;
        }
        // process one point of the scanline
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

    // check edge cases
    (this, s) match {
      case (_: ConstraintSetAll, _) | (_, _: ConstraintSetNone)   => this
      case (_, _: ConstraintSetAll) | (_: ConstraintSetNone, _)   => s
      case (ConstraintSetList(_, l1), (ConstraintSetList(_, l2))) => union(l1, l2)
    }
  }

  /**
   * Computes intersection of two constraint sets on the same domain.
   *
   * @param s the constraint set to compute intersection with
   * @return the intersection
   */
  def intersection(s: ConstraintSet): ConstraintSet = {
    require(domain == s.domain, "Intersection of diffrent domains")

    // See comments for the union case. This only differs how new in1 and in2 are computed.
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

  /**
   * Computes approximate complement within the given domain.
   *
   * It is only approximate, because endpoints of constraints are not complemented (they are not part of original
   * neither the complement).
   *
   * @return the complement
   */
  def complement(): ConstraintSet = {
    this match {
      case _: ConstraintSetAll  => ConstraintSetNone(domain)
      case _: ConstraintSetNone => ConstraintSetAll(domain)
      case ConstraintSetList(_, l1) =>
        val l = l1.sliding(2).flatMap {
          case List(LessThan(a), b) if a != b.x => List(MoreThan(a))
          case List(MoreThan(a), b) if a != b.x => List(LessThan(a))
          case _                                => List()
        }.toList
        if (l.isEmpty)
          ConstraintSetAll(domain)
        else
          ConstraintSetList(domain, l)
    }
  }

  /**
   * Closes endpoints and converts this constraint set to list of intervals.
   *
   * @return list of intervals
   */
  def toIntervals(): List[Interval] = this match {
    case _: ConstraintSetAll  => List(domain)
    case _: ConstraintSetNone => List()
    case ConstraintSetList(_, l1) =>
      // TODO optimisation
      val endpts = if (l1.head.isInstanceOf[LessThan]) {
        (domain.d :: l1.map(_.x)) :+ domain.u
      } else {
        l1.map(_.x) :+ domain.u
      }
      endpts.sliding(2).map { case List(a, b) => Interval(a, b) }.toList
  }

}

/**
 * Companion object to ConstraintSet.
 */
object ConstraintSet {

  /**
   * Constraint with an enpoint.
   */
  abstract sealed trait RealConstraint {
    val x: D.T
  }

  /** A constraint. */
  abstract sealed trait Constraint

  /** Everything within the domain. */
  case object All extends Constraint

  /** Nothing within the domain. */
  case object None extends Constraint

  /**
   * More than x.
   * @param x the endpoint
   */
  case class MoreThan(val x: D.T) extends Constraint with RealConstraint

  /**
   * Less than x.
   * @param x the endpoint
   */
  case class LessThan(val x: D.T) extends Constraint with RealConstraint

  // Check if constraint lies within the domain. If not sanitizes it.
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

  /**
   * Constructs union of two constraint sets that are next to each other - one on [d,xm], second on [xm,u].
   * Each containing a simple Constraint.
   *
   * @param domain the combined domain
   * @param xm the split point
   * @param c1 simple constraint valid on [d,xm]
   * @param c2 simple constraint vaild on [xm,u]
   * @return the combined constraint set
   */
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

  /**
   * Constructs a simple constraint set either containing all point or none (depending on b).
   *
   * @param domain the domain
   * @parma b b
   */
  def apply(domain: Interval, b: Boolean): ConstraintSet = {
    if (b) ConstraintSetAll(domain) else ConstraintSetNone(domain)
  }

  /**
   * Complete Constraint Set.
   *
   * @param the domain
   */
  case class ConstraintSetAll(override val domain: Interval) extends ConstraintSet(domain) {
    override def supremum(): D.T = domain.u
    override def infimum(): D.T = domain.d
  }

  /**
   * Empty Constraint Set.
   *
   * @param the domain
   */
  case class ConstraintSetNone(override val domain: Interval) extends ConstraintSet(domain) {
    override def supremum(): D.T = domain.d
    override def infimum(): D.T = domain.u
  }

  /**
   * Constraint set containing list of constraints.
   *
   * @param domain the domain
   * @param constraints the list of constraints
   */
  case class ConstraintSetList(override val domain: Interval, constraints: List[RealConstraint])
    extends ConstraintSet(domain) {

    // Verifies the list is not empty
    require(constraints.size > 0, "empty constraint set")

    // Verifies the constraints are ordered and contain exchanging LessThan, MoreThan
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