package com.github.comius.reals

import com.github.comius.reals.syntax.And
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.syntax.Sub

object NewtonApproximations extends Approximations {
  import com.github.comius.floats.Floats.{ impl => D }

  sealed trait Constraint {
    val x: D.T
  }
  case object All extends Constraint {
    val x = D.negInf
  }
  case object None extends Constraint {
    val x = D.posInf
  }
  case class MoreThan(val x: D.T) extends Constraint
  case class LessThan(val x: D.T) extends Constraint

  case class ConstraintSet private (domain: Interval, constraints: List[Constraint]) {
    require(constraints.size > 0, "empty constraint set")
    require(!constraints.contains(All) || constraints.size == 1, s"All with additional elements $constraints")
    require(!constraints.contains(None) || constraints.size == 1, s"None with additional elements $constraints")
    require(
      constraints.zip(constraints.tail).forall {
        case (LessThan(x), MoreThan(y)) => x.compareTo(y) <= 0
        case (MoreThan(x), LessThan(y)) => x.compareTo(y) <= 0
        case _                          => false
      },
      s"LessThan and MoreThan are not exchanging or not ordered: $constraints")

    def supremum(): D.T = {
      constraints.last match {
        case None         => domain.d
        case All          => domain.u
        case LessThan(x0) => x0
        case MoreThan(_)  => domain.u
      }
    }

    def infimum(): D.T = {
      constraints.head match {
        case None         => domain.u
        case All          => domain.d
        case LessThan(_)  => domain.d
        case MoreThan(x0) => x0
      }
    }

    def union(s: ConstraintSet): ConstraintSet = {
      require(domain == s.domain, "Union of diffrent domains")
      if (constraints == List(All) || s.constraints == List(None)) return this
      if (s.constraints == List(All) || constraints == List(None)) return s

      def union(l1: List[Constraint], l2: List[Constraint]): List[Constraint] = {
        (l1, l2) match {
          case (Nil, _)                                     => l2
          case (_, Nil)                                     => l1

          case (a :: at, b :: bt) if a.x.compareTo(b.x) > 0 => union(l2, l1)

          case (LessThan(x) :: at, LessThan(y) :: _)        => union(at, l2)
          case (MoreThan(x) :: _, MoreThan(y) :: bt)        => union(l1, bt)

          case (a :: at, _)                                 => a :: union(at, l2)
        }
      }
      val u = union(constraints, s.constraints)
      ConstraintSet(domain, u)
    }

    def intersection(s: ConstraintSet): ConstraintSet = {
      require(domain == s.domain, "Intersection of diffrent domains")
      if (s.constraints == List(All) || constraints == List(None)) return this
      if (constraints == List(All) || s.constraints == List(None)) return s

      def intersection(l1: List[Constraint], l2: List[Constraint]): List[Constraint] = {
        (l1, l2) match {
          case (Nil, _)                                     => l2
          case (_, Nil)                                     => l1

          case (a :: at, b :: bt) if a.x.compareTo(b.x) > 0 => intersection(l2, l1)

          case (LessThan(x) :: _, LessThan(y) :: bt)        => intersection(l1, bt)
          case (MoreThan(x) :: at, MoreThan(y) :: _)        => intersection(at, l2)

          case (a :: at, _)                                 => a :: intersection(at, l2)
        }
      }
      val u = intersection(constraints, s.constraints)
      ConstraintSet(domain, u)
    }
  }

  object ConstraintSet {

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

    def apply(domain: Interval, constraint: Constraint): ConstraintSet = {
      ConstraintSet(domain, List(sanitizeConstraint(domain, constraint)))
    }

    def apply(domain: Interval, xm: D.T, c1: Constraint, c2: Constraint): ConstraintSet = {
      val c1s = sanitizeConstraint(Interval(domain.d, xm), c1)
      val c2s = sanitizeConstraint(Interval(xm, domain.u), c2)

      val constraints = (c1s, c2s) match {
        case (MoreThan(_), LessThan(_)) => List(c1s, c2s)
        case (LessThan(_), MoreThan(_)) => List(c1s, c2s)

        case (All, All)                 => List(All)
        case (None, None)               => List(None)
        case (All, None)                => List(LessThan(xm))
        case (None, All)                => List(MoreThan(xm))

        case (MoreThan(_), All)         => List(c1s)
        case (LessThan(_), None)        => List(c1s)
        case (All, LessThan(_))         => List(c2s)
        case (None, MoreThan(_))        => List(c2s)

        case (MoreThan(_), None)        => List(c1s, LessThan(xm))
        case (LessThan(_), All)         => List(c1s, MoreThan(xm))
        case (None, LessThan(_))        => List(MoreThan(xm), c2s)
        case (All, MoreThan(_))         => List(LessThan(xm), c2s)

        case (LessThan(_), LessThan(_)) => List(c1s, MoreThan(xm), c2s)
        case (MoreThan(_), MoreThan(_)) => List(c1s, LessThan(xm), c2s)
      }
      ConstraintSet(domain, constraints)
    }

    def apply(domain: Interval, b: Boolean): ConstraintSet = {
      if (b) ConstraintSet(domain, List(MoreThan(domain.d))) else ConstraintSet(domain, List(LessThan(domain.d)))
    }
  }

  def lift(op: (ConstraintSet, ConstraintSet) => ConstraintSet)(x: Formula, y: Formula)(implicit ctx: Context[VarDomain], x0: Symbol, i: Interval) =
    {
      val Approximation(l1, u1) = estimate(x)
      val Approximation(l2, u2) = estimate(y)
      Approximation(op(l1, l2), op(u1, u2))
    }

  def estimate(formula: Formula)(implicit ctx: Context[VarDomain], x0: Symbol, i: Interval): Approximation[ConstraintSet] = formula match {
    case Less(x, y) =>
      def extendContextLower(ctx: Context[VarDomain]): Context[(Interval, Interval)] = {
        ctx.mapValues(v => (v match {
          case ExistsDomain(a, b) =>
            val m = a.split(b); Interval(m, m)
          case ForallDomain(a, b) => Interval(a, b)
          case CutDomain(a, b)    => Interval(a, b)
        }, Interval.ZERO))
      }
      
      def extendContextUpper(ctx: Context[VarDomain]): Context[(Interval, Interval)] = {
        ctx.mapValues(v => (v match {
          case ExistsDomain(a, b) =>
            Interval(b,a)
          case ForallDomain(a, b) => val m = a.split(b); Interval(m,m)
          case CutDomain(a, b)    => Interval(a, b)
        }, Interval.ZERO))
      }

      val xm = i.d.split(i.u)
      val xi = Interval(xm, xm)
      // value at the middle point, we don't need interval
      val a @ (Interval(lf, _), _) = AutomaticDifferentiation.evalr(Sub(y, x))(extendContextLower(ctx) + (x0 -> (xi, Interval.ZERO)))
      // derivative over the whole interval
      val b @ (_, Interval(ld, ud)) = AutomaticDifferentiation.evalr(Sub(y, x))(extendContextLower(ctx) + (x0 -> (i, Interval.ONE)))

      val divU: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.up)
      val divD: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.down)

      def halfLowerR(lf: D.T, ld: D.T) = {
        (ld.signum, lf.signum()) match {
          case (1, _)  => MoreThan(xm.subtract(divD(lf, ld), ctx.roundingContext.up))
          case (-1, _) => LessThan(xm.subtract(divU(lf, ld), ctx.roundingContext.down))
          case (0, 1)  => All
          case (0, _)  => None
        }
      }
      def halfLowerL(lf: D.T, ud: D.T) = {
        (ud.signum, lf.signum()) match {
          case (1, _)  => MoreThan(xm.subtract(divU(lf, ud), ctx.roundingContext.up))
          case (-1, _) => LessThan(xm.subtract(divU(lf, ud), ctx.roundingContext.down))
          case (0, 1)  => All
          case (0, _)  => None
        }
      }

      val lwr = ConstraintSet(Interval(i.d, i.u), xm, halfLowerL(lf, ud), halfLowerR(lf, ld))
      
       // value at the middle point, we don't need interval
      val (Interval(uf, _), _) = AutomaticDifferentiation.evalr(Sub(y, x))(extendContextUpper(ctx) + (x0 -> (xi, Interval.ZERO)))
      // derivative over the whole interval
      val (_, Interval(uld, uud)) = AutomaticDifferentiation.evalr(Sub(y, x))(extendContextUpper(ctx) + (x0 -> (i, Interval.ONE)))
      
      val upr = ConstraintSet(Interval(i.d, i.u), xm, halfLowerL(uf, uld), halfLowerR(uf, uud))
      Approximation(lwr, upr)

    case Exists(x, a, b, phi) =>
      estimate(phi)(ctx + (x -> ExistsDomain(a, b)), x0, i)

    case Forall(x, a, b, phi) =>
      estimate(phi)(ctx + (x -> ForallDomain(a, b)), x0, i)

    case And(x, y)                => lift(_.intersection(_))(x, y)

    case Or(x, y)                 => lift(_.union(_))(x, y)

    case ConstFormula(a: Boolean) => Approximation(ConstraintSet(i, a), ConstraintSet(i, a))
  }
}