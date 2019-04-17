package com.github.comius.reals

import com.github.comius.RoundingContext
import com.github.comius.reals.syntax.Var
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.syntax.Real
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.syntax.Mul
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Integrate
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Div
import com.github.comius.reals.syntax.CutR
import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.And
import com.github.comius.reals.syntax.Add
import com.github.comius.reals.BisectionApproximations.VarDomain
import com.github.comius.reals.BisectionApproximations.ExistsDomain
import com.github.comius.reals.BisectionApproximations.ForallDomain
import com.github.comius.reals.BisectionApproximations.CutDomain

object NewtonApproximations {
  import com.github.comius.floats.Floats.{ impl => D }
  import BisectionApproximations.Approximation

  val zeroInt = Interval(D.ZERO, D.ZERO)
  val oneInt = Interval(D.ONE, D.ONE)

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
        }, zeroInt))
      }
      
      def extendContextUpper(ctx: Context[VarDomain]): Context[(Interval, Interval)] = {
        ctx.mapValues(v => (v match {
          case ExistsDomain(a, b) =>
            Interval(b,a)
          case ForallDomain(a, b) => val m = a.split(b); Interval(m,m)
          case CutDomain(a, b)    => Interval(a, b)
        }, zeroInt))
      }

      val xm = i.d.split(i.u)
      val xi = Interval(xm, xm)
      // value at the middle point, we don't need interval
      val a @ (Interval(lf, _), _) = evalr(Sub(y, x))(extendContextLower(ctx) + (x0 -> (xi, zeroInt)))
      // derivative over the whole interval
      val b @ (_, Interval(ld, ud)) = evalr(Sub(y, x))(extendContextLower(ctx) + (x0 -> (i, oneInt)))

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
      val (Interval(uf, _), _) = evalr(Sub(y, x))(extendContextUpper(ctx) + (x0 -> (xi, zeroInt)))
      // derivative over the whole interval
      val (_, Interval(uld, uud)) = evalr(Sub(y, x))(extendContextUpper(ctx) + (x0 -> (i, oneInt)))
      
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

  type A = (Interval, Interval)

  def liftr(op: (A, A, RoundingContext) => A)(x: Real, y: Real)(implicit ctx: Context[(Interval, Interval)]): A = {
    val l1 = evalr(x)
    val l2 = evalr(y)
    op(l1, l2, ctx.roundingContext)
  }

  def evalr(expr: Real)(implicit ctx: Context[(Interval, Interval)]): (Interval, Interval) = {

    expr match {
      case Cut(_, a, b, _, _)  => (Interval(a, b), zeroInt)
      case CutR(_, _, _, _, _) => (Interval(D.negInf, D.posInf), zeroInt)
      case Const(a)            => (Interval(a, a), zeroInt)
      case Add(x, y) =>
        liftr((a, b, r) => (a._1.add(b._1, r), a._2.add(b._2, r)))(x, y)
      case Sub(x, y) =>
        liftr((a, b, r) => (a._1.subtract(b._1, r), a._2.subtract(b._2, r)))(x, y)
      case Mul(x, y) =>
        liftr((a, b, r) => (a._1.multiply(b._1, r), a._1.multiply(b._2, r).add(b._1.multiply(a._2, r), r)))(x, y)
      case Div(x, y) =>
        liftr((a, b, r) => (a._1.divide(b._1, r), a._2.multiply(b._1, r).subtract(b._2.multiply(a._1, r.swap), r.swap)
          .divide(b._1.multiply(b._1, r), r.swap())))(x, y) // TODO rounding
      case Integrate(x, a, b, e) =>
        val rc = ctx.roundingContext
        val xm = a.split(b)
        val xmi = Interval(xm, xm)
        val l1 = evalr(e)(ctx + (x -> (xmi, zeroInt)))
        val dl1 = evalr(e)(ctx + (x -> (Interval(a, b), oneInt)))
        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)
        val i8 = Interval(D.valueOf(8), D.valueOf(8))
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)

        (ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc), l1._2.multiply(ba, rc))
      /*
        val l1 = evalr(e)(ctx + (x -> (Interval(a, b), zeroInt)))
        val ba = Interval(b, b).subtract(Interval(a, a), ctx.roundingContext)
        (l1._1.multiply(ba, ctx.roundingContext), l1._2.multiply(ba, ctx.roundingContext))
        */
      case Var(name) => ctx.vars.get(name).get
    }
  }
}