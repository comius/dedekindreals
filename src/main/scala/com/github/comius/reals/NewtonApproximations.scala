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
import com.github.comius.reals.Approximations.VarDomain
import com.github.comius.reals.Approximations.ExistsDomain
import com.github.comius.reals.Approximations.ForallDomain
import com.github.comius.reals.Approximations.CutDomain

object NewtonApproximations {
  import com.github.comius.floats.Floats.{ impl => D }
  import Approximations.Approximation

  val zeroInt = Interval(D.ZERO, D.ZERO)
  val oneInt = Interval(D.ONE, D.ONE)

  sealed class Constraint(val x: D.T)
  case class MoreThan(override val x: D.T) extends Constraint(x)
  case class LessThan(override val x: D.T) extends Constraint(x)

  case class ConstraintSet private (domain: Interval, constraints: List[Constraint]) {

    def supremum(): D.T = {
      constraints.last match {
        case LessThan(x0) => x0
        case MoreThan(_)  => domain.u
      }
    }

    def infimum(): D.T = {
      constraints.head match {
        case LessThan(_)  => domain.d
        case MoreThan(x0) => x0
      }
    }

    def union(s: ConstraintSet): ConstraintSet = {
      
      if (domain == s.domain) {
        var h1 = constraints
        var h2 = s.constraints
        var r = List[Constraint]()
        while (!h1.isEmpty && !h2.isEmpty) {
          if (h1.head.x.compareTo(h2.head.x) > 0) { val h = h1; h1 = h2; h2 = h }
          (h1.head, h2.head) match {
            case (LessThan(_), LessThan(_)) => h1 = h1.tail
            case (LessThan(_), MoreThan(_)) => r = r :+ h1.head; h1 = h1.tail
            case (MoreThan(_), MoreThan(_)) => h2 = h2.tail
            case (MoreThan(_), LessThan(_)) => r = r :+ h1.head; h1 = h1.tail
          }
        }
        r = r ++ h1 ++ h2
        println(s"$constraints union ${s.constraints} = $r ")
        ConstraintSet(domain, r)
      } else {
        throw new Exception()
      }
    }

    def intersection(s: ConstraintSet): ConstraintSet = {
      println("hease")
      this
    }
  }

  object ConstraintSet {

    // Check if constraint lies in the domain
    private def sanitizeConstraint(domain: Interval, constraint: Constraint): Constraint = constraint match {
      case LessThan(x) =>
        if (x.compareTo(domain.d) <= 0) LessThan(domain.d)
        else if (domain.u.compareTo(x) < 0) LessThan(domain.u)
        else constraint
      case MoreThan(x) =>
        if (x.compareTo(domain.d) < 0) MoreThan(domain.d)
        else if (domain.u.compareTo(x) <= 0) MoreThan(domain.u)
        else constraint
    }

    def apply(domain: Interval, constraint: Constraint): ConstraintSet = {
      ConstraintSet(domain, List(sanitizeConstraint(domain, constraint)))
    }

    def apply(domain: Interval, xm: D.T, c1: Constraint, c2: Constraint): ConstraintSet = {
      val c1s = sanitizeConstraint(Interval(domain.d, xm), c1)
      val c2s = sanitizeConstraint(Interval(xm, domain.u), c2)
      val constraints = (c1s, c2s) match {
        case (MoreThan(`xm`), LessThan(`xm`)) => List(LessThan(domain.d))
        case (MoreThan(_), LessThan(_)) => List(c1s, c2s)
        case (LessThan(_), MoreThan(_)) => List(c1s, c2s)

        case (MoreThan(_), MoreThan(`xm`)) => List(c1s)
        case (LessThan(`xm`), LessThan(_)) => List(c2s)
        case (MoreThan(`xm`), MoreThan(_)) => List(c2s)
        case (LessThan(_), LessThan(`xm`)) => List(c1s)
        
        // following cases shouldn't happen        
        case (MoreThan(_), MoreThan(_)) => List(c1s, LessThan(xm), c2s)
        case (LessThan(_), LessThan(_)) => List(c1s, MoreThan(xm), c2s)
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
      def extendContext(ctx: Context[VarDomain]): Context[(Interval, Interval)] = {
        ctx.mapValues(v => (v match {
          case ExistsDomain(a, b) =>
            val m = a.split(b); Interval(m, m)
          case ForallDomain(a, b) => Interval(a, b)
          case CutDomain(a, b)    => Interval(a, b)
        }, zeroInt))
      }

      val xm = i.d.split(i.u)
      val xi = Interval(xm, xm)
      // value at the middle point, we don't need interval
      val a @ (Interval(lf, uf), _) = evalr(Sub(y, x))(extendContext(ctx) + (x0 -> (xi, zeroInt)))
      // derivative over the whole interval
      val b @ (_, Interval(ld, ud)) = evalr(Sub(y, x))(extendContext(ctx) + (x0 -> (i, oneInt)))

      val divU: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.up)
      val divD: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.down)

      def halfLowerR(lf: D.T, ld: D.T) = {
        (ld.signum, lf.signum()) match {
          case (1, _) =>
            val r = xm.subtract(divD(lf, ld), ctx.roundingContext.up)
            MoreThan(r)
          case (-1, _) =>
            val r = xm.subtract(divU(lf, ld), ctx.roundingContext.down)
            LessThan(r)
          case (0, 1) =>
            MoreThan(xm)
          case (0, _) =>
            LessThan(xm)
        }
      }
      def halfLowerL(lf: D.T, ud: D.T) = {
        (ud.signum, lf.signum()) match {
          case (1, _) =>
            val r = xm.subtract(divU(lf, ud), ctx.roundingContext.down)
            MoreThan(r)
          case (-1, _) =>
            val r = xm.subtract(divU(lf, ud), ctx.roundingContext.down)
            LessThan(r)
          case (0, 1) =>
            LessThan(xm)
          case (0, _) =>
            MoreThan(xm)
        }
      }
      val lwr =
        ConstraintSet(Interval(i.d, i.u), xm, halfLowerL(lf, ud), halfLowerR(lf, ld))

      Approximation(lwr, ConstraintSet(i, false))

    case Exists(x, a, b, phi) =>
      estimate(phi)(ctx + (x -> ExistsDomain(a, b)), x0, i)

    case Forall(x, a, b, phi) =>
      estimate(phi)(ctx + (x -> ForallDomain(a, b)), x0, i)

    case And(x, y)                => lift(_.union(_))(x, y)

    case Or(x, y)                 => lift(_.intersection(_))(x, y)

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