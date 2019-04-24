package com.github.comius.reals.newton

import com.github.comius.reals.syntax.And
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.syntax.Sub
import com.github.comius.BaByMarshall.LessThan
import java.util.regex.Pattern.All
import com.github.comius.reals.newton.ConstraintSet.Constraint
import com.github.comius.reals.newton.ConstraintSet.MoreThan
import com.github.comius.reals.newton.ConstraintSet.LessThan
import com.github.comius.reals.newton.ConstraintSet.All
import com.github.comius.reals.newton.ConstraintSet.None
import com.github.comius.reals.Approximations
import com.github.comius.reals.Approximations
import com.github.comius.reals.Interval
import com.github.comius.reals.Context
import com.github.comius.reals.VarDomain
import com.github.comius.reals.ExistsDomain
import com.github.comius.reals.ForallDomain
import com.github.comius.reals.CutDomain
import com.github.comius.reals.Approximation

object NewtonApproximations extends Approximations {
  import com.github.comius.floats.Floats.{ impl => D }

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
            Interval(b, a)
          case ForallDomain(a, b) =>
            val m = a.split(b); Interval(m, m)
          case CutDomain(a, b) => Interval(a, b)
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

      def halfLowerR(lf: D.T, ld: D.T): Constraint = {
        (ld.signum, lf.signum()) match {
          case (1, _)  => MoreThan(xm.subtract(divD(lf, ld), ctx.roundingContext.up))
          case (-1, _) => LessThan(xm.subtract(divU(lf, ld), ctx.roundingContext.down))
          case (0, 1)  => All
          case (0, _)  => None
        }
      }
      def halfLowerL(lf: D.T, ud: D.T): Constraint = {
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