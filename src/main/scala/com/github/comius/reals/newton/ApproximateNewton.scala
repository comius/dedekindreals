/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.newton

import com.github.comius.reals.Approximation
import com.github.comius.reals.Approximations
import com.github.comius.reals.Context
import com.github.comius.reals.CutDomain
import com.github.comius.reals.ExistsDomain
import com.github.comius.reals.ForallDomain
import com.github.comius.reals.Interval
import com.github.comius.reals.VarDomain
import com.github.comius.reals.newton.ConstraintSet.All
import com.github.comius.reals.newton.ConstraintSet.Constraint
import com.github.comius.reals.newton.ConstraintSet.LessThan
import com.github.comius.reals.newton.ConstraintSet.MoreThan
import com.github.comius.reals.newton.ConstraintSet.None
import com.github.comius.reals.syntax.And
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.newton.AutomaticDifferentiation.Down
import com.github.comius.reals.newton.AutomaticDifferentiation.Up

object ApproximateNewton extends Approximations {
  import com.github.comius.floats.Floats.{ impl => D }

  private def lift(op: (ConstraintSet, ConstraintSet) => ConstraintSet)(
    x: Formula, y: Formula)(implicit ctx: Context[VarDomain], x0: String, i: Interval): Approximation[ConstraintSet] =
    {
      val Approximation(l1, u1) = estimate(x)
      val Approximation(l2, u2) = estimate(y)
      Approximation(op(l1, l2), op(u1, u2))
    }

  def estimate(formula: Formula)(implicit ctx: Context[VarDomain], x0: String,
                                 i: Interval): Approximation[ConstraintSet] = formula match {
    case Less(x, y) =>

      val xm = i.d.split(i.u)
      // value at the middle point, we don't need interval
      val a @ (Interval(lf, _), _) = AutomaticDifferentiation.evalr(y - x)(ctx + (x0, CutDomain(xm,xm)), Set.empty, Down)
      // derivative over the whole interval
      val b @ (_, Interval(ld, ud)) = AutomaticDifferentiation.evalr(y - x)(ctx + (x0, CutDomain(i.d, i.u)), Set(x0), Down)

      val divU: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.up)
      val divD: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.down)

      def lowerRay(lf: D.T, d: D.T): Constraint = {
        (d.signum, lf.signum()) match {
          case (1, _)  => MoreThan(xm.subtract(divD(lf, d), ctx.roundingContext.up))
          case (-1, _) => LessThan(xm.subtract(divU(lf, d), ctx.roundingContext.down))
          case (_, 1)  => All
          case (_, _)  => None
        }
      }

      def upperRay(uf: D.T, d: D.T): Constraint = {
        (d.signum, uf.signum()) match {
          case (1, _)  => LessThan(xm.subtract(divU(uf, d), ctx.roundingContext.down))
          case (-1, _) => MoreThan(xm.subtract(divD(uf, d), ctx.roundingContext.up))
          case (_, 1)  => None
          case (_, _)  => All
        }
      }

      val lwr = ConstraintSet(Interval(i.d, i.u), xm, lowerRay(lf, ud), lowerRay(lf, ld))

      // value at the middle point, we don't need interval
      val (Interval(uf, _), _) = AutomaticDifferentiation.evalr(y - x)(ctx + (x0, CutDomain(xm, xm)), Set.empty, Up)
      // derivative over the whole interval
      val (_, Interval(uld, uud)) = AutomaticDifferentiation.evalr(y - x)(ctx + (x0, CutDomain(i.d, i.u)), Set(x0), Up)

      val upr = ConstraintSet(Interval(i.d, i.u), xm, upperRay(uf, uld), upperRay(uf, uud))
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
