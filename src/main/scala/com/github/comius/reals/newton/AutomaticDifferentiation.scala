/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.newton

import com.github.comius.RoundingContext
import com.github.comius.reals.Context
import com.github.comius.reals.Interval
import com.github.comius.reals.syntax.Add
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.syntax.CutR
import com.github.comius.reals.syntax.Div
import com.github.comius.reals.syntax.Integrate
import com.github.comius.reals.syntax.Mul
import com.github.comius.reals.syntax.Real
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.syntax.Var
import com.github.comius.reals.VarDomain
import com.github.comius.reals.Approximation
import com.github.comius.reals.ExistsDomain
import com.github.comius.reals.ForallDomain
import com.github.comius.reals.CutDomain
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.WholeDomain

object AutomaticDifferentiation {
  import com.github.comius.floats.Floats.{ impl => D }

  type A = (Interval, Interval)
  sealed trait IntervalRoundMode
  case object Down extends IntervalRoundMode
  case object Up extends IntervalRoundMode

  def liftr(op: (A, A, RoundingContext) => A)(x: Real, y: Real)(implicit ctx: Context[VarDomain], variables: Set[String],
                                                                roundMode: IntervalRoundMode): A = {
    val l1 = evalr(x)
    val l2 = evalr(y)
    op(l1, l2, ctx.roundingContext)
  }

  def cutdiff(f: Formula, z: String, zv: Interval)(implicit ctx: Context[VarDomain], variables: Set[String],
                                                   roundMode: IntervalRoundMode): Interval = f match {
    case Less(a, b) => {

      val l = Sub(a, b)
      val dfdz = evalr(l)(ctx + (z, CutDomain(zv.d, zv.u)), variables - z, roundMode)._2
      val dfdx = evalr(l)(ctx + (z, CutDomain(zv.d, zv.u)), variables, roundMode)._2
      val r = dfdz.divide(dfdx, ctx.roundingContext).negate
      // println(s"cutdiff> $f $zv $r $dfdz $dfdx ${ctx.vars}")
      // TODO workaround
      if (r == Interval(D.negInf, D.posInf)) Interval.ZERO else r
    }

  }

  def evalr(expr: Real)(implicit ctx: Context[VarDomain], variables: Set[String],
                        roundMode: IntervalRoundMode): (Interval, Interval) = {

    expr match {
      case Cut(z, a, b, l, u) =>
        val t1 = ApproximateNewton.estimate(l)(ctx, z, Interval(a, b))
        val t2 = ApproximateNewton.estimate(u)(ctx, z, Interval(a, b))
        val a3 = t1.lower.supremum()
        val b3 = t2.lower.infimum()        
        (Interval(a3, b3), cutdiff(l, z, Interval(a, b)))
      case CutR(z, l, _, _, _) => (Interval(D.negInf, D.posInf), cutdiff(l, z, Interval(D.negInf, D.posInf)))
      case Const(a)            => (Interval(a, a), Interval.ZERO)
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
        val xm = a.split(b)
        val xmi = Interval(xm, xm)
        val i8 = Interval(D.valueOf(8), D.valueOf(8))

        val rc = ctx.roundingContext

        val l1 = evalr(e)(ctx + (x, CutDomain(xm, xm)), variables, roundMode)
        val dl1 = evalr(e)(ctx + (x, CutDomain(a, b)), variables + x, roundMode)
        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)

        (ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc), l1._2.multiply(ba, rc))
      case Var(name) =>
        if (ctx.vars.get(name) == None) throw new Exception(s"Variable ${name} not found.")
        val Some(value) = ctx.vars.get(name)
        ((roundMode, value) match {
          case (Down, ExistsDomain(a, b)) =>
            val m = a.split(b); Interval(m, m)
          case (Down, ForallDomain(a, b)) => Interval(a, b)
          case (Down, CutDomain(a, b))    => Interval(a, b)
          case (Down, WholeDomain(a, b))  => Interval(a, b)
          case (Up, ExistsDomain(a, b))   => Interval(b, a)
          case (Up, ForallDomain(a, b)) =>
            val m = a.split(b); Interval(m, m)
          case (Up, CutDomain(a, b))   => Interval(b, a)
          case (Up, WholeDomain(a, b)) => Interval(b, a)
        }, if (variables.contains(name)) Interval.ONE else Interval.ZERO)
    }
  }

  /**
   * Lifts a binary operation on intervals to 'approximating two expressions and returning an approximation'.
   *
   * Type: (Interval -> Interval -> RoundingContext -> Interval) -> Expression -> Expression -> Context-> Approximation
   *
   * @param op the operation on Intervals
   * @param x first expression to be approximated
   * @param y second expression to be approximated
   * @param ctx context in which approximation is done
   * @return approx(x) op approx(y)
   */
  private def lift(op: (Interval, Interval, RoundingContext) => Interval)(
    x: Real, y: Real)(implicit ctx: Context[VarDomain]) =
    {
      val Approximation(l1, u1) = approximate(x)
      val Approximation(l2, u2) = approximate(y)
      Approximation(op(l1, l2, ctx.roundingContext), op(u1, u2, ctx.roundingContext.swap))
    }

  /**
   * Approximates an expression using intervals.
   *
   * @param expr the expression
   * @ctx context
   * @return approximation (lower and upper given with intervals)
   */
  def approximate(expr: Real)(implicit ctx: Context[VarDomain]): Approximation[Interval] = expr match {
    case Cut(_, a, b, _, _)  => Approximation(Interval(a, b), Interval(b, a))
    case CutR(_, _, _, _, _) => Approximation(Interval(D.negInf, D.posInf), Interval(D.posInf, D.negInf))
    case Integrate(x, a, b, e) =>
      val xm = a.split(b)
      val i8 = Interval(D.valueOf(8), D.valueOf(8))

      val lower = {
        val rc = ctx.roundingContext

        val l1 = evalr(e)(ctx + (x, CutDomain(xm, xm)), Set(), Down)
        val dl1 = evalr(e)(ctx + (x, CutDomain(a, b)), Set(x), Down)
        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)
        ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc)
      }
      val upper = {
        val rc = ctx.roundingContext.swap
        val l1 = evalr(e)(ctx + (x, CutDomain(xm, xm)), Set(), Up)
        val dl1 = evalr(e)(ctx + (x, CutDomain(a, b)), Set(x), Up)

        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)
        ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc)
      }

      Approximation(lower, upper)

    case Const(a)  => Approximation(Interval(a, a), Interval(a, a))
    case Add(x, y) => lift(_.add(_, _))(x, y)
    case Sub(x, y) => lift(_.subtract(_, _))(x, y)
    case Mul(x, y) => lift(_.multiply(_, _))(x, y)
    case Div(x, y) => lift(_.divide(_, _))(x, y)
    case Var(name) =>
      val Some(value) = ctx.vars.get(name)
      approximate(value)
  }

  /**
   * Approximates a variable on the given domain.
   *
   * @param domain the domain
   * @return approximation
   */
  def approximate(domain: VarDomain): Approximation[Interval] = domain match {
    case CutDomain(a, b)   => Approximation(Interval(a, b), Interval(b, a))
    case WholeDomain(a, b) => Approximation(Interval(a, b), Interval(b, a))
    case ExistsDomain(a, b) =>
      val m = a.split(b)
      Approximation(Interval(m, m), Interval(b, a))
    case ForallDomain(a, b) =>
      val m = a.split(b)
      Approximation(Interval(a, b), Interval(m, m))
  }
}
