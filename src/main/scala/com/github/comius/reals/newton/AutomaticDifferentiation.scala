/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.newton

import com.github.comius.reals.Context
import com.github.comius.interval.Interval
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.syntax.Integrate
import com.github.comius.reals.syntax.Real
import com.github.comius.reals.syntax.Var
import com.github.comius.reals.VarDomain
import com.github.comius.reals.Approximation
import com.github.comius.reals.ExistsDomain
import com.github.comius.reals.ForallDomain
import com.github.comius.reals.CutDomain
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.WholeDomain
import com.github.comius.interval.BinaryFunctionEvaluator
import com.github.comius.reals.syntax.RealBinaryFunction
import com.github.comius.reals.syntax.RealFunction

object AutomaticDifferentiation {
  import com.github.comius.floats.Floats.{ impl => D }

  type A = (Interval, Interval)
  sealed trait IntervalRoundMode
  case object Down extends IntervalRoundMode
  case object Up extends IntervalRoundMode

  def cutdiff(f: Formula, z: String, zv: Interval)(implicit ctx: Context[VarDomain], variables: Set[String],
    roundMode: IntervalRoundMode): Interval = f match {
    case Less(a, b) => {

      val l = a - b
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
      case Const(a) => (Interval(a, a), Interval.ZERO)
      case RealBinaryFunction(x, y, e) =>
        val l1 = evalr(x)
        val l2 = evalr(y)
        e.evalp(l1, l2, ctx.roundingContext)
      case RealFunction(x, e) =>
        val l1 = evalr(x)
        e.evalp(l1, ctx.roundingContext)
      case Integrate(x, a, b, e) =>
        val xm = a.split(b)
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
          case (Down, CutDomain(a, b)) => Interval(a, b)
          case (Down, WholeDomain(a, b)) => Interval(a, b)
          case (Up, ExistsDomain(a, b)) => Interval(b, a)
          case (Up, ForallDomain(a, b)) =>
            val m = a.split(b); Interval(m, m)
          case (Up, CutDomain(a, b)) => Interval(b, a)
          case (Up, WholeDomain(a, b)) => Interval(b, a)
        }, if (variables.contains(name)) Interval.ONE else Interval.ZERO)
    }
  }

  /**
   * Approximates an expression using intervals.
   *
   * @param expr the expression
   * @ctx context
   * @return approximation (lower and upper given with intervals)
   */
  def approximate(expr: Real)(implicit ctx: Context[VarDomain]): Approximation[Interval] = expr match {
    case Cut(_, a, b, _, _) => Approximation(Interval(a, b), Interval(b, a))
    case Integrate(x, a, b, e) =>
      val xm = a.split(b)
      val i8 = Interval(D.valueOf(8), D.valueOf(8))

      val lower = {
        val rc = ctx.roundingContext

        val l1 = evalr(e)(ctx + (x, CutDomain(xm, xm)), Set.empty, Down)
        val dl1 = evalr(e)(ctx + (x, CutDomain(a, b)), Set(x), Down)
        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)
        ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc)
      }
      val upper = {
        val rc = ctx.roundingContext.swap
        val l1 = evalr(e)(ctx + (x, CutDomain(xm, xm)), Set.empty, Up)
        val dl1 = evalr(e)(ctx + (x, CutDomain(a, b)), Set(x), Up)

        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)
        ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc)
      }

      Approximation(lower, upper)

    case Const(a) => Approximation(Interval(a, a), Interval(a, a))
    case RealBinaryFunction(x, y, e) =>
      val Approximation(l1, u1) = approximate(x)
      val Approximation(l2, u2) = approximate(y)
      Approximation(e.eval(l1, l2, ctx.roundingContext), e.eval(u1, u2, ctx.roundingContext.swap))
    case RealFunction(x, e) =>
      val Approximation(l1, u1) = approximate(x)
      Approximation(e.eval(l1, ctx.roundingContext), e.eval(u1, ctx.roundingContext.swap))
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
    case CutDomain(a, b) => Approximation(Interval(a, b), Interval(b, a))
    case WholeDomain(a, b) => Approximation(Interval(a, b), Interval(b, a))
    case ExistsDomain(a, b) =>
      val m = a.split(b)
      Approximation(Interval(m, m), Interval(b, a))
    case ForallDomain(a, b) =>
      val m = a.split(b)
      Approximation(Interval(a, b), Interval(m, m))
  }
}
