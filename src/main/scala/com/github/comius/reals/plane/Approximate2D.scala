/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.plane

import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.newton.AutomaticDifferentiation
import com.github.comius.reals.Approximations
import com.github.comius.reals.Approximations
import com.github.comius.reals.Approximation
import com.github.comius.reals.Interval
import com.github.comius.reals.Context
import com.github.comius.reals.VarDomain
import com.github.comius.reals.ExistsDomain
import com.github.comius.reals.ForallDomain
import com.github.comius.reals.CutDomain

object Approximate2D extends Approximations {
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

  // given f and search space, returns new lower and upper approximation and new search space (one iteration)
  def estimate(lss: Less, x0: (Symbol, Interval),
               y0: (Symbol, Interval))(implicit ctx: Context[VarDomain]): Approximation[ConstraintSet2D] = {

    val (xs, xi) = x0
    val (ys, yi) = y0

    val Less(x, y) = lss
    val f = Sub(y, x)
    val mx = xi.d.split(yi.u)
    val my = yi.d.split(yi.u)
    val lctx = extendContextLower(ctx)
    val uctx = extendContextUpper(ctx)

    val lfmxmy = AutomaticDifferentiation.evalr(f)(
      (lctx + (xs, (Interval(mx, mx), Interval.ZERO))) + (ys, (Interval(my, my), Interval.ZERO)))._1
    val ldfxi = AutomaticDifferentiation.
      evalr(f)(lctx + (xs, (xi, Interval.ONE)) + (ys, (yi, Interval.ZERO)))._2
    val ldfyi = AutomaticDifferentiation.
      evalr(f)(lctx + (xs, (xi, Interval.ZERO)) + (ys, (yi, Interval.ONE)))._2

    val llines =
      for (ldx <- List(ldfxi.d, ldfxi.u); ldy <- List(ldfyi.d, ldfyi.u))
        yield Line(lfmxmy.d, mx, my, ldx, ldy)
    val ufmxmy = AutomaticDifferentiation.evalr(f)(
      (uctx + (xs, (Interval(mx, mx), Interval.ZERO))) + (ys, (Interval(my, my), Interval.ZERO)))._1
    val udfxi = AutomaticDifferentiation.
      evalr(f)(uctx + (xs, (xi, Interval.ONE)) + (ys, (yi, Interval.ZERO)))._2
    val udfyi = AutomaticDifferentiation.
      evalr(f)(uctx + (xs, (xi, Interval.ZERO)) + (ys, (yi, Interval.ONE)))._2

    val ulines =
      for (ldx <- List(udfxi.d, udfxi.u); ldy <- List(udfyi.d, udfyi.u))
        yield Line(ufmxmy.d, mx, my, ldx, ldy).invert()

    Approximation(ConstraintSet2D(xi,yi).split(llines), 
        ConstraintSet2D(xi,yi).split(ulines))
    // fmxmy + (x - mx) dfxi + (y - my) dfyi
  }

}
