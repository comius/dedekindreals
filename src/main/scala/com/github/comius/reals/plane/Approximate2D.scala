/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.plane

import com.github.comius.reals.Approximation
import com.github.comius.reals.Approximations
import com.github.comius.reals.Context
import com.github.comius.reals.CutDomain
import com.github.comius.reals.Interval
import com.github.comius.reals.VarDomain
import com.github.comius.reals.newton.AutomaticDifferentiation
import com.github.comius.reals.newton.AutomaticDifferentiation.Down
import com.github.comius.reals.newton.AutomaticDifferentiation.Up
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Sub

object Approximate2D extends Approximations {
  // given f and search space, returns new lower and upper approximation and new search space (one iteration)
  def estimate(lss: Less, x0: (String, Interval),
               y0: (String, Interval))(implicit ctx: Context[VarDomain]): Approximation[ConstraintSet2D] = {

    val (xs, xi) = x0
    val (ys, yi) = y0

    val Less(x, y) = lss
    val f = Sub(y, x)
    val mx = xi.d.split(yi.u)
    val my = yi.d.split(yi.u)

    val pointContext = ctx + (xs, CutDomain(mx, mx)) + (ys, CutDomain(my, my))
    val intContext = ctx + (xs, CutDomain(xi.d, xi.u)) + (ys, CutDomain(yi.d, yi.u))

    val lfmxmy = AutomaticDifferentiation.evalr(f)(pointContext, Set(), Down)._1
    val ldfxi = AutomaticDifferentiation.evalr(f)(intContext, Set(xs), Down)._2
    val ldfyi = AutomaticDifferentiation.evalr(f)(intContext, Set(ys), Down)._2

    val llines =
      for (ldx <- List(ldfxi.d, ldfxi.u); ldy <- List(ldfyi.d, ldfyi.u))
        yield Line(lfmxmy.d, mx, my, ldx, ldy)

    val ufmxmy = AutomaticDifferentiation.evalr(f)(pointContext, Set(), Up)._1
    val udfxi = AutomaticDifferentiation.evalr(f)(intContext, Set(xs), Up)._2
    val udfyi = AutomaticDifferentiation.evalr(f)(intContext, Set(ys), Up)._2

    val ulines =
      for (ldx <- List(udfxi.d, udfxi.u); ldy <- List(udfyi.d, udfyi.u))
        yield Line(ufmxmy.d, mx, my, ldx, ldy).invert()

    Approximation(
      ConstraintSet2D(xi, yi).split(llines),
      ConstraintSet2D(xi, yi).split(ulines))
    // fmxmy + (x - mx) dfxi + (y - my) dfyi
  }

}
