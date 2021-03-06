/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.newton

import org.junit.Test

import com.github.comius.RoundingContext
import com.github.comius.reals.Context
import com.github.comius.interval.Interval
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.Integrate
import com.github.comius.reals.newton.AutomaticDifferentiation.Down
import com.github.comius.reals.syntax.Var

class NewtonApproximationsTest {
  import ApproximateNewton._
  import com.github.comius.reals.syntax.Real._

  @Test
  def testEstimate(): Unit = {

    println(estimate(0 < "x" - 1)(new Context(new RoundingContext(10, 10)), "x", Interval(0, 4))) // [1,4]
    println(estimate(0 < Const(1) - "x")(new Context(new RoundingContext(10, 10)), "x", Interval(0, 4))) // [0,1]

    println(estimate(0 < "x" - 3)(new Context(new RoundingContext(10, 10)), "x", Interval(0, 4))) // [3,4]
    println(estimate(0 < Const(3) - "x")(new Context(new RoundingContext(10, 10)), "x", Interval(0, 4))) // [0,3]

    println(estimate(0 < Const(1))(new Context(new RoundingContext(10, 10)), "x", Interval(0, 4))) // [0,4]
    println(estimate(0 < Const(-1))(new Context(new RoundingContext(10, 10)), "x", Interval(0, 4))) // []

    println(estimate(0 < Var("x") * "x")(new Context(new RoundingContext(10, 10)), "x", Interval(-1, 1))) // []
    println(estimate(Var("x") * "x" < 0)(new Context(new RoundingContext(10, 10)), "x", Interval(-1, 1))) // []
  }

  @Test
  def testIntegrate(): Unit = {
    println("->" + AutomaticDifferentiation.evalr(Integrate("x", 0, 1, Var("x") * "x"))(
        Context(new RoundingContext(10, 10)), Set(), Down)) // ([0,1],0) // ([0.000,0.5],0)
  }

}
