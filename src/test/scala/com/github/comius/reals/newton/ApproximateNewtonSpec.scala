/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.newton

import org.junit.runner.RunWith
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import com.github.comius.RoundingContext
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.floats.FloatsSpec
import com.github.comius.reals.Approximation
import com.github.comius.reals.Context
import com.github.comius.interval.Interval
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.Real
import com.github.comius.reals.newton.AutomaticDifferentiation.Down
import com.github.comius.reals.CutDomain
import com.github.comius.reals.TestUtil
import com.github.comius.reals.syntax.Var

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class ApproximateNewtonSpec extends Properties("ApproximateNewton") {
  import com.github.comius.reals.syntax.Real._
  private val precision = 10
  private val r = new RoundingContext(0, precision)

  private def in(x: D.T, f: Real): Boolean = {
    AutomaticDifferentiation.evalr(f)(
      Context(r, Map(("x", CutDomain(x, x)))), Set("x"), Down)._1.d.signum() > 0
  }

  val funcsLinear = List[Real](Const(0), Const(1), Const(-1), "x", Const(0) - "x", Var("x") + Const(1), "x" - Const(1),
    Const(0) - "x" + Const(1), Const(0) - "x" - Const(1))

  val i = Interval(-1, 1)

  property("linearFunctions") = TestUtil.forall(funcsLinear) { f =>

    val Approximation(lower, upper) =
      ApproximateNewton.estimate(0 < f)(Context(r), "x", i)
    // println(s"$f $lower $upper")
    forAll(FloatsSpec.genFloatInterval(i)) { x =>
      (ConstraintSetSpec.in(x, lower) == in(x, f) &&
        ConstraintSetSpec.in(x, upper) == !in(x, f))
    }
  }

}
