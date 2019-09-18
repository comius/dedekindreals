/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals

import com.github.comius.RoundingContext
import com.github.comius.reals.newton.ApproximateNewton
import com.github.comius.reals.syntax.Add
import com.github.comius.reals.syntax.And
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.syntax.Div
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Integrate
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Mul
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.syntax.Real
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.newton.AutomaticDifferentiation

object Eval {

  import com.github.comius.floats.Floats.{ impl => D }

  val evaluator: Evaluator = PlaneEvaluator

  def eval(expr: Real, precision: Int): Unit = {
    var rexpr = expr
    val dprec = 200 // precision *2
    val evalStart = System.currentTimeMillis()
    var iterationStart = evalStart

    println("\nEvaluating: " + expr)

    for (i <- 0 to 200) {
      val context = Context[VarDomain](new RoundingContext(0, dprec))
      val prec = D.valueOfEpsilon(precision)

      val l = AutomaticDifferentiation.approximate(rexpr)(context).lower

      val width = l.u.subtract(l.d, context.roundingContext.up)
      val currentTime = System.currentTimeMillis()
      println(s"Loop: ${i}: Dyadic precision: ${dprec}, current value: ${l}, expr ${rexpr.toString.length}, time ${currentTime - iterationStart}")

      iterationStart = currentTime
      if (width.compareTo(prec) < 0) {
        println(s"Total time: ${currentTime - evalStart} ms")
        println(l)
        return ;
      }      
      rexpr = evaluator.refine(rexpr)(Context[VarDomain](new RoundingContext(0, dprec)))
    }
  }

  def eval(expr: Formula, maxSteps: Int): Unit = {
    var rexpr = expr
    val dprec = 200 // precision *2
    val evalStart = System.currentTimeMillis() 
    var iterationStart = evalStart

    println("\nEvaluating: " + expr)

    for (i <- 0 to 200) {
      val context = Context[VarDomain](new RoundingContext(0, dprec))

      val l = evaluator.approximate(rexpr)(context)

      val currentTime = System.currentTimeMillis()
      println(s"Loop: ${i}: Dyadic precision: ${dprec}, current value: ${l}, expr ${rexpr.toString}, time ${currentTime - iterationStart}")
      iterationStart = currentTime
      if (l.lower == l.upper) {
        println(s"Total time: ${currentTime - evalStart} ms") 
        println(l)
        return ;
      }
      rexpr = evaluator.refine(rexpr)(Context[VarDomain](new RoundingContext(0, dprec)))

    }
  }
  
}
