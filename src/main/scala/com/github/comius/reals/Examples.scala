/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals

import com.github.comius.reals.syntax.Real

import com.github.comius.reals.syntax.Integrate

import com.github.comius.reals.syntax.Forall

import com.github.comius.reals.syntax.Exists

import com.github.comius.reals.syntax.Cut

import com.github.comius.reals.syntax.Const
import java.math.MathContext
import com.github.comius.reals.syntax.Var

// scalastyle:off magic.number
object Examples {
  import Real._
  import com.github.comius.floats.Floats.{ impl => D }

  // Definition of inverse using only multiplication.
  def inverse(x: Real): Real = {
    cut("y", y => exists("xu", D.negInf, D.posInf, xu => x < xu && ((y * xu < 1 && 0 < x) || (1 < y * xu && y < 0))),
      y => exists("xd", D.negInf, D.posInf, xd => xd < x && ((1 < y * xd && 0 < y) || (y * xd < 1 && x < 0))))
  }

  def inverseR(x: Real): Real = {
    // Because we know how to multiply, we don't need exists
    cut("y", y => (y * x < 1 && 0 < x) || (1 < y * x && x < 0),
      y => (1 < y * x && 0 < x) || (y * x < 1 && x < 0))
  }

  def main(args: Array[String]): Unit = {
    val eval: Evaluator = SimpleEvaluator

    val a = Const(77617.0)
    val b = Const(33096.0)
    eval.eval(Const(333.75) * b * b * b * b * b * b + a * a *
      (11 * a * a * b * b - b * b * b * b * b * b - 121 * b * b * b * b - 2) +
      Const(5.5) * b * b * b * b * b * b * b * b + a / (2 * b), 10)

    eval.eval(cut("x", 1, 2, x => x < 1 + 1 / x, x => 1 + 1 / x < x), 10)
    eval.eval(cut("x", 0, 2, x => cut("y", 0, 2, y => y < x, y => x < y) < 1, x => 1 < x), 10)

    eval.eval(inverseR(3), 3)
    eval.eval(inverseR(-3), 3)
    eval.eval(inverse(3), 3)
    eval.eval(inverse(-3), 3)

    eval.eval(forall("x", 0, 1, x => exists("y", 0, 1, y => x < y || y < 1)), 10)

    eval.eval(cut("y", -1, 2, y => exists("x", 0, 1, x => y < x * (1 - x)),
      y => forall("x", 0, 1, x => x * (1 - x) < y)), 10)

    // e
    eval.eval(cut("y", 2, 3, y => integrate("x", 0, 1, x => 1 / (x + 1 / (y - 1))) < 1,
      y => 1 < integrate("x", 0, 1, x => 1 / (x + 1 / (y - 1)))), 5)

    // == log(2) = 0.693
    eval.eval(Integrate("x", 1, 2, Const(1) / "x"), 5)

    // PI
    eval.eval(integrate("x", 0, 1, x => cut("y", 0, 1, y => x * x + y * y < 1, y => 1 < x * x + y * y)) * 4, 3)

    eval.eval(exists("x", 0, 1, x => x * x < 0), 10)
    eval.eval(exists("x", 0, 1, x => 0 < x * x), 10)
    eval.eval(exists("x", D.negInf, D.posInf, x => 0 < x * x), 10)
    eval.eval(exists("x", D.negInf, D.posInf, x => 0 < x * x * x), 10)
    eval.eval(exists("x", D.negInf, D.posInf, x => x * x * x < 0), 10)
    eval.eval(cut("x", 1, 2, x => x * x < 2, x => 2 < x * x), 10)
    //  eval.eval(cut("x", x => x < 0 || x * x < 200, x => 200 < x * x && 0 < x), 10)
  }
}
