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

import com.github.comius.reals.syntax.CutR

import com.github.comius.reals.syntax.Cut

import com.github.comius.reals.syntax.Const
import java.math.MathContext
import com.github.comius.reals.syntax.Var

object Examples {
  import Real._
  import com.github.comius.floats.Floats.{ impl => D }

  // Definition of inverse using only multiplication.
  def inverse(x: Real): Real = {
    CutR('y, Exists('xu, D.negInf, D.posInf, x < 'xu && (('y * 'xu < 1 && 0 < x) || (1 < 'y * 'xu && 'y < 0))),
      Exists('xd, D.negInf, D.posInf, 'xd < x && ((1 < 'y * 'xd && 0 < Var('y)) || ('y * 'xd < 1 && x < 0))))  
  }
  
  def inverseR(x: Real): Real = {
    // Because we know how to multiply, we don't need exists  
    CutR('y, ('y * x < 1 && 0 < x) || (1 < 'y * x && x < 0),
      (1 < 'y * x && 0 < x) || ('y * x < 1 && x < 0))
  }
  
  def main(args: Array[String]): Unit = {
    
    Eval.eval(inverseR(3), 3)
    Eval.eval(inverseR(-3), 3)
    Eval.eval(inverse(3), 3)
    Eval.eval(inverse(-3), 3)

    Eval2D.eval(inverseR(3), 3)
    Eval2D.eval(inverseR(-3), 3)

    //Eval2D.eval(inverse(-3), 3) // TODO bug
    // Eval2D.eval(inverse(3), 3) // TODO bug
    
    
    Eval2D.eval(Forall('x, 0, 1, Exists('y, 0, 1, 'x < 'y || 'y < 1)), 10)

    Eval2D.eval(Cut('y, -1, 2, Exists('x, 0, 1, 'y < 'x * (Const(1) - 'x)), Forall('x, 0, 1, 'x * (Const(1) - 'x) < 'y)), 10)

    // e
    Eval2D.eval(Cut('y, 2, 3, Integrate('x, 0, 1, 1 / ('x + 1 / ('y - 1))) < 1,
      1 < Integrate('x, 0, 1, 1 / ('x + 1 / ('y - 1)))), 5)

    // == log(2) = 0.693
    Eval2D.eval(Integrate('x, 1, 2, Const(1) / 'x), 5)

    // PI
    Eval2D.eval(Integrate('x, 0, 1, Cut('y, 0, 1, 'x * 'x + 'y * 'y < 1, 1 < 'x * 'x + 'y * 'y)) * 4, 3)

    Eval2D.eval(Exists('x, 0, 1, 'x * 'x < 0), 10)
    Eval2D.eval(Exists('x, 0, 1, 0 < 'x * 'x), 10)
    Eval2D.eval(Exists('x, D.negInf, D.posInf, 0 < 'x * 'x), 10)
    Eval2D.eval(Exists('x, D.negInf, D.posInf, 0 < 'x * 'x * 'x), 10)
    Eval2D.eval(Exists('x, D.negInf, D.posInf, 'x * 'x * 'x < 0), 10)
    Eval2D.eval(Cut('x, 1, 2, 'x * 'x < 2, 2 < 'x * 'x), 10)
    Eval2D.eval(CutR('x, 'x < 0 || 'x * 'x < 200, 200 < 'x * 'x && Const(0) < 'x), 10)

  }
}
