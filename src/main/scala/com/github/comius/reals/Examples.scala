package com.github.comius.reals

import com.github.comius.reals.syntax.Real

import com.github.comius.reals.syntax.Integrate

import com.github.comius.reals.syntax.Forall

import com.github.comius.reals.syntax.Exists

import com.github.comius.reals.syntax.CutR

import com.github.comius.reals.syntax.Cut

import com.github.comius.reals.syntax.Const

object Examples {
   import Real._
   import Eval.eval
   import com.github.comius.floats.Floats.{impl => D}
  
   def main(args: Array[String]) = {
    // e
    eval(Cut('y, 2, 3, Integrate('x, 0, 1, 1 / ('x + 1/('y-1))) < 1,
      1 < Integrate('x, 0, 1, 1 / ('x + 1/('y-1)))), 3)      
      
    val u = Integrate('x, 0, 1, 1 / ('x + 1/('y-1)))
    eval(Cut('y, 2, 3, u < 1, 1 < u), 2)  
      
      
    // == log(2) = 0.693
    eval(Integrate('x, 1, 2, Const(1) / 'x), 5)

    // PI
    eval(Integrate('x, 0, 1, Cut('y, 0, 1, 'x * 'x + 'y * 'y < 1, 1 < 'x * 'x + 'y * 'y)) * 4, 3)
    
    eval(Exists('x, 0, 1, 'x * 'x < 0), 10)
    eval(Exists('x, 0, 1, 0 < 'x * 'x), 10)
    eval(Exists('x, D.negInf, D.posInf, 0 < 'x * 'x), 10)
    eval(Exists('x, D.negInf, D.posInf, 0 < 'x * 'x * 'x), 10)
    eval(Exists('x, D.negInf, D.posInf, 'x * 'x * 'x < 0), 10)
    eval(Cut('x, 1, 2, 'x * 'x < 2, 2 < 'x * 'x), 10)
    eval(CutR('x, 'x < 0 || 'x * 'x < 200, 200 < 'x * 'x && Const(0) < 'x), 10)
    eval(Cut('y, -1, 2, Exists('x, 0, 1, 'y < 'x * (Const(1) - 'x)), Forall('x, 0, 1, 'x * (Const(1) - 'x) < 'y)), 10)
  }
}