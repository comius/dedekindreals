/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.interval

import com.github.comius.RoundingContext

trait FunctionEvaluator {
  val name: (String, String)
  def eval(a: Interval, r: RoundingContext): Interval
  def evalp(a: (Interval, Interval), r: RoundingContext): (Interval, Interval)
}

trait BinaryFunctionEvaluator {
  val name: (String, String, String)
  def eval(a: Interval, b: Interval, r: RoundingContext): Interval
  def evalp(a: (Interval, Interval), b: (Interval, Interval), r: RoundingContext): (Interval, Interval)
}

case object Add extends BinaryFunctionEvaluator {
  override val name = ("(", "+", ")")
  override def eval(a: Interval, b: Interval, r: RoundingContext): Interval = {
    a.add(b, r)
  }
  override def evalp(a: (Interval, Interval), b: (Interval, Interval), r: RoundingContext): (Interval, Interval) = {
    (a._1.add(b._1, r), a._2.add(b._2, r))
  }
}

case object Sub extends BinaryFunctionEvaluator {
  override val name = ("(", "-", ")")
  override def eval(a: Interval, b: Interval, r: RoundingContext): Interval = {
    a.subtract(b, r)
  }
  override def evalp(a: (Interval, Interval), b: (Interval, Interval), r: RoundingContext): (Interval, Interval) = {
    (a._1.subtract(b._1, r), a._2.subtract(b._2, r))
  }
}

case object Mul extends BinaryFunctionEvaluator {
  override val name = ("(", "*", ")")
  override def eval(a: Interval, b: Interval, r: RoundingContext): Interval = {
    a.multiply(b, r)
  }
  override def evalp(a: (Interval, Interval), b: (Interval, Interval), r: RoundingContext): (Interval, Interval) = {
    (a._1.multiply(b._1, r), a._1.multiply(b._2, r).add(b._1.multiply(a._2, r), r))
  }
}

case object Div extends BinaryFunctionEvaluator {
  override val name = ("(", "/", ")")
  override def eval(a: Interval, b: Interval, r: RoundingContext): Interval = {
    a.divide(b, r)
  }
  override def evalp(a: (Interval, Interval), b: (Interval, Interval), r: RoundingContext): (Interval, Interval) = {
    (a._1.divide(b._1, r), a._2.multiply(b._1, r).subtract(b._2.multiply(a._1, r.swap), r.swap)
      .divide(b._1.multiply(b._1, r), r.swap())) // TODO rounding
  }
}

final case class Pow(n: Int) extends FunctionEvaluator {
  override val name = ("(", s")^$n")
  override def eval(a: Interval, r: RoundingContext): Interval = {
    var re = Interval.ONE
    for (i <- 1 to n) {
      re = re.multiply(a, r)
    }
    re
  }

  override def evalp(a: (Interval, Interval), r: RoundingContext): (Interval, Interval) = {
    (eval(a._1, r), Interval.ONE)
    var re = (Interval.ONE, Interval.ZERO)
    for (i <- 1 to n) {
      re = Mul.evalp(re, a, r)
    }
    re
    // TODO optimize multiplications, interval powers, proper derivative, negative powers
  }
}
