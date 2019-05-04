/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals

import com.github.comius.reals.newton.AutomaticDifferentiation

/**
 * Simple approximations.
 */
case object AproximateSimple extends Approximations {
  import com.github.comius.reals.syntax._

  /**
   * Lifts a binary operation on booleans to 'approximating two formulas and returning an approximation'.
   *
   * Type: (Bool -> Bool -> Bool) -> Formula -> Formula -> Context-> Approximation
   *
   * @param op the operation on booleans
   * @param x first formula to be approximated
   * @param y second formula to be approximated
   * @param ctx context in which approximation is done
   * @return approx(x) op approx(y)
   */
  private def lift(op: (Boolean, Boolean) => Boolean)(
    x: Formula, y: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean] =
    {
      val Approximation(l1, u1) = approximate(x)
      val Approximation(l2, u2) = approximate(y)
      Approximation(op(l1, l2), op(u1, u2))
    }

  /**
   * Approximates a formula using booleans for upper and lower.
   *
   * @param formula the formula
   * @ctx context
   * @return approximation (lower and upper given with boolean)
   */
  def approximate(formula: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean] = formula match {
    case Less(x, y) =>
      val a @ Approximation(li1, ui1) = AutomaticDifferentiation.approximate(x)
      val b @ Approximation(li2, ui2) = AutomaticDifferentiation.approximate(y)
      Approximation(li1.u.compareTo(li2.d) < 0, ui1.u.compareTo(ui2.d) < 0)

    case Exists(x, a, b, phi) =>
      approximate(phi)(ctx + (x -> ExistsDomain(a, b)))

    case Forall(x, a, b, phi) =>
      approximate(phi)(ctx + (x -> ForallDomain(a, b)))

    case And(x, y)                => lift(_ && _)(x, y)

    case Or(x, y)                 => lift(_ || _)(x, y)

    case ConstFormula(a: Boolean) => Approximation(a, a)
  }


}
