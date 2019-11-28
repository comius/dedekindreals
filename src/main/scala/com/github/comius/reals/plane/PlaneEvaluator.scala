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
import com.github.comius.reals.newton.ConstraintSet
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetAll
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetNone
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
import com.github.comius.reals.plane.Approximate2D
import com.github.comius.reals.plane.ConstraintSet2D
import com.github.comius.reals.newton.AutomaticDifferentiation

object PlaneEvaluator extends Evaluator {

  def approximate(formula: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean] = {
    approximate0(formula)
  }

  import AproximateSimple._
  import com.github.comius.floats.Floats.{ impl => D }

  def toIntervals(a: Approximation[ConstraintSet]): List[Interval] = {
    val is = a.lower.union(a.upper).complement().toIntervals()

    // Add midpoint, to guarantee progress
    val m = a.lower.domain.d.split(a.lower.domain.u)

    is.flatMap { i =>
      if (i.d.compareTo(m) <= 0 && i.u.compareTo(m) >= 0)
        List(Interval(i.d, m), Interval(m, i.u))
      else List(i)
    }
  }

  def refineExists(exists: Exists)(implicit ctx: Context[VarDomain]): Formula = {
    val Exists(x, a, b, phi) = exists
    val phi2 = refine(phi)(ctx + (x -> WholeDomain(a, b)))
    phi2 match {
      case _: ConstFormula => phi2
      case _ =>
        // Compute new intervals from approximation in 1D
        val a1 = approximate1(phi, x -> Interval(a, b))

        val ints = toIntervals(a1)
        // println("Exists " + phi + "> " + ints)
        ints.map { i => Exists(x, i.d, i.u, phi2) }
          .reduceOption[Formula](Or(_, _)).getOrElse(ConstFormula(!a1.lower.isInstanceOf[ConstraintSetNone]))
    }
  }

  def refineForall(forall: Forall)(implicit ctx: Context[VarDomain]): Formula = {
    val Forall(x, a, b, phi) = forall
    val phi2 = refine(phi)(ctx + (x -> WholeDomain(a, b)))
    phi2 match {
      case _: ConstFormula => phi2
      case _ =>
        // Compute new intervals from approximation in 1D
        val a1 = approximate1(phi, x -> Interval(a, b))
        val ints = toIntervals(a1)
        // println("forall " + phi + "> " + ints)
        ints.map { i => Forall(x, i.d, i.u, phi2) }
          .reduceOption[Formula](And(_, _)).getOrElse(ConstFormula(a1.lower.isInstanceOf[ConstraintSetAll]))
    }
  }

  def approximate2(f: Formula, x0: (String, Interval),
    y0: (String, Interval))(implicit ctx: Context[VarDomain]): Approximation[ConstraintSet2D] = f match {
    case Less(x, y) =>
      Approximate2D.estimate(Less(x, y), x0, y0)

    case And(x, y) =>
      val Approximation(l1, u1) = approximate2(x, x0, y0)
      val Approximation(l2, u2) = approximate2(y, x0, y0)
      Approximation(l1.intersection(l2), u1.union(u2))

    case Or(x, y) =>
      val Approximation(l1, u1) = approximate2(x, x0, y0)
      val Approximation(l2, u2) = approximate2(y, x0, y0)
      Approximation(l1.union(l2), u1.intersection(u2))
    // TODO deeper nesting
  }

  def approximate1(
    f: Formula,
    x0: (String, Interval))(implicit ctx: Context[VarDomain]): Approximation[ConstraintSet] = f match {
    case Exists(x, a, b, phi) =>
      val Approximation(l, u) = approximate2(phi, x0, x -> Interval(a, b))
      // println("exists l: "+ l)
      // println("exists u: "+ u)
      Approximation(l.projectExists(ctx.roundingContext), u.projectForall(ctx.roundingContext))

    case Forall(x, a, b, phi) =>
      val Approximation(l, u) = approximate2(phi, x0, x -> Interval(a, b))
      // println("forall l: "+ l)
      // println("forall u: "+ u)

      Approximation(l.projectForall(ctx.roundingContext), u.projectExists(ctx.roundingContext))

    case And(x, y) =>
      val Approximation(l1, u1) = approximate1(x, x0)
      val Approximation(l2, u2) = approximate1(y, x0)
      Approximation(l1.intersection(l2), u1.union(u2))

    case Or(x, y) =>
      val Approximation(l1, u1) = approximate1(x, x0)
      val Approximation(l2, u2) = approximate1(y, x0)
      Approximation(l1.union(l2), u1.intersection(u2))

    case Less(x, y) => // fall back to Newton
      ApproximateNewton.estimate(Less(x, y))(ctx, x0._1, x0._2)

    case c: ConstFormula =>
      val i = x0._2
      Approximation(ConstraintSet(i, c.b), ConstraintSet(i, !c.b))
  }

  def approximate0(formula: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean] = formula match {
    case Less(x, y) =>
      val a @ Approximation(li1, ui1) = AutomaticDifferentiation.approximate(x)
      val b @ Approximation(li2, ui2) = AutomaticDifferentiation.approximate(y)
      Approximation(li1.u.compareTo(li2.d) < 0, ui1.u.compareTo(ui2.d) < 0)

    case Exists(x, a, b, phi) =>
      val Approximation(l, u) = approximate1(phi, (x, Interval(a, b)))(ctx)
      Approximation(!l.isInstanceOf[ConstraintSetNone], !u.isInstanceOf[ConstraintSetAll])

    case Forall(x, a, b, phi) =>
      val Approximation(l, u) = approximate1(phi, (x, Interval(a, b)))(ctx)
      Approximation(l.isInstanceOf[ConstraintSetAll], u.isInstanceOf[ConstraintSetNone])

    case And(x, y) =>
      val Approximation(l1, u1) = approximate0(x)
      val Approximation(l2, u2) = approximate0(y)
      Approximation(l1 && l2, u1 && u2)

    case Or(x, y) =>
      val Approximation(l1, u1) = approximate0(x)
      val Approximation(l2, u2) = approximate0(y)
      Approximation(l1 || l2, u1 || u2)

    case ConstFormula(a: Boolean) => Approximation(a, a)
  }

  def refineCut(cut: Cut)(implicit ctx: Context[VarDomain]): Cut = {
    val Cut(x, a, b, l, u) = cut

    val (m1, m2) = a.trisect(b, ctx.roundingContext.up.getPrecision)
    val a2 = if (approximate0(l)(ctx + (x -> CutDomain(m1, m1))).lower) m1 else a
    val b2 = if (approximate0(u)(ctx + (x -> CutDomain(m2, m2))).lower) m2 else b
    // Cut(x, a2,b2, refine(l)(ctx + (x -> CutDomain(a2, b2))), refine(u)(ctx + (x -> CutDomain(a2, b2))))

    // TODO find bugs
    // println(s"debug> ${Interval(a3,b3)} ${t1.lower} ${t2.lower}")

    val Approximation(ll, lu) = approximate1(l, x -> Interval(a, b))
    val Approximation(ul, uu) = approximate1(u, x -> Interval(a, b))

    // println("lowe> " + ll.supremum()+ " " + uu.supremum)
    // println("upe> " + ul.infimum()+ " " + lu.infimum)
    val a4 = ll.supremum() // .max(uu.supremum)
    val b4 = ul.infimum() // .min(lu.infimum)

    val an = a2.max(a4)
    val bn = b2.min(b4)

    Cut(x, an, bn, refine(l)(ctx + (x -> CutDomain(an, bn))), refine(u)(ctx + (x -> CutDomain(an, bn))))
  }
}
