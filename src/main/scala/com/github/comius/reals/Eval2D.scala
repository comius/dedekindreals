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
import com.github.comius.reals.syntax.CutR
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
import com.github.comius.reals.Approximate2D.ConstraintSet2D

object Eval2D {
  import BisectionApproximations._
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

  def refine(formula: Formula)(implicit ctx: Context[VarDomain]): Formula = {
    approximate(formula)(ctx) match {
      case Approximation(true, _)  => ConstFormula(true)
      case Approximation(_, false) => ConstFormula(false)
      case _ =>
        formula match {

          case Less(x, y) => Less(refine(x), refine(y))

          case Exists(x, a, b, phi) =>
            val phi2 = refine(phi)(ctx + (x -> ExistsDomain(a, b)))
            if (phi2.isInstanceOf[ConstFormula])
              phi2
            else {
              // Compute new intervals from approximation in 1D
              val a1 = approximate1(phi, x -> ExistsDomain(a, b))

              val ints = toIntervals(a1)
              // println("Exists " + phi + "> " + ints)
              ints.map { i => Exists(x, i.d, i.u, phi2) }
                .reduceOption[Formula](Or(_, _)).getOrElse(ConstFormula(!a1.lower.isInstanceOf[ConstraintSetNone]))
            }
          case Forall(x, a, b, phi) =>
            val phi2 = refine(phi)(ctx + (x -> ForallDomain(a, b)))
            if (phi2.isInstanceOf[ConstFormula])
              phi2
            else {
              // Compute new intervals from approximation in 1D
              val a1 = approximate1(phi, x -> ForallDomain(a, b))
              val ints = toIntervals(a1)
              // println("forall " + phi + "> " + ints)
              ints.map { i => Forall(x, i.d, i.u, phi2) }
                .reduceOption[Formula](And(_, _)).getOrElse(ConstFormula(a1.lower.isInstanceOf[ConstraintSetAll]))
            }

          case And(x, y) =>
            refine(x) match {
              case ConstFormula(true)  => refine(y)
              case ConstFormula(false) => ConstFormula(false)
              case a => refine(y) match {
                case ConstFormula(true)  => a
                case ConstFormula(false) => ConstFormula(false)
                case b                   => And(a, b)
              }
            }
          case Or(x, y) =>
            refine(x) match {
              case ConstFormula(true)  => ConstFormula(true)
              case ConstFormula(false) => refine(y)
              case a => refine(y) match {
                case ConstFormula(true)  => ConstFormula(true)
                case ConstFormula(false) => a
                case b                   => Or(a, b)
              }
            }
          case c: ConstFormula => c
        }
    }
  }

  def approximate2(f: Formula, x0: (Symbol, VarDomain),
                   y0: (Symbol, VarDomain))(implicit ctx: Context[VarDomain]): Approximation[ConstraintSet2D] = f match {
    case Less(x, y) =>
      val cs = Approximate2D.ConstraintSet2D(Interval(x0._2.lower, x0._2.upper), Interval(y0._2.lower, y0._2.upper))
      val (a, s) = Approximate2D.refine(Less(x, y), cs, x0._1, y0._1)
      a
    // TODO deeper nesting
  }

  def approximate1(
    f:  Formula,
    x0: (Symbol, VarDomain))(implicit ctx: Context[VarDomain]): Approximation[ConstraintSet] = f match {
    case Exists(x, a, b, phi) =>
      val Approximation(l, u) = approximate2(phi, x0, x -> ExistsDomain(a, b))
      Approximation(l.projectExists(ctx.roundingContext), u.projectForall(ctx.roundingContext))

    case Forall(x, a, b, phi) =>
      val Approximation(l, u) = approximate2(phi, x0, x -> ForallDomain(a, b))
      Approximation(l.projectForall(ctx.roundingContext), u.projectExists(ctx.roundingContext))

    case And(x, y) =>
      val Approximation(l1, u1) = approximate1(x, x0)
      val Approximation(l2, u2) = approximate1(y, x0)
      Approximation(l1.intersection(l2), u1.intersection(u2))

    case Or(x, y) =>
      val Approximation(l1, u1) = approximate1(x, x0)
      val Approximation(l2, u2) = approximate1(y, x0)
      Approximation(l1.union(l2), u1.union(u2))

    case Less(x, y) => // fall back to Newton
      ApproximateNewton.estimate(Less(x, y))(ctx, x0._1, Interval(x0._2.lower, x0._2.upper))

    case c: ConstFormula =>
      val i = Interval(x0._2.lower, x0._2.upper)
      Approximation(ConstraintSet(i, c.b), ConstraintSet(i, !c.b))
  }

  def refine(expr: Real)(implicit ctx: Context[VarDomain]): Real = expr match {
    case CutR(x, l, u, a, b) =>
      val aFound = approximate(l)(ctx + (x -> CutDomain(a, a))).lower
      val bFound = approximate(u)(ctx + (x -> CutDomain(b, b))).lower
      if (aFound && bFound) {
        Cut(x, a, b, refine(l)(ctx + (x -> CutDomain(a, b))), refine(u)(ctx + (x -> CutDomain(a, b))))
      } else {
        val a2 = if (aFound) a else a.multiply(D.valueOf(2), ctx.roundingContext.down)
        val b2 = if (bFound) b else b.multiply(D.valueOf(2), ctx.roundingContext.up)
        val i = CutDomain(if (aFound) a else D.negInf, if (bFound) b else D.posInf)
        CutR(x, refine(l)(ctx + (x -> i)), refine(u)(ctx + (x -> i)), a2, b2)
      }
    case Cut(x, a, b, l, u) =>

      val (m1, m2) = a.trisect(b, ctx.roundingContext.up.getPrecision)
      val a2 = if (approximate(l)(ctx + (x -> CutDomain(m1, m1))).lower) m1 else a
      val b2 = if (approximate(u)(ctx + (x -> CutDomain(m2, m2))).lower) m2 else b
      // Cut(x, a2,b2, refine(l)(ctx + (x -> CutDomain(a2, b2))), refine(u)(ctx + (x -> CutDomain(a2, b2))))

      // TODO find bugs
      // println(s"debug> ${Interval(a3,b3)} ${t1.lower} ${t2.lower}")

      val a4 = approximate1(l, x -> CutDomain(a, b)).lower.supremum()
      val b4 = approximate1(u, x -> CutDomain(a, b)).lower.infimum()

      val an = a2.max(a4)
      val bn = b2.min(b4)

      Cut(x, an, bn, refine(l)(ctx + (x -> CutDomain(an, bn))), refine(u)(ctx + (x -> CutDomain(an, bn))))
    case Integrate(x, a, b, e) =>
      val m = a.split(b)
      val le = refine(e)(ctx + (x -> CutDomain(a, m)))
      val ue = refine(e)(ctx + (x -> CutDomain(m, b)))
      Add(Integrate(x, a, m, le), Integrate(x, m, b, ue))
    case Add(x, y) => Add(refine(x), refine(y))
    case Sub(x, y) => Sub(refine(x), refine(y))
    case Mul(x, y) => Mul(refine(x), refine(y))
    case Div(x, y) => Div(refine(x), refine(y))
    case x         => x
  }

  def eval(expr: Real, precision: Int): Unit = {
    var rexpr = expr
    val dprec = 200 // precision *2
    var stime = System.currentTimeMillis()

    println("\nEvaluating: " + expr)

    for (i <- 0 to 200) {
      val context = Context[VarDomain](new RoundingContext(0, dprec))
      val prec = D.valueOfEpsilon(precision)

      val l = approximate(rexpr)(context).lower

      val width = l.u.subtract(l.d, context.roundingContext.up)
      val ctime = System.currentTimeMillis()
      println(s"Loop: ${i}: Dyadic precision: ${dprec}, current value: ${l}, expr ${rexpr.toString.length}, time ${ctime - stime}")

      stime = ctime
      if (width.compareTo(prec) < 0) {
        println(l)
        return ;
      }
      rexpr = refine(rexpr)(Context[VarDomain](new RoundingContext(0, dprec)))

    }
  }

  def eval(expr: Formula, maxSteps: Int): Unit = {
    var rexpr = expr
    val dprec = 200 // precision *2
    var stime = System.currentTimeMillis()

    println("\nEvaluating: " + expr)

    for (i <- 0 to 200) {
      val context = Context[VarDomain](new RoundingContext(0, dprec))

      val l = approximate(rexpr)(context)

      val ctime = System.currentTimeMillis()
      println(s"Loop: ${i}: Dyadic precision: ${dprec}, current value: ${l}, expr ${rexpr.toString.length}, time ${ctime - stime}")
      stime = ctime
      if (l.lower == l.upper) {
        println(l)
        return ;
      }
      rexpr = refine(rexpr)(Context[VarDomain](new RoundingContext(0, dprec)))

    }
  }

}
