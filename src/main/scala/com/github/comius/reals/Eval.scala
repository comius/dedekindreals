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
  import AproximateSimple._
  import com.github.comius.floats.Floats.{ impl => D }

  trait Refine[-A, +B] {
    def refine(e: A)(implicit ctx: Context[VarDomain]): B
  }
  
  implicit object RefineCut extends Refine[Cut, Cut] {  
    def refine(cut: Cut)(implicit ctx: Context[VarDomain]): Cut = {
        val Cut(x, a, b, l, u) = cut
        
        val (m1, m2) = a.trisect(b, ctx.roundingContext.up.getPrecision)
        val a2 = if (approximate(l)(ctx + (x -> CutDomain(m1, m1))).lower) m1 else a
        val b2 = if (approximate(u)(ctx + (x -> CutDomain(m2, m2))).lower) m2 else b
        //Cut(x, a2,b2, refine(l)(ctx + (x -> CutDomain(a2, b2))), refine(u)(ctx + (x -> CutDomain(a2, b2))))
  
        val t1 = ApproximateNewton.estimate(l)(ctx, x, Interval(a, b))
        val t2 = ApproximateNewton.estimate(u)(ctx, x, Interval(a, b))
        val a3 = t1.lower.supremum() //.max(t2.upper.infimum())
        val b3 = t2.lower.infimum() //.min(t1.upper.supremum())
        // TODO find bugs
        //println(s"debug> ${Interval(a3,b3)} ${t1.lower} ${t2.lower}")
        val an = a2.max(a3)
        val bn = b2.min(b3)
  
        Cut(x, an, bn, Eval.refine(l)(ctx + (x -> CutDomain(an, bn))), Eval.refine(u)(ctx + (x -> CutDomain(an, bn))))
    }
  }
  
  implicit object RefineExists extends Refine[Exists, Formula] {  
    def refine(exists: Exists)(implicit ctx: Context[VarDomain]): Formula = {
      val Exists(x, a, b, phi) = exists
      val m = a.split(b)
      val phi2 = Eval.refine(phi)(ctx + (x -> WholeDomain(a, b)))
      if (phi2.isInstanceOf[ConstFormula])
        phi2
      else
        Or(Exists(x, a, m, phi2), Exists(x, m, b, phi2))
    }
  }

  implicit object RefineForall extends Refine[Forall, Formula] {  
    def refine(forall: Forall)(implicit ctx: Context[VarDomain]): Formula = {
      val Forall(x, a, b, phi) = forall
      val m = a.split(b)
      val phi2 = Eval.refine(phi)(ctx + (x -> WholeDomain(a, b)))
      if (phi2.isInstanceOf[ConstFormula])
        phi2
      else
        And(Forall(x, a, m, phi2), Forall(x, m, b, phi2))
     }
  }

  
  def refine(formula: Formula)(implicit ctx: Context[VarDomain]): Formula = {
    approximate(formula)(ctx) match {
      case Approximation(true, _)  => ConstFormula(true)
      case Approximation(_, false) => ConstFormula(false)
      case _ =>
        formula match {

          case Less(x, y) => Less(refine(x), refine(y))

          case exists: Exists =>
            val r = implicitly[Refine[Exists,Formula]]
            r.refine(exists)            
          case forall: Forall =>
            val r = implicitly[Refine[Forall,Formula]]
            r.refine(forall)
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

  def refine(expr: Real)(implicit ctx: Context[VarDomain]): Real = expr match {
    case cut: Cut => 
      val r = implicitly[Refine[Cut, Real]]
      r.refine(cut)
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

      val l = AutomaticDifferentiation.approximate(rexpr)(context).lower

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
