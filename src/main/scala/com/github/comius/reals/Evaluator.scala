package com.github.comius.reals

import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.And
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.syntax.Real
import com.github.comius.reals.syntax.Integrate
import com.github.comius.reals.syntax.Add
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.syntax.Mul
import com.github.comius.reals.syntax.Div
import com.github.comius.RoundingContext
import com.github.comius.reals.newton.AutomaticDifferentiation

trait Evaluator {
  import com.github.comius.floats.Floats.{ impl => D }
    
  def approximate(formula: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean]
    
  def refineCut(cut: Cut)(implicit ctx: Context[VarDomain]): Cut
  def refineExists(exists: Exists)(implicit ctx: Context[VarDomain]): Formula
  def refineForall(forall: Forall)(implicit ctx: Context[VarDomain]): Formula

  def refine(formula: Formula)(implicit ctx: Context[VarDomain]): Formula = {
    approximate(formula)(ctx) match {
      case Approximation(true, _) => ConstFormula(true)
      case Approximation(_, false) => ConstFormula(false)
      case _ =>
        formula match {

          case Less(x, y) => Less(refine(x), refine(y))

          case exists: Exists =>
            refineExists(exists)
          case forall: Forall =>
            refineForall(forall)
          case And(x, y) =>
            refine(x) match {
              case ConstFormula(true) => refine(y)
              case ConstFormula(false) => ConstFormula(false)
              case a => refine(y) match {
                case ConstFormula(true) => a
                case ConstFormula(false) => ConstFormula(false)
                case b => And(a, b)
              }
            }
          case Or(x, y) =>
            refine(x) match {
              case ConstFormula(true) => ConstFormula(true)
              case ConstFormula(false) => refine(y)
              case a => refine(y) match {
                case ConstFormula(true) => ConstFormula(true)
                case ConstFormula(false) => a
                case b => Or(a, b)
              }
            }
          case c: ConstFormula => c
        }
    }
  }

  def refine(expr: Real)(implicit ctx: Context[VarDomain]): Real = expr match {
    case cut: Cut =>
      refineCut(cut)
    case Integrate(x, a, b, e) =>
      val m = a.split(b)
      val le = refine(e)(ctx + (x -> CutDomain(a, m)))
      val ue = refine(e)(ctx + (x -> CutDomain(m, b)))
      Add(Integrate(x, a, m, le), Integrate(x, m, b, ue))
    case Add(x, y) => Add(refine(x), refine(y))
    case Sub(x, y) => Sub(refine(x), refine(y))
    case Mul(x, y) => Mul(refine(x), refine(y))
    case Div(x, y) => Div(refine(x), refine(y))
    case x => x
  }
  
  
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
      rexpr = refine(rexpr)(Context[VarDomain](new RoundingContext(0, dprec)))
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

      val l = approximate(rexpr)(context)

      val currentTime = System.currentTimeMillis()
      println(s"Loop: ${i}: Dyadic precision: ${dprec}, current value: ${l}, expr ${rexpr.toString}, time ${currentTime - iterationStart}")
      iterationStart = currentTime
      if (l.lower == l.upper) {
        println(s"Total time: ${currentTime - evalStart} ms") 
        println(l)
        return ;
      }
      rexpr = refine(rexpr)(Context[VarDomain](new RoundingContext(0, dprec)))

    }
  }
}