
package com.marshall

import Real._
import java.math.BigDecimal
import java.math.BigInteger

object Eval {
  type Interval = (BigDecimal, BigDecimal)

  case class Context(roundingContext: RoundingContext, vars: Map[Symbol, Interval] = Map()) {
    def +(p: (Symbol, Interval)) = copy(vars = vars + p)
  }

    def upper(formula: Formula)(implicit ctx: Context): Boolean = formula match {
    case Less(x: Real, y: Real) =>
      val (_, xi) = upper(x)(ctx)
      val (yi, _) = upper(y)(ctx)      
      xi.compareTo(yi) < 0

    // TODO
    case Exists(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
      
      upper(phi)(ctx + (x -> (b, a)))

    // case ExistsR(x: Symbol, phi: Formula)                              => false
    case Forall(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
      val m = Utils.splitInterval(a, b, ctx.roundingContext)(0)
      upper(phi)(ctx + (x -> (m, m)))

    case And(x: Formula, y: Formula) => upper(x) && upper(y)
    case Or(x: Formula, y: Formula)  => upper(x) || upper(y)
    case ConstFormula(a: Boolean)    => a
  }

  def upper(expr: Real)(implicit ctx: Context): Interval = expr match {
    case Cut(_, a, b, _, _) => (b, a)
    case CutR(_, _, _)      => (null, null) // TODO

    case Const(a)           => (a, a)
    case Add(x, y) =>
      val (x1, x2) = upper(x)
      val (y1, y2) = upper(y)
      (x1.add(y1, ctx.roundingContext.up), x2.add(y2, ctx.roundingContext.down))
    case Sub(x, y) =>
      val (x1, x2) = upper(x)
      val (y1, y2) = upper(y)
      (x1.subtract(y1, ctx.roundingContext.up), x2.subtract(y2, ctx.roundingContext.down))
    case Mul(x, y) =>
      val (x1, x2) = upper(x)
      val (y1, y2) = upper(y)
      (
          x1.multiply(y1, ctx.roundingContext.up).max(x1.multiply(y2, ctx.roundingContext.up)).max(x2.multiply(y1, ctx.roundingContext.up)).max(x2.multiply(y2, ctx.roundingContext.up)),
        x1.multiply(y1, ctx.roundingContext.down).min(x1.multiply(y2, ctx.roundingContext.down)).min(x2.multiply(y1, ctx.roundingContext.down)).min(x2.multiply(y2, ctx.roundingContext.down))
        ) //TODO
    case Div(x, y) =>
      val (x1, x2) = upper(x)
      val (y1, y2) = upper(y)
      (x1.divide(y2, ctx.roundingContext.up), x2.divide(y1, ctx.roundingContext.down)) //TODO
    case Var(name) => 
      val (a,b) = ctx.vars.get(name).get
      (b,a)
  }
  
  
  def lower(formula: Formula)(implicit ctx: Context): Boolean = formula match {
    case Less(x: Real, y: Real) =>
      val (_, xi) = lower(x)(ctx)
      val (yi, _) = lower(y)(ctx)      
      xi.compareTo(yi) < 0

    // TODO
    case Exists(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
      val m = Utils.splitInterval(a, b, ctx.roundingContext)(0)
      lower(phi)(ctx + (x -> (m, m))) || lower(phi)(ctx + (x -> (a, a))) || lower(phi)(ctx + (x -> (b, b)))

    // case ExistsR(x: Symbol, phi: Formula)                              => false
    case Forall(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
      lower(phi)(ctx + (x -> (a, b)))

    case And(x: Formula, y: Formula) => lower(x) && lower(y)
    case Or(x: Formula, y: Formula)  => lower(x) || lower(y)
    case ConstFormula(a: Boolean)    => a
  }

  def lower(expr: Real)(implicit ctx: Context): Interval = expr match {
    case Cut(_, a, b, _, _) => (a, b)
    case CutR(_, _, _)      => (null, null) // TODO

    case Const(a)           => (a, a)
    case Add(x, y) =>
      val (x1, x2) = lower(x)
      val (y1, y2) = lower(y)
      (x1.add(y1, ctx.roundingContext.down), x2.add(y2, ctx.roundingContext.up))
    case Sub(x, y) =>
      val (x1, x2) = lower(x)
      val (y1, y2) = lower(y)
      (x1.subtract(y1, ctx.roundingContext.down), x2.subtract(y2, ctx.roundingContext.up))
    case Mul(x, y) =>
      val (x1, x2) = lower(x)
      val (y1, y2) = lower(y)
      (
        x1.multiply(y1, ctx.roundingContext.down).min(x1.multiply(y2, ctx.roundingContext.down)).min(x2.multiply(y1, ctx.roundingContext.down)).min(x2.multiply(y2, ctx.roundingContext.down)),
        x1.multiply(y1, ctx.roundingContext.up).max(x1.multiply(y2, ctx.roundingContext.up)).max(x2.multiply(y1, ctx.roundingContext.up)).max(x2.multiply(y2, ctx.roundingContext.up))) //TODO
    case Div(x, y) =>
      val (x1, x2) = lower(x)
      val (y1, y2) = lower(y)
      (x1.divide(y2, ctx.roundingContext.down), x2.divide(y1, ctx.roundingContext.up)) //TODO
    case Var(name) => ctx.vars.get(name).get
  }

  def refine(formula: Formula)(implicit ctx: Context): Formula = {
    if (lower(formula)(ctx)) {
      ConstFormula(true)
    } else if (!upper(formula)(ctx)) { 
      ConstFormula(false)
    } else {
      formula match {

        case Less(x: Real, y: Real) => Less(refine(x), refine(y))

        // TODO existsR
        case Exists(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
          val m = Utils.splitInterval(a, b, ctx.roundingContext)(0)
          val phi2 = refine(phi)(ctx + (x -> (a, b)))
          if (phi2.isInstanceOf[ConstFormula])
            phi2
          else
            Or(Exists(x, a, m, phi), Exists(x, m, b, phi))
        case Forall(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
          val m = Utils.splitInterval(a, b, ctx.roundingContext)(0)
          val phi2 = refine(phi)(ctx + (x -> (a, b)))
          if (phi2.isInstanceOf[ConstFormula])
            phi2
          else
            And(Forall(x, a, m, phi), Forall(x, m, b, phi))

        case And(x: Formula, y: Formula) => And(refine(x)(ctx), refine(y)(ctx))
        case Or(x: Formula, y: Formula)  => Or(refine(x)(ctx), refine(y)(ctx))
        case c: ConstFormula             => c
      }
    }
  }

  def refine(expr: Real)(implicit ctx: Context): Real = expr match {
    case Cut(x, a, b, l, u) =>
      val m: Array[BigDecimal] = Utils.splitInterval(a, b, ctx.roundingContext)
      val m1 = m(0)
      val m2 = m(1)
      val a2 = if (lower(l)(ctx + (x -> (m1, m1)))) m1 else a
      val b2 = if (lower(u)(ctx + (x -> (m2, m2)))) m2 else b
      Cut(x, a2, b2, refine(l)(ctx + (x -> (a2, b2))), refine(u)(ctx + (x -> (a2, b2))))
    case Add(x, y) => Add(refine(x), refine(y))
    case Sub(x, y) => Sub(refine(x), refine(y))
    case Mul(x, y) => Mul(refine(x), refine(y))
    case Div(x, y) => Div(refine(x), refine(y))
    case x         => x
  }

  def eval(expr: Real, precision: Int): Unit = {
    var rexpr = expr
    var dprec = 200 // precision *2

    for (i <- 0 to 200) {
      val context = Context(new RoundingContext(0, dprec))
      val prec = new BigDecimal(BigInteger.ONE, precision, context.roundingContext.down);

      val l = lower(rexpr)(context)

      val width = l._2.subtract(l._1, context.roundingContext.up)
      println(s"Loop: ${i}: Dyadic precision: ${dprec}, current value: ${Utils.intervalToString(l._1, l._2)}, expr ${rexpr}")
      if (width.compareTo(prec) < 0) {
        println(Utils.intervalToString(l._1, l._2))
        return ;
      }
      rexpr = refine(rexpr)(Context(new RoundingContext(0, dprec)))

    }
  }

  def main(args: Array[String]) = {
    //   eval(Cut('x, 0, 2, 'x * 'x < Const(2), Const(2) < 'x * 'x), 10)
    
    println(upper(Exists('x, new BigDecimal("0.4999"), new BigDecimal("0.5001"), 'y < 'x * (Const(1) - 'x)))(Context(new RoundingContext(0, 200), Map('y -> (new BigDecimal(0.26), new BigDecimal(0.24))))))

    eval(Cut('y, -1, 2, Exists('x, 0, 1, 'y < 'x * (Const(1) - 'x)), Forall('x, 0, 1, 'x * (Const(1) - 'x) < 'y)), 10)
  }
}