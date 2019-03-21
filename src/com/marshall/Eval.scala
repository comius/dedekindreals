
package com.marshall

import Real._
import java.math.BigDecimal
import java.math.BigInteger

object Eval {
  
  case class Approximation[T](lower: T, upper: T)
  
  case class Context[T](roundingContext: RoundingContext, vars: Map[Symbol, T] = Map[Symbol, T]()) {
    def +(p: (Symbol, T)) = copy(vars = vars + p)
  }  
  
  def approximate(formula: Formula)(ctx: Context[Approximation[Interval]]): Approximation[Boolean] = formula match {
    case Less(x: Real, y: Real) =>
      val Approximation(li1, ui1) = approximate(x)(ctx)
      val Approximation(li2, ui2) = approximate(y)(ctx)
      Approximation(li1._2.compareTo(li2._1) < 0, ui1._2.compareTo(ui2._1) < 0)

    // TODO
    case Exists(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
      val m = Utils.splitInterval(a, b, ctx.roundingContext)(0)
      approximate(phi)(ctx + (x -> Approximation(Interval(m,m), Interval(a, b))))

    // case ExistsR(x: Symbol, phi: Formula)                              => false
    case Forall(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
      val m = Utils.splitInterval(a, b, ctx.roundingContext.swap)(0)
      approximate(phi)(ctx + (x -> Approximation(Interval(a, b),Interval(m, m))))

    case And(x: Formula, y: Formula) => 
      val Approximation(l1,u1) = approximate(x)(ctx)
      val Approximation(l2,u2) = approximate(y)(ctx)
      Approximation(l1 && l2, u1 && u2)
      
    case Or(x: Formula, y: Formula)  =>
      val Approximation(l1,u1) = approximate(x)(ctx)
      val Approximation(l2,u2) = approximate(y)(ctx)
      Approximation(l1 || l2, u1 || u2)
    case ConstFormula(a: Boolean)    => Approximation(a, a)
  }

  def approximate(expr: Real)(ctx: Context[Approximation[Interval]]): Approximation[Interval] = expr match {
    case Cut(_, a, b, _, _) => Approximation(Interval(a, b), Interval(b, a))
    case CutR(_, _, _)      => Approximation(Interval(null, null),Interval(null, null)) // TODO

    case Const(a)           => Approximation(Interval(a, a), Interval(a, a))
    case Add(x, y) =>
       val Approximation(Interval(lx1, lx2), Interval(ux1, ux2)) = approximate(x)(ctx)
       val Approximation(Interval(ly1, ly2), Interval(uy1, uy2)) = approximate(y)(ctx)
       Approximation(Interval(lx1.add(ly1, ctx.roundingContext.down), lx2.add(ly2, ctx.roundingContext.up)),
           Interval(ux1.add(uy1, ctx.roundingContext.swap.up), ux2.add(uy2, ctx.roundingContext.swap.down)))
    case Sub(x, y) =>
      val Approximation(Interval(lx1, lx2), Interval(ux1, ux2)) = approximate(x)(ctx)
       val Approximation(Interval(ly1, ly2), Interval(uy1, uy2)) = approximate(y)(ctx)
       Approximation(Interval(lx1.subtract(ly2, ctx.roundingContext.down), lx2.subtract(ly1, ctx.roundingContext.up)), // TODO fix
           Interval(ux1.subtract(uy2, ctx.roundingContext.swap.up), ux2.subtract(uy1, ctx.roundingContext.swap.down)))
    case Mul(x, y) =>
      val Approximation(Interval(lx1, lx2), Interval(ux1, ux2)) = approximate(x)(ctx)
       val Approximation(Interval(ly1, ly2), Interval(uy1, uy2)) = approximate(y)(ctx)
       
      Approximation(Interval(
        lx1.multiply(ly1, ctx.roundingContext.down).min(lx1.multiply(ly2, ctx.roundingContext.down)).min(lx2.multiply(ly1, ctx.roundingContext.down)).min(lx2.multiply(ly2, ctx.roundingContext.down)),
        lx1.multiply(ly1, ctx.roundingContext.up).max(lx1.multiply(ly2, ctx.roundingContext.up)).max(lx2.multiply(ly1, ctx.roundingContext.up)).max(lx2.multiply(ly2, ctx.roundingContext.up))), //TODO
      
      Interval(
        ux1.multiply(uy1, ctx.roundingContext.swap.up).max(ux1.multiply(uy2, ctx.roundingContext.swap.up)).max(ux2.multiply(uy1, ctx.roundingContext.swap.up)).max(ux2.multiply(uy2, ctx.roundingContext.swap.up)),
        ux1.multiply(uy1, ctx.roundingContext.swap.down).min(ux1.multiply(uy2, ctx.roundingContext.swap.down)).min(ux2.multiply(uy1, ctx.roundingContext.swap.down)).min(ux2.multiply(uy2, ctx.roundingContext.swap.down)))) //TODO
    /*case Div(x, y) =>
      val Interval(x1, x2) = upper(x)
      val Interval(y1, y2) = upper(y)
      Interval(x1.divide(y2, ctx.roundingContext.up), x2.divide(y1, ctx.roundingContext.down)) */
    case Var(name) =>
      ctx.vars.get(name).get
  }

  def extendContext(ctx: Context[Interval]): Context[Approximation[Interval]] = {
    Context(ctx.roundingContext, ctx.vars.mapValues(i => Approximation(i, i.swap)))
  }
  
  def refine(formula: Formula)(implicit ctx: Context[Interval]): Formula = {
    approximate(formula)(extendContext(ctx)) match {
      case Approximation(true, _) => ConstFormula(true)
      case Approximation(_, false) => ConstFormula(false)
      case _ =>   
      formula match {

        case Less(x: Real, y: Real) => Less(refine(x), refine(y))

        // TODO existsR
        case Exists(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
          val m = Utils.splitInterval(a, b, ctx.roundingContext)(0)
          val phi2 = refine(phi)(ctx + (x -> Interval(a, b)))
          if (phi2.isInstanceOf[ConstFormula])
            phi2
          else
            Or(Exists(x, a, m, phi), Exists(x, m, b, phi))
        case Forall(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) =>
          val m = Utils.splitInterval(a, b, ctx.roundingContext)(0)
          val phi2 = refine(phi)(ctx + (x -> Interval(a, b)))
          if (phi2.isInstanceOf[ConstFormula])
            phi2
          else
            And(Forall(x, a, m, phi), Forall(x, m, b, phi))

        case And(x: Formula, y: Formula) =>
          val a = refine(x)
          val b = refine(y)
          (a, b) match {
            case (ConstFormula(true), b)  => b
            case (ConstFormula(false), b) => ConstFormula(false)
            case (a, ConstFormula(true))  => a
            case (a, ConstFormula(false)) => ConstFormula(false)
            case (a, b)                   => And(a, b)
          }
        case Or(x: Formula, y: Formula) =>
          val a = refine(x)
          val b = refine(y)
          (a, b) match {
            case (ConstFormula(true), b)  => ConstFormula(true)
            case (ConstFormula(false), b) => b
            case (a, ConstFormula(true))  => ConstFormula(true)
            case (a, ConstFormula(false)) => a
            case (a, b)                   => Or(a, b)
          }
        case c: ConstFormula => c
      }
    }
  }

  def refine(expr: Real)(implicit ctx: Context[Interval]): Real = expr match {
    case Cut(x, a, b, l, u) =>
      val m: Array[BigDecimal] = Utils.splitInterval(a, b, ctx.roundingContext)
      val m1 = m(0)
      val m2 = m(1)
      val a2 = if (approximate(l)(extendContext(ctx) + (x -> Approximation(Interval(m1, m1),Interval(m1, m1)))).lower) m1 else a
      val b2 = if (approximate(u)(extendContext(ctx) + (x -> Approximation(Interval(m1, m1),Interval(m2, m2)))).lower) m2 else b
      Cut(x, a2, b2, refine(l)(ctx + (x -> Interval(a2, b2))), refine(u)(ctx + (x -> Interval(a2, b2))))
    case Add(x, y) => Add(refine(x), refine(y))
    case Sub(x, y) => Sub(refine(x), refine(y))
    case Mul(x, y) => Mul(refine(x), refine(y))
    case Div(x, y) => Div(refine(x), refine(y))
    case x         => x
  }

  def eval(expr: Real, precision: Int): Unit = {
    var rexpr = expr
    var dprec = 200 // precision *2
    var stime = System.currentTimeMillis()

    for (i <- 0 to 200) {
      val context = Context[Approximation[Interval]](new RoundingContext(0, dprec))
      val prec = new BigDecimal(BigInteger.ONE, precision, context.roundingContext.down);

      val l = approximate(rexpr)(context).lower

      val width = l._2.subtract(l._1, context.roundingContext.up)
      val ctime = System.currentTimeMillis()
      println(s"Loop: ${i}: Dyadic precision: ${dprec}, current value: ${Utils.intervalToString(l._1, l._2)}, expr ${rexpr.toString.length}, time ${ctime - stime}")
      stime = ctime
      if (width.compareTo(prec) < 0) {
        println(Utils.intervalToString(l._1, l._2))
        return ;
      }
      rexpr = refine(rexpr)(Context[Interval](new RoundingContext(0, dprec)))

    }
  }

  def main(args: Array[String]) = {
    //   eval(Cut('x, 0, 2, 'x * 'x < Const(2), Const(2) < 'x * 'x), 10)

    val rc = new RoundingContext(0, 200)
    val N = 100
    var dx = BigDecimal.ONE.divide(N, rc.down)
    var yl = new BigDecimal(1)
    var yu = new BigDecimal(1)
    var x = BigDecimal.ZERO
    while (x.compareTo(BigDecimal.ONE) < 0) {
      yl = yl.add(yl.multiply(dx, rc.down), rc.down)
      yu = yu.divide(BigDecimal.ONE.subtract(dx, rc.up), rc.up)
      x = x.add(dx, rc.up)
      //dx = dx.multiply(new BigDecimal(0.90), rc.up)
    }
    println("e = " +Utils.intervalToString(yl, yu))
    
    
    //println(upper(Exists('x, new BigDecimal("0.4999"), new BigDecimal("0.5001"), 'y < 'x * (Const(1) - 'x)))(Context(new RoundingContext(0, 200), Map('y -> Interval(new BigDecimal(0.26), new BigDecimal(0.24))))))

    eval(Cut('x, 1, 2, 'x * 'x < 2, 2 < 'x * 'x), 10)
    eval(Cut('y, -1, 2, Exists('x, 0, 1, 'y < 'x * (Const(1) - 'x)), Forall('x, 0, 1, 'x * (Const(1) - 'x) < 'y)), 10)
  }
}