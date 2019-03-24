
package com.marshall

import Real._
import java.math.BigDecimal
import java.math.BigInteger
import com.marshall.dyadic.DyadicDecimal

object Eval {

  case class Approximation[T](lower: T, upper: T)

  case class Context[T](roundingContext: RoundingContext, vars: Map[Symbol, T] = Map[Symbol, T]()) {
    def +(p: (Symbol, T)) = copy(vars = vars + p)
  }

  def lift(op: (Boolean, Boolean) => Boolean)(x: Formula, y: Formula)(implicit ctx: Context[Approximation[Interval]]) =
    {
      val Approximation(l1, u1) = approximate(x)
      val Approximation(l2, u2) = approximate(y)
      Approximation(op(l1, l2), op(u1, u2))
    }

  def approximate(formula: Formula)(implicit ctx: Context[Approximation[Interval]]): Approximation[Boolean] = formula match {
    case Less(x, y) =>
      val Approximation(li1, ui1) = approximate(x)
      val Approximation(li2, ui2) = approximate(y)
      Approximation(li1._2.compareTo(li2._1) < 0, ui1._2.compareTo(ui2._1) < 0)

    // TODO
    case Exists(x, a, b, phi) =>
      val m = a.split(b) //Utils.splitInterval(a, b, ctx.roundingContext)(0)
      approximate(phi)(ctx + (x -> Approximation(Interval(m, m), Interval(a, b))))

    // case ExistsR(x: Symbol, phi: Formula)                              => false
    case Forall(x, a, b, phi) =>
      val m = a.split(b) //Utils.splitInterval(a, b, ctx.roundingContext.swap)(0)
      approximate(phi)(ctx + (x -> Approximation(Interval(a, b), Interval(m, m))))

    case And(x, y)                => lift(_ && _)(x, y)

    case Or(x, y)                 => lift(_ || _)(x, y)

    case ConstFormula(a: Boolean) => Approximation(a, a)
  }

  def lift(op: (Interval, Interval, RoundingContext) => Interval)(x: Real, y: Real)(implicit ctx: Context[Approximation[Interval]]) =
    {
      val Approximation(l1, u1) = approximate(x)
      val Approximation(l2, u2) = approximate(y)
      Approximation(op(l1, l2, ctx.roundingContext), op(u1, u2, ctx.roundingContext.swap))
    }

  def approximate(expr: Real)(implicit ctx: Context[Approximation[Interval]]): Approximation[Interval] = expr match {
    case Cut(_, a, b, _, _)  => Approximation(Interval(a, b), Interval(b, a))
    case CutR(_, _, _, _, _) => Approximation(Interval(DyadicDecimal.negInf, DyadicDecimal.posInf), Interval(DyadicDecimal.posInf, DyadicDecimal.negInf))

    case Const(a)            => Approximation(Interval(a, a), Interval(a, a))
    case Add(x, y)           => lift(_.add(_, _))(x, y)
    case Sub(x, y)           => lift(_.subtract(_, _))(x, y)
    case Mul(x, y)           => lift(_.multiply(_, _))(x, y)
    case Div(x, y)           => lift(_.divide(_, _))(x, y)
    case Var(name)           => ctx.vars.get(name).get
  }

  def extendContext(ctx: Context[Interval]): Context[Approximation[Interval]] = {
    Context(ctx.roundingContext, ctx.vars.mapValues(i => Approximation(i, i.swap)))
  }

  def refine(formula: Formula)(implicit ctx: Context[Interval]): Formula = {
    approximate(formula)(extendContext(ctx)) match {
      case Approximation(true, _)  => ConstFormula(true)
      case Approximation(_, false) => ConstFormula(false)
      case _ =>
        formula match {

          case Less(x, y) => Less(refine(x), refine(y))

          case Exists(x, a, b, phi) =>
            val m = a.split(b) //Utils.splitInterval(a, b, ctx.roundingContext)(0)
            val phi2 = refine(phi)(ctx + (x -> Interval(a, b)))
            if (phi2.isInstanceOf[ConstFormula])
              phi2
            else
              Or(Exists(x, a, m, phi), Exists(x, m, b, phi))
          case Forall(x, a, b, phi) =>
            val m = a.split(b) //Utils.splitInterval(a, b, ctx.roundingContext)(0)
            val phi2 = refine(phi)(ctx + (x -> Interval(a, b)))
            if (phi2.isInstanceOf[ConstFormula])
              phi2
            else
              And(Forall(x, a, m, phi), Forall(x, m, b, phi))

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

  def refine(expr: Real)(implicit ctx: Context[Interval]): Real = expr match {
    case CutR(x, l, u, a, b) =>

      val aFound = approximate(l)(extendContext(ctx) + (x -> Approximation(Interval(a, a), Interval(a, a)))).lower
      val bFound = approximate(u)(extendContext(ctx) + (x -> Approximation(Interval(b, b), Interval(b, b)))).lower
      if (aFound && bFound) {
        Cut(x, a, b, refine(l)(ctx + (x -> Interval(a, b))), refine(u)(ctx + (x -> Interval(a, b))))
      } else {
        val a2 = if (aFound) a else a.multiply(DyadicDecimal.valueOf(2), ctx.roundingContext.down)
        val b2 = if (bFound) b else b.multiply(DyadicDecimal.valueOf(2), ctx.roundingContext.up)        
        val i = Interval(if (aFound) a else DyadicDecimal.negInf, if (bFound) b else DyadicDecimal.posInf)
        CutR(x, refine(l)(ctx + (x -> i)), refine(u)(ctx + (x -> i)), a2, b2)
      }
    case Cut(x, a, b, l, u) =>
      val (m1, m2) = a.trisect(b, ctx.roundingContext)
      val a2 = if (approximate(l)(extendContext(ctx) + (x -> Approximation(Interval(m1, m1), Interval(m1, m1)))).lower) m1 else a
      val b2 = if (approximate(u)(extendContext(ctx) + (x -> Approximation(Interval(m1, m1), Interval(m2, m2)))).lower) m2 else b
      Cut(x, a2, b2, refine(l)(ctx + (x -> Interval(a2, b2))), refine(u)(ctx + (x -> Interval(a2, b2))))
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
      val context = Context[Approximation[Interval]](new RoundingContext(0, dprec))
      val prec = DyadicDecimal.valueOf(new BigDecimal(BigInteger.ONE, precision, context.roundingContext.down))

      val l = approximate(rexpr)(context).lower

      val width = l._2.subtract(l._1, context.roundingContext.up)
      val ctime = System.currentTimeMillis()
      println(s"Loop: ${i}: Dyadic precision: ${dprec}, current value: ${l}, expr ${rexpr.toString.length}, time ${ctime - stime}")
      stime = ctime
      if (width.compareTo(prec) < 0) {
        println(l)
        return ;
      }
      rexpr = refine(rexpr)(Context[Interval](new RoundingContext(0, dprec)))

    }
  }

  def eval(expr: Formula, maxSteps: Int): Unit = {
    var rexpr = expr
    val dprec = 200 // precision *2
    var stime = System.currentTimeMillis()
    val precision = 53
    
    println("\nEvaluating: " + expr)
    
    for (i <- 0 to 200) {
      val context = Context[Approximation[Interval]](new RoundingContext(0, dprec))
      val prec = DyadicDecimal.valueOf(new BigDecimal(BigInteger.ONE, precision, context.roundingContext.down))

      val l = approximate(rexpr)(context)
    
      val ctime = System.currentTimeMillis()
      println(s"Loop: ${i}: Dyadic precision: ${dprec}, current value: ${l}, expr ${rexpr.toString.length}, time ${ctime - stime}")
      stime = ctime
      if (l.lower == l.upper) {
        println(l)
        return ;
      }
      rexpr = refine(rexpr)(Context[Interval](new RoundingContext(0, dprec)))

    }
  }
  
  def main(args: Array[String]) = {
    //   eval(Cut('x, 0, 2, 'x * 'x < Const(2), Const(2) < 'x * 'x), 10)

    val rc = new RoundingContext(0, 200)
    val N = 100
    val dx = DyadicDecimal.ONE.divide(N, rc.down)
    var yl = DyadicDecimal.valueOf(1)
    var yu = DyadicDecimal.valueOf(1)
    var x = DyadicDecimal.ZERO
    while (x.compareTo(DyadicDecimal.ONE) < 0) {
      yl = yl.add(yl.multiply(dx, rc.down), rc.down)
      yu = yu.divide(DyadicDecimal.ONE.subtract(dx, rc.up), rc.up)
      x = x.add(dx, rc.up)
      //dx = dx.multiply(new BigDecimal(0.90), rc.up)
    }
    println("e = " + Interval(yl, yu))

    //println(upper(Exists('x, new BigDecimal("0.4999"), new BigDecimal("0.5001"), 'y < 'x * (Const(1) - 'x)))(Context(new RoundingContext(0, 200), Map('y -> Interval(new BigDecimal(0.26), new BigDecimal(0.24))))))

    eval(Exists('x, 0, 1, 'x * 'x < 0), 10)
    eval(Exists('x, 0, 1, 0 < 'x * 'x), 10)
    eval(Exists('x, DyadicDecimal.negInf, DyadicDecimal.posInf, 0 < 'x * 'x), 10)
    eval(Exists('x, DyadicDecimal.negInf, DyadicDecimal.posInf, 0 < 'x * 'x * 'x), 10)
    eval(Exists('x, DyadicDecimal.negInf, DyadicDecimal.posInf, 'x * 'x * 'x < 0), 10)
    eval(Cut('x, 1, 2, 'x * 'x < 2, 2 < 'x * 'x), 10)
    eval(CutR('x, 'x < 0 || 'x * 'x < 200, 200 < 'x * 'x && Const(0) < 'x), 10)
    eval(Cut('y, -1, 2, Exists('x, 0, 1, 'y < 'x * (Const(1) - 'x)), Forall('x, 0, 1, 'x * (Const(1) - 'x) < 'y)), 10)
  }
}