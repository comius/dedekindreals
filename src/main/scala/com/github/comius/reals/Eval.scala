
package com.github.comius.reals

import Real._

import com.github.comius.RoundingContext;

object Eval {
  import com.github.comius.floats.Floats.{impl => D}
  import Approximations._

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
        val a2 = if (aFound) a else a.multiply(D.valueOf(2), ctx.roundingContext.down)
        val b2 = if (bFound) b else b.multiply(D.valueOf(2), ctx.roundingContext.up)
        val i = Interval(if (aFound) a else D.negInf, if (bFound) b else D.posInf)
        CutR(x, refine(l)(ctx + (x -> i)), refine(u)(ctx + (x -> i)), a2, b2)
      }
    case Cut(x, a, b, l, u) =>
      val (m1, m2) = a.trisect(b, ctx.roundingContext.up.getPrecision)
      val a2 = if (approximate(l)(extendContext(ctx) + (x -> Approximation(Interval(m1, m1), Interval(m1, m1)))).lower) m1 else a
      val b2 = if (approximate(u)(extendContext(ctx) + (x -> Approximation(Interval(m1, m1), Interval(m2, m2)))).lower) m2 else b

      /*val test = NewtonApproximations.estimate(l)(extendContext(ctx), x, Interval(a,b))
      val ls = l.toString

      println(s"debug> ${Interval(a,b)}, ${test.lower} ${ls.substring(0, Math.min(100, ls.length))}")
     */
      Cut(x, a2, b2, refine(l)(ctx + (x -> Interval(a2, b2))), refine(u)(ctx + (x -> Interval(a2, b2))))
    case Integrate(x, a, b, e) =>
      val m = a.split(b)
      val le = refine(e)(ctx + (x -> Interval(a, m)))
      val ue = refine(e)(ctx + (x -> Interval(m, b)))
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
      val context = Context[Approximation[Interval]](new RoundingContext(0, dprec))
      val prec = D.valueOfEpsilon(precision)

      val l = approximate(rexpr)(context).lower

      val width = l.y.subtract(l.x, context.roundingContext.up)
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

    println("\nEvaluating: " + expr)

    for (i <- 0 to 200) {
      val context = Context[Approximation[Interval]](new RoundingContext(0, dprec))

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
    // e
    eval(Cut('y, 2, 3, Integrate('x, 0, 1, ('y - 1) / ('x * 'y + 1 - 'x)) < 1,
      1 < Integrate('x, 0, 1, ('y - 1) / ('x * 'y + 1 - 'x))), 1)

    // == log(2) = 0.693
    eval(Integrate('x, 0, 1, Const(1) / (Const(1) + 'x)), 3)

    // PI
    eval(Integrate('x, 0, 1, Cut('y, 0, 1, 'x * 'x + 'y * 'y < 1, 1 < 'x * 'x + 'y * 'y)) * 4, 2)

    /*
    val rc = new RoundingContext(0, 200)
    val N = 100
    val dx = D.ONE.divide(N, rc.down)
    var yl = D.valueOf(1)
    var yu = D.valueOf(1)
    var x = D.ZERO
    while (x.compareTo(D.ONE) < 0) {
      yl = yl.add(yl.multiply(dx, rc.down), rc.down)
      yu = yu.divide(D.ONE.subtract(dx, rc.up), rc.up)
      x = x.add(dx, rc.up)
      //dx = dx.multiply(new BigDecimal(0.90), rc.up)
    }
    println("e = " + Interval(yl, yu))
		*/
    
    //println(upper(Exists('x, new BigDecimal("0.4999"), new BigDecimal("0.5001"), 'y < 'x * (Const(1) - 'x)))(Context(new RoundingContext(0, 200), Map('y -> Interval(new BigDecimal(0.26), new BigDecimal(0.24))))))

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