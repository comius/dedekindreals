package com.github.comius.reals

import java.math.MathContext
import java.math.RoundingMode

import scala.annotation.tailrec

import com.github.comius.RoundingContext
import com.github.comius.floats.Floats.{ impl => D }
import scala.util.control.Exception

/**
 * General interval with rational endpoints. There are no restrictions on the endpoints (d<u, d=u or d<u).
 *
 * @constructor Constructs a new interval
 * @param d lower endpoint
 * @param u upper endpoint
 */
case class Interval(d: D.T, u: D.T) {

  /**
   * Flips the interval. Warning: this is not computable on R!
   *
   * @return interval with swapped endpoints
   */
  def flip(): Interval = {
    Interval(u, d)
  }

  private val negInfOnException = Exception.failAsValue(classOf[ArithmeticException])(D.negInf)
  private val posInfOnException = Exception.failAsValue(classOf[ArithmeticException])(D.posInf)

  /**
   * Adds two intervals.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def add(i2: Interval, r: RoundingContext) = {
    val Interval(e, t) = i2

    Interval(negInfOnException(d.add(e, r.down)), posInfOnException(u.add(t, r.up)))
  }

  /**
   * Negates an intervals
   *
   * @return interval
   */
  def negate() = {
    Interval(u.negate, d.negate)
  }

  /**
   * Subtracts two intervals.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def subtract(i2: Interval, r: RoundingContext) = {
    val Interval(e, t) = i2
    Interval(negInfOnException(d.subtract(t, r.down)), posInfOnException(u.subtract(e, r.up)))
  }

  /**
   * Default multiplication is using by Kaucher.
   */
  val multiply = multiplyLakayev(_, _)

  /**
   * Multiplies two intervals using Moore's formulas.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def multiplyMoore(i2: Interval, r: RoundingContext) = {
    val Interval(e, t) = i2
    def min(x: D.T*) = x.reduce(_.min(_))
    def max(x: D.T*) = x.reduce(_.max(_))

    val lower = min(d.multiply(e, r.down), d.multiply(t, r.down), u.multiply(e, r.down), u.multiply(t, r.down))
    val upper = max(d.multiply(e, r.up), d.multiply(t, r.up), u.multiply(e, r.up), u.multiply(t, r.up))

    // We need a workaround for back-to-front intervals, but it doesn't work on mixed intervals
    if (d.compareTo(u) < 0) Interval(lower, upper) else Interval(upper, lower)
  }

  /**
   * Multiplies two intervals using Kaucher's formulas.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def multiplyKaucher(i2: Interval, r: RoundingContext) = {
    val Interval(e, t) = i2

    def mulU(a: D.T, b: D.T) = posInfOnException(a.multiply(b, r.up))
    def mulD(a: D.T, b: D.T) = negInfOnException(a.multiply(b, r.down))

    (d.signum, u.signum, e.signum, t.signum) match {
      case (-1 | 0, -1, 1, 0 | 1)         => Interval(mulD(d, t), mulU(u, e))
      case (-1, -1 | 0, 1, -1)            => Interval(mulD(u, t), mulU(u, e))
      case (-1 | 0, -1, -1 | 0, 1 | 0)    => Interval(mulD(d, t), mulU(d, e))
      case (-1 | 0, -1, -1 | 0, -1)       => Interval(mulD(u, t), mulU(d, e))

      case (-1 | 0, 0 | 1, 1, 0 | 1)      => Interval(mulD(d, t), mulU(u, t))
      case (-1 | 0, 0 | 1, 1, -1)         => Interval(D.ZERO, D.ZERO)
      case (-1 | 0, 0 | 1, -1 | 0, 0 | 1) => Interval(mulD(d, t).min(mulD(u, e)), mulU(d, e).max(mulU(u, t)))
      case (-1 | 0, 0 | 1, -1 | 0, -1)    => Interval(mulD(u, e), mulU(d, e))

      case (1, -1, 0 | 1, 1)              => Interval(mulD(d, e), mulU(u, e))
      case (0 | 1, -1, 1, -1 | 0)         => Interval(mulD(d, e).max(mulD(u, t)), mulU(d, t).min(mulU(u, e)))
      case (1, -1, -1, 1)                 => Interval(D.ZERO, D.ZERO)
      case (1, -1, -1 | 0, -1 | 0)        => Interval(mulD(u, t), mulU(d, t))

      case (1, 0 | 1, 1, 0 | 1)           => Interval(mulD(d, e), mulU(u, t))
      case (1, 0 | 1, 1, -1)              => Interval(mulD(d, e), mulU(d, t))
      case (1, 0 | 1, -1 | 0, 1 | 0)      => Interval(mulD(u, e), mulU(u, t))
      case (1, 0 | 1, -1 | 0, -1)         => Interval(mulD(u, e), mulU(d, t))
    }
  }

  /**
   * Multiplies two intervals using Lakayev's formulas.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def multiplyLakayev(i2: Interval, r: RoundingContext) = {
    def pm(x: D.T): (D.T, D.T) = {
      x.signum() match {
        case 1  => (x, D.ZERO)
        case -1 => (D.ZERO, x.negate())
        case _  => (D.ZERO, D.ZERO)
      }
    }

    val (lxp, lxm) = pm(d)
    val (uxp, uxm) = pm(u)
    val (lyp, lym) = pm(i2.d)
    val (uyp, uym) = pm(i2.u)

    def max(a: D.T, b: D.T): D.T = a.max(b)
    def mulD(a: D.T, b: D.T): D.T = negInfOnException(a.multiply(b, r.down))
    def mulU(a: D.T, b: D.T): D.T = posInfOnException(a.multiply(b, r.up))

    if (d == D.posInf || i2.d == D.posInf || u == D.negInf || i2.u == D.negInf) Interval(D.posInf, D.negInf)
    else
      Interval(
        negInfOnException(max(mulD(lxp, lyp), mulD(uxm, uym)).subtract(max(mulU(uxp, lym), mulU(lxm, uyp)), r.down)),
        posInfOnException(max(mulU(uxp, uyp), mulU(lxm, lym)).subtract(max(mulD(lxp, uym), mulD(uxm, lyp)), r.up)))

  }

  /**
   * Inverse of an interval.
   *
   * @param r rounding context
   * @return interval
   */
  def inverse(r: RoundingContext): Interval = (d.signum(), u.signum()) match {
    case (-1, -1) if d == D.negInf && u == D.negInf => Interval(D.ZERO, D.posInf)
    case (-1, -1) if d == D.negInf => Interval(D.ONE.divide(u, r.down), D.posInf)
    case (1, 1) if u == D.posInf && d == D.posInf => Interval(D.negInf, D.ZERO)
    case (1, 1) if u == D.posInf => Interval(D.negInf, D.ONE.divide(d, r.up))
    case (1, 1) if d == D.posInf => Interval(D.ONE.divide(u, r.down), D.ZERO)
    case (1, 1) | (-1, -1) => Interval(D.ONE.divide(u, r.down), D.ONE.divide(d, r.up))
    case (0, -1) => Interval(D.ONE.divide(u, r.down), D.negInf)
    case (1, 0) => Interval(D.posInf, D.ONE.divide(d, r.up))
    case (1, -1) => Interval(D.posInf, D.negInf)
    case _ => Interval(D.negInf, D.posInf)
  }

  /**
   * Divides two intervals.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def divide(i2: Interval, r: RoundingContext) = {
    multiply(i2.inverse(r), r)
  }

  /**
   * Nice string representation of an interval.
   *
   * @return a string
   */
  override def toString() = {
    val prec = 2

    if (d == u) {
      d.toString
    } else {
      val mcDown = new MathContext(1, RoundingMode.DOWN) // DOWN is towards zero
      val x1 = d.add(D.ZERO, mcDown)
      val y1 = u.add(D.ZERO, mcDown)

      // Numbers are of the same magnitude
      if (x1 == y1) {

        // Now find equal significant digits
        @tailrec
        def findLast(starting: Int)(filter: Int => Boolean): Integer = {
          if (!filter(starting)) starting - 1 else findLast(starting + 1)(filter)
        }
        val p = findLast(prec) { p =>
          val mcDown = new MathContext(p, RoundingMode.DOWN)
          d.add(D.ZERO, mcDown) == u.add(D.ZERO, mcDown) ||
            u.subtract(d, MathContext.UNLIMITED).abs().compareTo(D.valueOfEpsilon(p)) < 0
        }

        val mcDownP = new MathContext(p + prec, RoundingMode.DOWN) // DOWN is towards zero
        val mcUpP = new MathContext(p + prec, RoundingMode.UP) // UP is away from zero

        val xp = d.add(D.ZERO, mcDownP).toPlainString()
        val yp = u.add(D.ZERO, mcUpP).toPlainString()

        val p2 = 1 + findLast(0)(px => px < xp.length && px < yp.length && xp(px) == yp(px))

        s"${xp.substring(0, p2)}[${xp.substring(p2)},${yp.substring(p2)}]"
      } else {
        // We are printing numbers of different magnitude. We're more interested in the magnitude than anything else.
        s"[${x1},${y1}]"
      }
    }
  }
}

 