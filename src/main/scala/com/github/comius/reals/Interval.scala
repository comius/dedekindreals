package com.github.comius.reals

import com.github.comius.Utils;

import com.github.comius.floats.{ DyadicDecimal => D }
import com.github.comius.RoundingContext
import com.github.comius.floats.NaNException
import java.math.RoundingMode

case class Interval(x: D.T, y: D.T) {

  def swap(): Interval = {
    Interval(y, x)
  }

  def add(i2: Interval, r: RoundingContext) = {
    val i1 = this
    Interval(i1.x.add(i2.x, r.down), i1.y.add(i2.y, r.up))
  }

  def subtract(i2: Interval, r: RoundingContext) = {
    val i1 = this
    Interval(i1.x.subtract(i2.y, r.down), i1.y.subtract(i2.x, r.up))
  }

  def multiplyMoore(i2: Interval, r: RoundingContext) = {
    val i1 = this
    val lower = i1.x.multiply(i2.x, r.down).min(
      i1.x.multiply(i2.y, r.down)).min(
        i1.y.multiply(i2.x, r.down)).min(
          i1.y.multiply(i2.y, r.down))
    val upper = i1.x.multiply(i2.x, r.up).max(
      i1.x.multiply(i2.y, r.up)).max(
        i1.y.multiply(i2.x, r.up)).max(
          i1.y.multiply(i2.y, r.up))
    // We need a workaround for back-to-front intervals, but it doesn't work on mixed intervals
    if (x.compareTo(y) < 0) Interval(lower, upper) else Interval(upper, lower)
  }

  def multiply(i2: Interval, r: RoundingContext) = {
    val i1 = this
    val d = i1.x
    val u = i1.y
    val e = i2.x
    val t = i2.y

    (d.signum, u.signum, e.signum, t.signum) match {
      case (-1, -1, 1, 1) => Interval(d.multiply(t, r.down), u.multiply(e, r.up))
      case (-1, -1, 1, _) => Interval(u.multiply(t, r.down), u.multiply(e, r.up))
      case (-1, -1, _, 1) => Interval(d.multiply(t, r.down), d.multiply(e, r.up))
      case (-1, -1, _, _) => Interval(u.multiply(t, r.down), d.multiply(e, r.up))

      case (-1, _, 1, 1)  => Interval(d.multiply(t, r.down), u.multiply(t, r.up))
      case (-1, _, 1, _)  => Interval(D.ZERO, D.ZERO)
      case (-1, _, _, 1) => Interval(
        d.multiply(t, r.down).min(u.multiply(e, r.down)),
        d.multiply(e, r.up).max(u.multiply(t, r.up)))
      case (-1, _, _, _) => Interval(u.multiply(e, r.down), d.multiply(e, r.up))

      case (_, -1, 1, 1) => Interval(d.multiply(e, r.down), u.multiply(e, r.up))
      case (_, -1, 1, _) => Interval(
        d.multiply(e, r.down).max(u.multiply(t, r.down)),
        d.multiply(t, r.up).min(u.multiply(e, r.up)))
      case (_, -1, _, 1) => Interval(D.ZERO, D.ZERO)
      case (_, -1, _, _) => Interval(u.multiply(t, r.down), d.multiply(t, r.up))

      case (_, _, 1, 1)  => Interval(d.multiply(e, r.down), u.multiply(t, r.up))
      case (_, _, 1, _)  => Interval(d.multiply(e, r.down), d.multiply(t, r.up))
      case (_, _, _, 1)  => Interval(u.multiply(e, r.down), u.multiply(t, r.up))
      case (_, _, _, _)  => Interval(u.multiply(e, r.down), d.multiply(t, r.up))
    }
  }

  def multiplyLakayev(i2: Interval, r: RoundingContext) = {
    val i1 = this

    def pm(x: D.T): (D.T, D.T) = {
      x.signum() match {
        case 1  => (x, D.ZERO)
        case -1 => (D.ZERO, x.negate())
        case _  => (D.ZERO, D.ZERO)
      }
    }

    val (lxp, lxm) = pm(i1.x)
    val (uxp, uxm) = pm(i1.y)
    val (lyp, lym) = pm(i2.x)
    val (uyp, uym) = pm(i2.y)

    // TODO check intervals with infinite endpoints - problem when multiplied by zeroes !!!

    Interval(
      lxp.multiply(lyp, r.down).max(uxm.multiply(uym, r.down)).subtract(
        uxp.multiply(lym, r.up).max(lxm.multiply(uyp, r.up)), r.down),
      uxp.multiply(uyp, r.up).max(lxm.multiply(lym, r.up)).subtract(
        lxp.multiply(uym, r.down).max(uxm.multiply(lyp, r.down)), r.up))
  }

  def divide(i2: Interval, r: RoundingContext) = {
    // TODO handle zero in the interval
    multiply(Interval(D.ONE.divide(i2.y, r.down), D.ONE.divide(i2.x, r.down)), r)
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Interval => x.compareTo(that.x) == 0 && y.compareTo(that.y) == 0
      case _              => false
    }

  override def toString() = {
    (x, y) match {
      case (D.Number(a), D.Number(b)) => Utils.intervalToString(a, b)
      case (a, b)                     => s"[${a},${b}]"
    }
  }
}

 