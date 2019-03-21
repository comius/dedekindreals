package com.marshall
import java.math.BigDecimal

case class Interval(x: BigDecimal, y: BigDecimal) extends Product2[BigDecimal, BigDecimal] {
  def _1 = x
  def _2 = y

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
      case (-1, -1, 1, 1) => Interval(d.multiply(t,r.down), u.multiply(e, r.up))
      case (-1, -1, 1, _) => Interval(u.multiply(t,r.down), u.multiply(e, r.up))
      case (-1, -1, _, 1) => Interval(d.multiply(t,r.down), d.multiply(e, r.up))
      case (-1, -1, _, _) => Interval(u.multiply(t,r.down), d.multiply(e, r.up))
      
      case (-1, _, 1, 1) => Interval(d.multiply(t,r.down), u.multiply(t, r.up))
      case (-1, _, 1, _) => Interval(BigDecimal.ZERO, BigDecimal.ZERO)
      case (-1, _, _, 1) => Interval(d.multiply(t,r.down).min(u.multiply(e,r.down)), 
          d.multiply(e,r.up).max(u.multiply(t,r.up)))
      case (-1, _, _, _) => Interval(u.multiply(e,r.down), d.multiply(t, r.up))
      
      case (_, -1, 1, 1) => Interval(d.multiply(e,r.down), u.multiply(e, r.up))
      case (_, -1, 1, _) => Interval(d.multiply(e,r.down).max(u.multiply(t,r.down)),
          d.multiply(t,r.up).min(u.multiply(e,r.up)))
      case (_, -1, _, 1) => Interval(BigDecimal.ZERO, BigDecimal.ZERO)
      case (_, -1, _, _) => Interval(u.multiply(t,r.down), d.multiply(t, r.up))
      
      case (_, _, 1, 1) => Interval(d.multiply(e,r.down), u.multiply(t, r.up))
      case (_, _, 1, _) => Interval(d.multiply(e,r.down), d.multiply(t, r.up))
      case (_, _, _, 1) => Interval(u.multiply(e,r.down), u.multiply(t, r.up))
      case (_, _, _, _) => Interval(u.multiply(e,r.down), d.multiply(t, r.up))
    }
  }

  def divide(i2: Interval, r: RoundingContext) = {
    multiply(Interval(BigDecimal.ONE.divide(i2.y, r.down), BigDecimal.ONE.divide(i2.x, r.down)), r)
  }
}

 