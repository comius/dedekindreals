package com.marshall
import java.math.BigDecimal

case class Interval(x: BigDecimal, y: BigDecimal) extends Product2[BigDecimal, BigDecimal] {
  def _1 = x
  def _2 = y
  
  def swap(): Interval = {
    Interval(y,x)
  }
  
  def add(i2: Interval, r: RoundingContext) = {
    val i1 = this
    Interval(i1.x.add(i2.x, r.down), i1.y.add(i2.y, r.up))
  }
  
  def subtract(i2: Interval, r: RoundingContext) = {
    val i1 = this
    Interval(i1.x.subtract(i2.y, r.down), i1.y.subtract(i2.x, r.up))
  }
  
  def multiply(i2: Interval, r: RoundingContext) = {
    val i1 = this
    val lower = i1.x.multiply(i2.x, r.down).min(
        i1.x.multiply(i2.y, r.down)).min(
        i1.y.multiply(i2.x, r.down)).min(
            i1.y.multiply(i2.y, r.down))
    val upper = i1.x.multiply(i2.x, r.up).max(
        i1.x.multiply(i2.y, r.up)).max(
        i1.y.multiply(i2.x, r.up)).max(
            i1.y.multiply(i2.y, r.up))   
    Interval(lower, upper)
  }
  
  def divide(i2: Interval, r: RoundingContext) = {    
    multiply(Interval(BigDecimal.ONE.divide(i2.y, r.down), BigDecimal.ONE.divide(i2.x, r.down)), r) 
  }
}

 