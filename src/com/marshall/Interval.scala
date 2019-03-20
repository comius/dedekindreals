package com.marshall
import java.math.BigDecimal

case class Interval(x1: BigDecimal, x2: BigDecimal) extends Product2[BigDecimal, BigDecimal] {
  def _1 = x1
  def _2 = x2
}

 