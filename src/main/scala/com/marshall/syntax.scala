package com.marshall

import java.math.BigDecimal


sealed trait Real {
  def +(other: Real) = Add(this, other)
  def -(other: Real) = Sub(this, other)
  def *(other: Real) = Mul(this, other)
  def <(other: Real) = Less(this, other)
}

sealed trait Formula

case class Cut(x: Symbol, a: BigDecimal, b: BigDecimal, lower: Formula, upper: Formula) extends Real {
  override def toString: String = {
    s"Cut(${x},${Utils.intervalToString(a,b)},${lower},${upper})"
  }
}
case class CutR(x: Symbol, lower: Formula, upper: Formula) extends Real
case class Add(x: Real, y: Real) extends Real
case class Sub(x: Real, y: Real) extends Real
case class Mul(x: Real, y: Real) extends Real
case class Div(x: Real, y: Real) extends Real
case class Const(a: BigDecimal) extends Real {
  override def toString: String = a.toString
}

case class Var(name: Symbol) extends Real {
  override def toString: String = name.toString
}

case class ConstFormula(b: Boolean) extends Formula
case class Less(x: Real, y: Real) extends Formula
case class And(x: Formula, y: Formula) extends Formula
case class Or(x: Formula, y: Formula) extends Formula
case class Forall(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) extends Formula
case class Exists(x: Symbol, a: BigDecimal, b: BigDecimal, phi: Formula) extends Formula
case class ExistsR(x: Symbol, phi: Formula) extends Formula

object Real {
  implicit def int2BigDecimal(x: Int): BigDecimal = {
    new BigDecimal(x)
  }
  
  implicit def bigDecimal2Const(x: BigDecimal): Const = {
    Const(x)
  }

  implicit def int2Const(x: Int): Const = {
    Const(new BigDecimal(x))
  }
  
  implicit def symbol2Var(name: Symbol): Var = {
    Var(name)
  }  
}