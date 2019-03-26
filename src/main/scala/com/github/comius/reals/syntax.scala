package com.github.comius.reals

import com.github.comius.floats.{DyadicDecimal => D}

sealed trait Real {
  def +(other: Real) = Add(this, other)
  def -(other: Real) = Sub(this, other)
  def *(other: Real) = Mul(this, other)
  def <(other: Real) = Less(this, other)
}

sealed trait Formula {
  def &&(other: Formula) = And(this, other)
  def ||(other: Formula) = Or(this, other)
}

case class Cut(x: Symbol, a: D.T, b: D.T, lower: Formula, upper: Formula) extends Real {
  override def toString: String = {
    s"Cut(${x},${Interval(a, b)},${lower},${upper})"
  }
}
case class CutR(x: Symbol, lower: Formula, upper: Formula, 
    a: D.T = D.valueOf(-1), b: D.T = D.valueOf(1)) extends Real
case class Add(x: Real, y: Real) extends Real
case class Sub(x: Real, y: Real) extends Real
case class Mul(x: Real, y: Real) extends Real
case class Div(x: Real, y: Real) extends Real
case class Const(a: D.T) extends Real {
  override def toString: String = a.toString
}

case class Var(name: Symbol) extends Real {
  override def toString: String = name.toString
}

case class ConstFormula(b: Boolean) extends Formula
case class Less(x: Real, y: Real) extends Formula
case class And(x: Formula, y: Formula) extends Formula
case class Or(x: Formula, y: Formula) extends Formula
case class Forall(x: Symbol, a: D.T, b: D.T, phi: Formula) extends Formula
case class Exists(x: Symbol, a: D.T, b: D.T, phi: Formula) extends Formula

object Real {
  implicit def int2BigDecimal(x: Int): D.T = {
    D.valueOf(x)
  }

  implicit def bigDecimal2Const(x: D.T): Const = {
    Const(x)
  }

  implicit def int2Const(x: Int): Const = {
    Const(D.valueOf(x))
  }

  implicit def symbol2Var(name: Symbol): Var = {
    Var(name)
  }
}