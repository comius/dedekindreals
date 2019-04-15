package com.github.comius.reals.syntax

import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.Interval
import java.math.MathContext
import java.math.RoundingMode

/* Our syntax is made of Reals and Formulas */

/**
 * Real number.
 *
 * Arithmetic and comparison operators are defined here to ease writing expressione. The operators just compose together
 * syntax.
 */
sealed trait Real {
  def +(other: Real) = Add(this, other)
  def -(other: Real) = Sub(this, other)
  def *(other: Real) = Mul(this, other)
  def /(other: Real) = Div(this, other)
  def <(other: Real) = Less(this, other)
}

/**
 * Formulas.
 *
 * Logic operators are defined here to ease writing expressione. The operators just compose expressions together
 * into a syntax tree.
 */
sealed trait Formula {
  def &&(other: Formula) = And(this, other)
  def ||(other: Formula) = Or(this, other)
}

/* Constructors for Real: constants, variables arithmetic operations, Cut, CutR, integration */

/**
 * A constant.
 *
 * @param a the value of the constant
 */
case class Const(a: D.T) extends Real {
  override def toString: String = a.toString
}

/**
 * A variable.
 * 
 * @param name name of the variable.
 */
case class Var(name: Symbol) extends Real {
  override def toString: String = name.toString
}

/** Addition. */
case class Add(x: Real, y: Real) extends Real

/** Subtraction. */
case class Sub(x: Real, y: Real) extends Real

/** Multiplication. */
case class Mul(x: Real, y: Real) extends Real

/** Division. */
case class Div(x: Real, y: Real) extends Real

/** Cut. */
case class Cut(x: Symbol, a: D.T, b: D.T, lower: Formula, upper: Formula) extends Real {
  override def toString: String = {
    s"Cut(${x},${Interval(a, b)},${lower},${upper})"
  }
}

/** Cut over R. */
case class CutR(x: Symbol, lower: Formula, upper: Formula,
                a: D.T = D.valueOf(-1), b: D.T = D.valueOf(1)) extends Real


/** Integration. */                
case class Integrate(x: Symbol, a: D.T, b: D.T, expr: Real) extends Real

/* Constructors for Formula: constants, logical operations, forall, exists. */

case class ConstFormula(b: Boolean) extends Formula
case class Less(x: Real, y: Real) extends Formula
case class And(x: Formula, y: Formula) extends Formula
case class Or(x: Formula, y: Formula) extends Formula
case class Forall(x: Symbol, a: D.T, b: D.T, phi: Formula) extends Formula
case class Exists(x: Symbol, a: D.T, b: D.T, phi: Formula) extends Formula

/**
 * Helper class that provides implicit conversions for integers to constants.
 */
object Real {
  import scala.language.implicitConversions

  implicit def int2BigDecimal(x: Int): D.T = {
    D.valueOf(x)
  }

  implicit def bigDecimal2Const(x: D.T): Const = {
    Const(x)
  }

  implicit def int2Const(x: Int): Const = {
    Const(D.valueOf(x))
  }
  
  implicit def str2Const(x: String): Const = {
    Const(D.valueOf(x, MathContext.UNLIMITED))
  }

  implicit def symbol2Var(name: Symbol): Var = {
    Var(name)
  }
}