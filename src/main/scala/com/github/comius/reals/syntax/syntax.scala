/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.syntax

import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.Interval
import java.math.MathContext

/* Our syntax is made of Reals and Formulas */

/**
 * Real number.
 *
 * Arithmetic and comparison operators are defined here to ease writing expressione. The operators just compose together
 * syntax.
 */
sealed trait Real {
  def +(other: Real): Real = Add(this, other) // scalastyle:ignore method.name
  def -(other: Real): Real = Sub(this, other) // scalastyle:ignore method.name
  def *(other: Real): Real = Mul(this, other) // scalastyle:ignore method.name
  def /(other: Real): Real = Div(this, other) // scalastyle:ignore method.name
  def <(other: Real): Less = Less(this, other) // scalastyle:ignore method.name
}

/**
 * Formulas.
 *
 * Logic operators are defined here to ease writing expressione. The operators just compose expressions together
 * into a syntax tree.
 */
sealed trait Formula {
  def &&(other: Formula): Formula = And(this, other) // scalastyle:ignore method.name
  def ||(other: Formula): Formula = Or(this, other) // scalastyle:ignore method.name
}

/* Constructors for Real: constants, variables arithmetic operations, Cut, integration */

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
case class Var(name: String) extends Real {
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
case class Cut(x: String, a: D.T, b: D.T, lower: Formula, upper: Formula) extends Real {
  override def toString: String = {
    s"Cut(${x},${Interval(a, b)},${lower},${upper})"
  }
}

/** Integration. */
case class Integrate(x: String, a: D.T, b: D.T, expr: Real) extends Real

/* Constructors for Formula: constants, logical operations, forall, exists. */

case class ConstFormula(b: Boolean) extends Formula
case class Less(x: Real, y: Real) extends Formula
case class And(x: Formula, y: Formula) extends Formula
case class Or(x: Formula, y: Formula) extends Formula
case class Forall(x: String, a: D.T, b: D.T, phi: Formula) extends Formula
case class Exists(x: String, a: D.T, b: D.T, phi: Formula) extends Formula

/**
 * Helper class that provides implicit conversions for integers to constants.
 */
object Real {
  import scala.language.implicitConversions

  implicit def int2BigDecimal(x: Int): D.T = {
    D.valueOf(x)
  }

  implicit def double2BigDecimal(x: Double): D.T = {
    D.valueOf(x)
  }

  implicit def bigDecimal2Const(x: D.T): Const = {
    Const(x)
  }

  implicit def int2Const(x: Int): Const = {
    Const(D.valueOf(x))
  }

  implicit def str2Var(name: String): Var = {
    Var(name)
  }
  
  def exists(x: String, a: D.T, b: D.T, phi: Real => Formula) = {
    Exists(x,a,b,phi(Var(x)))
  }
  
  def forall(x: String, a: D.T, b: D.T, phi: Real => Formula) = {
    Forall(x,a,b,phi(Var(x)))
  }
  
  def cut(x: String, a: D.T, b: D.T, lower: Real => Formula, upper: Real => Formula) = {
    Cut(x, a, b, lower(Var(x)), upper(Var(x)))
  }
  
  def cut(x: String, lower: Real => Formula, upper: Real => Formula) = {
    Cut(x, D.negInf, D.posInf, lower(Var(x)), upper(Var(x)))
  }
  
  def integrate(x: String, a: D.T, b: D.T, expr: Real => Real) = {
    Integrate(x, a, b, expr(Var(x)))
  }
}
