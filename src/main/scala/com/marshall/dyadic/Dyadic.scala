package com.marshall.dyadic

import java.math.MathContext

trait Dyadic[T] {
  def add(b: T, c: MathContext): T
  def subtract(b: T, c: MathContext): T
  def multiply(b: T, c: MathContext): T
  def divide(b: T, c: MathContext): T

  def isPosInf(): Boolean
  def isNegInf(): Boolean

}

