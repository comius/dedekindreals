/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals

import com.github.comius.RoundingContext

case class Context[T](roundingContext: RoundingContext, vars: Map[Symbol, T] = Map[Symbol, T]()) {
  def + (p: (Symbol, T)) = copy(vars = vars + p)
  
  def + (s: Symbol, v: T) = copy(vars = vars + ((s,v)))
 
  
  def mapValues[B](f: T => B): Context[B] =
    Context(roundingContext, vars.mapValues(f))
  
}