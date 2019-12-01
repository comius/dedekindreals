/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals

import com.github.comius.RoundingContext

final case class Context[T](roundingContext: RoundingContext, vars: Map[String, T] = Map[String, T]()) {
  def +(p: (String, T)): Context[T] = copy(vars = vars + p) // scalastyle:ignore method.name

  def +(s: String, v: T): Context[T] = copy(vars = vars + ((s, v))) // scalastyle:ignore method.name

  def mapValues[B](f: T => B): Context[B] =
    Context(roundingContext, vars.mapValues(f).toMap)

}
