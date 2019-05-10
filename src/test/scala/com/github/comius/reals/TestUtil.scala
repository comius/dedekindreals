/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals

import org.scalacheck.util.Pretty
import org.scalacheck.Prop
import org.scalacheck.Prop.Result

/**
 * Testing utils missing in scalacheck. Testing all cases (not just random ones).
 */
object TestUtil {
  /**
   * A special forall that tests all values (not just random ones).
   *
   * Implemented in a form of property, because aggregating Prop-s with && throws stack exception.
   */
  def forall[A1](g1: TraversableOnce[A1])(f: A1 => Prop)(implicit pp1: A1 => Pretty): Prop = Prop {
    prms0 =>
      var r = Result(Prop.True)
      for (gv1 <- g1 if r.status == Prop.True) {
        r = r && f(gv1)(prms0)
        if (r.status != Prop.True) {
          r = r.addArg(Prop.Arg("ARG0", gv1, 0, gv1, pp1(gv1), pp1(gv1)))
        }
      }
      if (r.status == Prop.True) Result(Prop.Proof) else r
  }

  /**
   * A special forall that tests all values (not just random ones).
   *
   * Implemented in a form of property, because aggregating Prop-s with && throws stack exception.
   */
  def forall[A1, A2](g1: TraversableOnce[A1], g2: TraversableOnce[A2])(
    f: (A1, A2) => Prop)(implicit pp1: A1 => Pretty, pp2: A2 => Pretty): Prop = Prop {
    prms0 =>
      var r = Result(Prop.True)
      for (gv1 <- g1; gv2 <- g2 if r.status == Prop.True) {
        r = r && f(gv1, gv2)(prms0)
        if (r.status != Prop.True) {
          r = r.addArg(Prop.Arg("ARG1", gv2, 0, gv2, pp2(gv2), pp2(gv2)))
            .addArg(Prop.Arg("ARG0", gv1, 0, gv1, pp1(gv1), pp1(gv1)))
        }
      }
      if (r.status == Prop.True) Result(Prop.Proof) else r
  }
}
