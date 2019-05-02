/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.plane

import com.github.comius.reals.Interval
import com.github.comius.RoundingContext
import com.github.comius.reals.newton.ConstraintSet
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetNone
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetList
import com.github.comius.reals.newton.ConstraintSet.MoreThan
import com.github.comius.reals.newton.ConstraintSet.LessThan

case class ConstraintSet2D private (xi: Interval, yi: Interval, hull: List[Line]) {
  import ConstraintSet2D._

  def split(llines: List[Line]): ConstraintSet2D = {
    def filterDuplicate[A](l: List[A]): List[A] = {
      if (l.isEmpty) {
        l
      } else {
        val acc = List.newBuilder[A]
        for ((p1, p2) <- zipConsPairs(l))
          if (p1 != p2) acc += p1
        acc.result
      }
    }
    // go round and compute intersection
    def split(hull: List[Line], l: Line): (List[Line], List[Line]) = {
      if (hull.isEmpty) {
        (List(), List())
      } else {
        val acc1 = List.newBuilder[Line]
        val acc2 = List.newBuilder[Line]

        for (((l1, l2), l3) <- zipConsTriples(hull)) {

          val in1 = l.inside(l1, l2)
          if (in1) acc1 += l1 else acc2 += l1
          // TODO optimise
          if (in1 != l.inside(l2, l3)) {
            if (in1) acc1 += l2 else acc2 += l2
            acc1 += l
            acc2 += l.invert()
          }
        }
        (filterDuplicate(acc1.result), filterDuplicate(acc2.result))
      }
    }

    // compute hull
    var lhull = hull
    for (l <- llines) {
      val (lhull1, _) = split(lhull, l)
      lhull = lhull1
    }
    ConstraintSet2D(xi, yi, lhull)
  }

  def isIn(p: Point): Boolean = {
    if (hull.isEmpty) false else hull.forall(_.inside(p))
  }

  override def toString(): String = {
    hull.mkString("RegionPlot[{", " && ", s"},{x,${xi.d},${xi.u}},{y,${yi.d},${yi.u}}]")
  }

  def projectExists(r: RoundingContext): ConstraintSet = {
    //TODO optimise
    if (hull.isEmpty) {
      ConstraintSetNone(xi)
    } else {
      val ups = List.newBuilder[D.T]
      val downs = List.newBuilder[D.T]
      for ((l1, l2) <- zipConsPairs(hull)) {
        if (l1.dfyi.signum != l2.dfyi.signum) {
          downs += l1.intersection(l2, r.down, r.down).x
          ups += l1.intersection(l2, r.up, r.up).x
        }
      }
      ConstraintSetList(xi, List(MoreThan(ups.result.reduce(_.min(_))), LessThan(downs.result.reduce(_.max(_)))))
    }
  }

  def projectForall(r: RoundingContext): ConstraintSet = {
    val dups = List.newBuilder[D.T]
    val ddowns = List.newBuilder[D.T]
    val uups = List.newBuilder[D.T]
    val udowns = List.newBuilder[D.T]
    for (((l1, l2), l3) <- zipConsTriples(hull)) {
      if (l2.dfxi == D.ZERO) {
        if (l2.ym == yi.d) {
          dups += l1.intersection(l2, r.up, r.up).x
          ddowns += l1.intersection(l2, r.down, r.down).x
          dups += l2.intersection(l3, r.up, r.up).x
          ddowns += l2.intersection(l3, r.down, r.down).x
        } else if (l2.ym == yi.u) {
          uups += l1.intersection(l2, r.up, r.up).x
          udowns += l1.intersection(l2, r.down, r.down).x
          uups += l2.intersection(l3, r.up, r.up).x
          udowns += l2.intersection(l3, r.down, r.down).x
        }
      }

    }
    if (uups.result().isEmpty || dups.result.isEmpty || udowns.result.isEmpty || ddowns.result.isEmpty) {
      ConstraintSetNone(xi)
    } else {
      val d = ConstraintSetList(xi, List(MoreThan(dups.result.reduce(_.min(_))), LessThan(ddowns.result.reduce(_.max(_)))))
      val u = ConstraintSetList(xi, List(MoreThan(uups.result.reduce(_.min(_))), LessThan(udowns.result.reduce(_.max(_)))))
      d.intersection(u)
    }
  }
}

object ConstraintSet2D {
  def apply(xi: Interval, yi: Interval): ConstraintSet2D = {
    new ConstraintSet2D(xi, yi,
      List(
        Line(D.ZERO, D.ZERO, yi.d, D.ZERO, D.ONE),
        Line(D.ZERO, xi.u, D.ZERO, D.ONE.negate, D.ZERO),
        Line(D.ZERO, D.ZERO, yi.u, D.ZERO, D.ONE.negate),
        Line(D.ZERO, xi.d, D.ZERO, D.ONE, D.ZERO)))
  }

  private def zipConsPairs[A](l: List[A]): List[(A, A)] = l match {
    case x :: xs => l.zip(xs :+ x)
    case Nil     => Nil
  }

  private def zipConsTriples[A](l: List[A]): List[((A, A), A)] = l match {
    case x1 :: x2 :: xs => l.zip((x2 :: xs) :+ x1).zip(xs :+ x1 :+ x2)
    case _              => Nil
  }
}
