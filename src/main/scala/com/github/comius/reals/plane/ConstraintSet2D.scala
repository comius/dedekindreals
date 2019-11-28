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
import com.github.comius.reals.plane.ConstraintSet2D.ConvexHull

/**
 * An open subset of intervals xi and yi (in the plane) described by a list of convex hulls.
 *
 * @param xi interval in first axis
 * @param yi interval in second axis
 * @param hull list of convex hulls
 */
case class ConstraintSet2D private (xi: Interval, yi: Interval, hulls: List[ConvexHull]) {

  def split(constraints: List[Line]): ConstraintSet2D = {
    ConstraintSet2D(xi, yi, hulls.map(_.split(constraints)._1))
  }

  def projectExists(r: RoundingContext): ConstraintSet = {
    hulls.map(_.projectExists(xi, r)).reduceOption(_.union(_)).getOrElse(ConstraintSetNone(xi))
  }

  def projectForall(r: RoundingContext): ConstraintSet = {
    var initial = ConstraintSet2D(xi, yi)
    hulls.foreach { cs => initial = initial.minus(cs) }
    initial.projectExists(r).complement
  }

  // subtracts a single constraint set from this one
  def minus(cs: ConvexHull): ConstraintSet2D = {
    ConstraintSet2D(xi, yi, hulls.flatMap(_.minus(cs).map(ConvexHull(_))))
  }

  def union(c2: ConstraintSet2D): ConstraintSet2D = {
    // TODO don't duplicate polys
    ConstraintSet2D(xi, yi, hulls ++ c2.hulls)
  }

  def intersection(cs2: ConstraintSet2D): ConstraintSet2D = {
    // compute intersections
    ConstraintSet2D(xi, yi, (for {
      c1 <- this.hulls;
      c2 <- cs2.hulls
    } yield c1.intersection(c2)).toList.filter(_.constraints.nonEmpty))
  }

  def isIn(p: Point): Boolean = {
    hulls.exists(_.isIn(p))
  }
}

object ConstraintSet2D {

  def apply(xi: Interval, yi: Interval): ConstraintSet2D = {
    ConstraintSet2D(xi, yi, List(ConvexHull(List(
      Line(D.ZERO, D.ZERO, yi.d, D.ZERO, D.ONE),
      Line(D.ZERO, xi.u, D.ZERO, D.ONE.negate, D.ZERO),
      Line(D.ZERO, D.ZERO, yi.u, D.ZERO, D.ONE.negate),
      Line(D.ZERO, xi.d, D.ZERO, D.ONE, D.ZERO)))))
  }

  case class ConvexHull private (constraints: List[Line]) {

    def split(llines: List[Line]): (ConvexHull, List[List[Line]]) = {
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
            if (in1 > 0) acc1 += l1 else if (in1 < 0) acc2 += l1
            // TODO optimise, don't compute l.inside(li, li+1) twice
            if (in1 != l.inside(l2, l3)) {
              if (in1 > 0) acc1 += l2 else if (in1 < 0) acc2 += l2
              acc1 += l
              acc2 += l.invert()
            }
          }
          (filterDuplicate(acc1.result), filterDuplicate(acc2.result))
        }
      }

      // compute hull
      var lhull = constraints
      val restlist = List.newBuilder[List[Line]]
      for (l <- llines) {

        val (lhull1, rest) = split(lhull, l)
        lhull = lhull1
        if (rest.length > 2) restlist += rest
      }
      (ConvexHull(lhull), restlist.result)
    }

    def isIn(p: Point): Boolean = {
      if (constraints.isEmpty) false else constraints.forall(_.inside(p) > 0)
    }

    override def toString(): String = {
      constraints.mkString("RegionPlot[{", " && ", "},{x,-5,5},{y,-5,5}]")
    }

    def projectExists(xi: Interval, r: RoundingContext): ConstraintSet = {
      //TODO optimise - if could directly assign to min and max, no need to have lists here
      if (constraints.isEmpty) {
        ConstraintSetNone(xi)
      } else {
        val ups = List.newBuilder[D.T]
        val downs = List.newBuilder[D.T]
        for ((l1, l2) <- zipConsPairs(constraints)) {
          if (l1.dfyi.signum != l2.dfyi.signum) {
            downs += l1.intersection(l2, r.down, r.down).x
            ups += l1.intersection(l2, r.up, r.up).x
          }
        }
        val min = ups.result.reduce(_.min(_))
        val max = downs.result.reduce(_.max(_))
        if (min == max) ConstraintSetNone(xi) else ConstraintSetList(xi, List(MoreThan(min), LessThan(max)))
      }
    }

    // subtracts a single constraint set from this one
    def minus(cs: ConvexHull): List[List[Line]] = {
      split(cs.constraints)._2
    }

    def intersection(cs: ConvexHull): ConvexHull = {
      split(cs.constraints)._1
    }
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
