package tree

import distance.Distance.Euclidean
import distance.Point
import shapeless.{HList, Nat, the}
import shapeless.ops.hlist
import shapeless.ops.hlist.At

import scala.concurrent.{ExecutionContext, Future}

sealed trait KDTree[T <: Euclidean[T]]{

  val upperBoundaries: List[Option[BigDecimal]]
  val lowerBoundaries: List[Option[BigDecimal]]

  def nnSearch(m: Int, target: T, currentBest: List[(T, BigDecimal)] = Nil): List[(T, BigDecimal)]
  def traverse(point: T): Leaf[T]
}

case class Leaf[T <: Euclidean[T]](point: T,
                                   upperBoundaries: List[Option[BigDecimal]],
                                   lowerBoundaries: List[Option[BigDecimal]]) extends KDTree[T]{

  override def nnSearch(m: Int, target: T, currentBest: List[(T, BigDecimal)]): List[(T, BigDecimal)] = {
    val distanceToTarget: BigDecimal = target.distance(point)
//   Take the m points with the shortest distances and then sort the list in descending order, so that the relevant neighbor is
//    in the head of the list.
    ((point, distanceToTarget)::currentBest).sortBy{case (_, dist) => dist}.take(m).reverse
  }

  override def traverse(point: T): Leaf[T] = this
}

case class Node[T <: Euclidean[T]](axis: Nat,
                   splitValue: BigDecimal,
                   upperBoundaries: List[Option[BigDecimal]],
                   lowerBoundaries: List[Option[BigDecimal]],
                   left: KDTree[T],
                   right: KDTree[T]) extends KDTree[T]{

  import KDTree._

  def nnSearch(m: Int, target: T, currentBest: List[(T, BigDecimal)] = Nil): List[(T, BigDecimal)] = {

//  Search left first, then right
    if(sortByAxis(axis)(target) <= splitValue){

      val updatedBest = left.nnSearch(m, target, currentBest)

      if(ballWithinBounds(m, target, left.upperBoundaries, left.lowerBoundaries, updatedBest)){
        updatedBest
      } else {
        right.nnSearch(m, target, updatedBest)
      }

//  Search right first, then right
    } else {

      val updatedBest = right.nnSearch(m, target, currentBest)

      if(ballWithinBounds(m, target, left.upperBoundaries, left.lowerBoundaries, updatedBest)){
        updatedBest
      } else {
        left.nnSearch(m, target, updatedBest)
      }

    }
  }


  def traverse[L <: HList, N <: Nat](point: T)
                        (implicit at: hlist.At.Aux[L, Nat, BigDecimal]): Leaf[T] = {

    val repr: L = point.repr()
    if(lowerThanByAxis(repr, axis, splitValue)) traverseOrReturn(point, left) else traverseOrReturn(point, right)
  }

  private def lowerThanByAxis[N <: Nat](pointRepr: HList, axis: N, split: BigDecimal)
                                           (implicit at: hlist.At.Aux[L, Nat, BigDecimal]): Boolean = {

    at(pointRepr, axis) <= split

  }

  private def traverseOrReturn[L <: KDTree[T]](point: T, nextNode: L): Leaf[T] = {
    nextNode match{
      case leaf: Leaf[T] => leaf
      case node: Node[T] => node.traverse(point)
    }
  }

}

object KDTree {

  def grow[T <: Euclidean[T], N <: Nat](data: List[T],
           axis: Option[N] = None,
           upperBoundaries: List[Option[BigDecimal]] = List(None, None),
           lowerBoundaries: List[Option[BigDecimal]] = List(None, None))
          (implicit ec: ExecutionContext): Future[KDTree[T]] ={

    val nextAxis = getNextAxis()
//    TODO:Use the nextAxis value above for the expressions below instead of simply axis

    data match{
      case Nil => throw new Exception("No data to index.")
      case point::Nil =>
        Future.successful(
        Leaf(point, upperBoundaries, lowerBoundaries)
      )
      case l: List[T] =>
        val split = median(l, axis)(mapperForAxis(axis))

        val leftData: List[T] = data.filter(filterByAxis(axis, split, true))
        val rightData: List[T] = data.filter(filterByAxis(axis, split, false))

        val leftUpperBoundaries = updateBoundary(upperBoundaries, split, axis)
        val rightLowerBoundaries = updateBoundary(lowerBoundaries, split, axis)

        val leftSon = grow(leftData, getNextAxis(leftData), leftUpperBoundaries, lowerBoundaries)
        val rightSon = grow(rightData, getNextAxis(rightData), upperBoundaries, rightLowerBoundaries)

        for{
          left <- leftSon
          right <- rightSon
        } yield {
          Node(
            axis,
            split,
            upperBoundaries,
            lowerBoundaries,
            left,
            right
          )
        }
    }
  }

  def median[T <: Euclidean[T]](data: List[T], axis: Nat)(mapper: T => BigDecimal): BigDecimal = {
    val mapped = data.sortBy(sortByAxis(axis)).par.map(mapper)
    if(mapped.length % 2 == 0){
      mapped(mapped.size/2 - 1) + (mapped(mapped.size - mapped.size/2) - mapped(mapped.size/2 - 1))/2.0
    } else{
      mapped.drop(mapped.size/2).head
    }
  }

  private def mapperForAxis[T <: Euclidean[T], N<: Nat](axis: N)
                                                       (implicit at: At.Aux[T, N, BigDecimal]): T => BigDecimal = {
    p: T => at(p, axis)
  }

  private def filterByAxis[T <: Euclidean[T], N <: Nat](currentAxis: N, split: BigDecimal, left: Boolean)
                                    (implicit at: At.Aux[T, N, BigDecimal]): T => Boolean = {
    p: T =>
      val coordinate = at(p, currentAxis)
      if(left) coordinate <= split
      else coordinate > split
  }

  private def updateBoundary(boundary: List[Option[BigDecimal]], splitValue: BigDecimal, dimension: Int): List[Option[BigDecimal]] = {
    if(dimension == 0){
      Some(splitValue)::boundary.tail
    } else{
      boundary.head::List(Some(splitValue))
    }
  }

  def sortByAxis[T, N<:Nat](currentAxis: Nat)(
                                           implicit at: At.Aux[T, N, BigDecimal]
  ) = {
    p: T => at(p, currentAxis)

  }

  def ballWithinBounds[T <: Euclidean[T]](m: Int,
                       query: T,
                       upperBound: List[Option[BigDecimal]],
                       lowerBound: List[Option[BigDecimal]],
                       mNN: List[(T, BigDecimal)]): Boolean = {

    mNN match{
      case _: List[(T, BigDecimal)] if mNN.size < m => false
      case _: List[(T, BigDecimal)] =>

        val mDistance = mNN.head._2

        if(
          lowerBound.head.exists(query.x.coordinateDistance(_) <= mDistance) ||
            upperBound.head.exists(query.x.coordinateDistance(_) <= mDistance)) false
        else if(
          lowerBound.tail.head.exists(query.y.coordinateDistance(_) <= mDistance) ||
            upperBound.tail.head.exists(query.y.coordinateDistance(_) <= mDistance) ) false
        else true
    }
  }

  def getNextAxis[T <: Euclidean[T]](data: List[T]): Int = {
    // Choose the dimension with the highest variance
    val varX = data.maxBy(_.x).x - data.minBy(_.x).x
    val varY = data.maxBy(_.y).y - data.minBy(_.y).y

    // Choose a pivot point along this dimension by obtaining the median
      if(varX >= varY) 0 else 1
  }

  implicit class Coordinate(t1: BigDecimal){

    def coordinateDistance(other: BigDecimal): BigDecimal = {
      (t1 - other).abs
    }

  }

}
