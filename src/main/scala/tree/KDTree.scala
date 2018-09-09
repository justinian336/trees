package tree

import distance.Distance

import scala.concurrent.{ExecutionContext, Future}

sealed trait KDTree[T <: Distance[T]]{

  val upperBoundaries: List[Option[BigDecimal]]
  val lowerBoundaries: List[Option[BigDecimal]]

  def nnSearch(m: Int, target: T, currentBest: List[(T, BigDecimal)] = Nil): List[(T, BigDecimal)]
//  def traverse(point: T): Leaf[T]
}

case class Leaf[T <: Distance[T]](point: T,
                                           upperBoundaries: List[Option[BigDecimal]],
                                           lowerBoundaries: List[Option[BigDecimal]]) extends KDTree[T]{

  override def nnSearch(m: Int, target: T, currentBest: List[(T, BigDecimal)] = Nil): List[(T, BigDecimal)] = {
    val distanceToTarget: BigDecimal = target.distance(point)
//   Take the m points with the shortest distances and then sort the list in descending order, so that the relevant neighbor is
//    in the head of the list.
//    println(s"visiting")
    ((point, distanceToTarget)::currentBest).sortBy{case (_, dist) => dist}.take(m).reverse
  }

//  override def traverse(point: T): Leaf[T] = this
}

case class Node[T <: Distance[T]](axis: Int,
                                           splitValue: BigDecimal,
                                           upperBoundaries: List[Option[BigDecimal]],
                                           lowerBoundaries: List[Option[BigDecimal]],
                                           left: KDTree[T],
                                           right: KDTree[T]) extends KDTree[T]{

  import KDTree._

  def nnSearch(m: Int,
               target: T,
               currentBest: List[(T, BigDecimal)] = Nil): List[(T, BigDecimal)] = {

//  Search left first, then right
    if({
      val asList = target.features
      asList(axis) <= splitValue
    } ){

      val updatedBest = left.nnSearch(m, target, currentBest)

      if(ballWithinBounds(m, target, left.upperBoundaries, left.lowerBoundaries, updatedBest)){
        updatedBest
      } else
      if (boundsOverlapBall(m, target, upperBoundaries, lowerBoundaries, updatedBest)){
        right.nnSearch(m, target, updatedBest)
      } else updatedBest

//  Search right first, then left
    } else {

      val updatedBest = right.nnSearch(m, target, currentBest)

      if(ballWithinBounds(m, target, right.upperBoundaries, right.lowerBoundaries, updatedBest)){
        updatedBest
      } else
      if (boundsOverlapBall(m, target, upperBoundaries, lowerBoundaries, updatedBest)){
        left.nnSearch(m, target, updatedBest)
      } else updatedBest
    }
  }


//  def traverse[L <: HList, N <: Nat](point: T)
//                        (implicit at: hlist.At.Aux[L, Nat, BigDecimal]): Leaf[T] = {
//
//    val repr = point.repr()
//    val nextNode = if(at(repr, axis) <= splitValue) left else right
//    nextNode match{
//      case leaf: Leaf[T] => leaf
//      case node: Node[T] => node.traverse(point)(at)
//    }
//  }
}

object KDTree {

  def grow[T <: Distance[T]](data: List[T],
                                      i: Int = 0,
                                      upperBoundaries: List[Option[BigDecimal]] = Nil,
                                      lowerBoundaries: List[Option[BigDecimal]] = Nil)
                                     (
            implicit ec: ExecutionContext
          ): Future[KDTree[T]] ={

    val nextAxis: Int = getNextAxis(data, i)

    val (filledLowerBoundaries, filledUpperBoundaries) = {
      if(upperBoundaries == Nil && lowerBoundaries == Nil){
        (
          data.head.features.map{_=> None},
          data.head.features.map{_=> None}
        )
      }
      else (lowerBoundaries, upperBoundaries)
    }

    data match{
      case Nil => throw new Exception("No data to index.")
      case l:List[T] if l.size == 1 =>
        Future.successful(
          Leaf(l.head, filledUpperBoundaries, filledLowerBoundaries)
      )
      case l: List[T] =>
        val split = median(l, nextAxis){p: T =>
          val listRepr: List[BigDecimal] = p.features
          listRepr(nextAxis)
        }

        val leftData: List[T] = data.filter{p:T =>
          val listRepr: List[BigDecimal] = p.features
          listRepr(nextAxis) <= split
        }

        val rightData: List[T] = data.filter{p:T =>
          val listRepr: List[BigDecimal] = p.features
          listRepr(nextAxis) > split
        }

        val leftUpperBoundaries = updateBoundary(filledUpperBoundaries, split, nextAxis)
        val rightLowerBoundaries = updateBoundary(filledLowerBoundaries, split, nextAxis)

        val leftSon = grow(leftData, nextAxis, leftUpperBoundaries, filledLowerBoundaries)
        val rightSon = grow(rightData, nextAxis, filledUpperBoundaries, rightLowerBoundaries)

        for{
          left <- leftSon
          right <- rightSon
        } yield {
          Node(
            nextAxis,
            split,
            filledUpperBoundaries,
            filledLowerBoundaries,
            left,
            right
          )
        }
    }
  }

  def median[T <: Distance[T]](data: List[T], axis: Int)(mapper: T => BigDecimal): BigDecimal = {
    val mapped = data.sortBy(sortByAxis(axis)).par.map(mapper)
    if(mapped.length % 2 == 0){
      mapped(mapped.size/2 - 1) + (mapped(mapped.size - mapped.size/2) - mapped(mapped.size/2 - 1))/2.0
    } else{
      mapped.drop(mapped.size/2).head
    }
  }

  private def updateBoundary(boundary: List[Option[BigDecimal]], splitValue: BigDecimal, dimension: Int): List[Option[BigDecimal]] = {
    boundary.updated(dimension, Some(splitValue))
  }

  def sortByAxis[T <: Distance[T]](currentAxis: Int): T => BigDecimal = { p: T =>
    val asList = p.features
    asList(currentAxis)
  }

  def ballWithinBounds[T <: Distance[T]](
                       m: Int,
                       query: T,
                       upperBound: List[Option[BigDecimal]],
                       lowerBound: List[Option[BigDecimal]],
                       mNN: List[(T, BigDecimal)]): Boolean = {

    mNN match{
      case _: List[(T, BigDecimal)] if mNN.size < m => false
      case _: List[(T, BigDecimal)] =>

        val mDistance = mNN.head._2

        val zipped = lowerBound.zip(upperBound).zipWithIndex

        val returns = zipped.foldLeft(false){case (res, ((l, u), dim))=>
          val asList: List[BigDecimal] = query.features
          val value: BigDecimal = asList(dim)

            res ||
              l.exists(value.coordinateDistance(_) <= mDistance) ||
              u.exists(value.coordinateDistance(_) <= mDistance)

        }

        !returns
    }
  }

  def boundsOverlapBall[T <: Distance[T]](
                        m: Int,
                        query: T,
                        upperBound: List[Option[BigDecimal]],
                        lowerBound: List[Option[BigDecimal]],
                        mNN: List[(T, BigDecimal)]): Boolean = {

    if(mNN.size < m) true
    else {

        query.features.zipWithIndex.foldLeft(BigDecimal(0)){case (cumSum, (v, i))=>
          (lowerBound(i), upperBound(i)) match {
            case (Some(l), _) if v <= l =>
              val coordDist = cumSum + v.coordinateDistance(l)
              if(coordDist > mNN.head._2) return true
              else coordDist

            case (_, Some(u)) if v > u=>
              val coordDist = cumSum + v.coordinateDistance(u)
              if(coordDist > mNN.head._2) return true
              else coordDist
            case _ => return true
          }
      } > mNN.head._2

    }
  }

  def getNextAxis[T <: Distance[T]](data: List[T], current: Int): Int = {

    val maxDims = data.head.features.size - 1
    if(current == maxDims ) 0 else current + 1
  }

  implicit class Coordinate(t1: BigDecimal){

    def coordinateDistance(other: BigDecimal): BigDecimal = {
      (t1 - other).abs
    }

  }

}
