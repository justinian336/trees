package tree

import distance.Distance.{Euclidean, Manhattan}
import generic.GenericUtils.{reducerPoly, sqDiffMap, absDiffMap}
import shapeless.ops.hlist.{LeftFolder, Mapper, Zip}
import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.tuple.ToList

import scala.concurrent.{ExecutionContext, Future}

sealed trait KDTree[T <: Manhattan[T]]{

  val upperBoundaries: List[Option[BigDecimal]]
  val lowerBoundaries: List[Option[BigDecimal]]

  def nnSearch[H<: HList, L<: HList](m: Int, target: T, currentBest: List[(T, BigDecimal)] = Nil)(
    implicit gen: Generic.Aux[T, H],
    zipper: Zip.Aux[H::H::HNil, L],
    diffMapper: Mapper.Aux[absDiffMap.type, L, H],
    folder: LeftFolder.Aux[H, BigDecimal, reducerPoly.type, BigDecimal]
  ): List[(T, BigDecimal)]
//  def traverse(point: T): Leaf[T]
}

case class Leaf[T <: Manhattan[T]](point: T,
                                   upperBoundaries: List[Option[BigDecimal]],
                                   lowerBoundaries: List[Option[BigDecimal]]) extends KDTree[T]{

  override def nnSearch[H<: HList, L<: HList](m: Int, target: T, currentBest: List[(T, BigDecimal)] = Nil)(
    implicit gen: Generic.Aux[T, H],
    zipper: Zip.Aux[H::H::HNil, L],
    diffMapper: Mapper.Aux[absDiffMap.type, L, H],
    folder: LeftFolder.Aux[H, BigDecimal, reducerPoly.type, BigDecimal]
  ): List[(T, BigDecimal)] = {
    val distanceToTarget: BigDecimal = target.distance(point)
//   Take the m points with the shortest distances and then sort the list in descending order, so that the relevant neighbor is
//    in the head of the list.
//    println(s"visiting")
    ((point, distanceToTarget)::currentBest).sortBy{case (_, dist) => dist}.take(m).reverse
  }

//  override def traverse(point: T): Leaf[T] = this
}

case class Node[T <: Manhattan[T]](axis: Int,
                   splitValue: BigDecimal,
                   upperBoundaries: List[Option[BigDecimal]],
                   lowerBoundaries: List[Option[BigDecimal]],
                   left: KDTree[T],
                   right: KDTree[T])(
    implicit toList: ToList.Aux[T, BigDecimal, List[BigDecimal]]
) extends KDTree[T]{

  import KDTree._

  def nnSearch[H<: HList, L<: HList](m: Int,
               target: T,
               currentBest: List[(T, BigDecimal)] = Nil)(
    implicit gen: Generic.Aux[T, H],
    zipper: Zip.Aux[H::H::HNil, L],
    diffMapper: Mapper.Aux[absDiffMap.type, L, H],
    folder: LeftFolder.Aux[H, BigDecimal, reducerPoly.type, BigDecimal]
  ): List[(T, BigDecimal)] = {

//  Search left first, then right
    if({
      val asList = target.toList
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

  def grow[T <: Manhattan[T]](data: List[T],
                              i: Int = 0,
           upperBoundaries: List[Option[BigDecimal]] = Nil,
           lowerBoundaries: List[Option[BigDecimal]] = Nil)
          (
            implicit ec: ExecutionContext,
           toList: ToList.Aux[T, BigDecimal, List[BigDecimal]]
          ): Future[KDTree[T]] ={

    val nextAxis: Int = getNextAxis(data, i)

    val (filledLowerBoundaries, filledUpperBoundaries) = {
      if(upperBoundaries == Nil && lowerBoundaries == Nil){
        (
          data.head.toList.map{_=> None},
          data.head.toList.map{_=> None}
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
          val listRepr: List[BigDecimal] = p.toList
          listRepr(nextAxis)
        }

        val leftData: List[T] = data.filter{p:T =>
          val listRepr: List[BigDecimal] = p.toList
          listRepr(nextAxis) <= split
        }

        val rightData: List[T] = data.filter{p:T =>
          val listRepr: List[BigDecimal] = p.toList
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

  def median[T <: Manhattan[T]](data: List[T], axis: Int)(mapper: T => BigDecimal)(
    implicit toList: ToList.Aux[T, BigDecimal, List[BigDecimal]]
  ): BigDecimal = {
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

  def sortByAxis[T <: Manhattan[T]](currentAxis: Int)(implicit toList: ToList.Aux[T, BigDecimal, List[BigDecimal]]): T => BigDecimal = { p: T =>
    val asList = p.toList
    asList(currentAxis)
  }

  def ballWithinBounds[T <: Manhattan[T]](
                       m: Int,
                       query: T,
                       upperBound: List[Option[BigDecimal]],
                       lowerBound: List[Option[BigDecimal]],
                       mNN: List[(T, BigDecimal)])(
    implicit toList: ToList.Aux[T, BigDecimal, List[BigDecimal]]
  ): Boolean = {

    mNN match{
      case _: List[(T, BigDecimal)] if mNN.size < m => false
      case _: List[(T, BigDecimal)] =>

        val mDistance = mNN.head._2

        val zipped = lowerBound.zip(upperBound).zipWithIndex

        val returns = zipped.foldLeft(false){case (res, ((l, u), dim))=>
          val asList: List[BigDecimal] = query.toList
          val value: BigDecimal = asList(dim)

            res ||
              l.exists(value.coordinateDistance(_) <= mDistance) ||
              u.exists(value.coordinateDistance(_) <= mDistance)

        }

        !returns
    }
  }

  def boundsOverlapBall[T <: Manhattan[T]](
                        m: Int,
                        query: T,
                        upperBound: List[Option[BigDecimal]],
                        lowerBound: List[Option[BigDecimal]],
                        mNN: List[(T, BigDecimal)])(
    implicit toList: ToList.Aux[T, BigDecimal, List[BigDecimal]]
  ): Boolean = {

    if(mNN.size < m) true
    else {

        query.toList.zipWithIndex.foldLeft(BigDecimal(0)){case (cumSum, (v, i))=>
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
//        if(lowerBound(i).exists(_ > v))
//          lowerBound(i).map(v.coordinateDistance(_) + cumSum).getOrElse(cumSum)
//        else if(upperBound(i).exists( _ < v)) upperBound(i).map(v.coordinateDistance(_) + cumSum).getOrElse(cumSum)
//        else 0
      } > mNN.head._2

//
//
//
//      val sum: Option[BigDecimal] =  {
//        if(lowerBound.head.exists(_ > query.x)) lowerBound.head.map(query.x.coordinateDistance)
//        else if(upperBound.head.exists( _ < query.x)) upperBound.head.map(query.x.coordinateDistance)
//        else None
//      }.flatMap{cumSum =>
//        if(lowerBound.tail.head.exists(_ > query.y)) lowerBound.tail.head.map(query.y.coordinateDistance).map(_ + cumSum)
//        else if(upperBound.tail.head.exists(_ < query.y)) upperBound.tail.head.map(query.y.coordinateDistance).map(_ + cumSum)
//        else None
//      }
//
//      sum.forall{_ > mNN.head._2}
    }
  }

  def getNextAxis[T <: Manhattan[T]](data: List[T], current: Int)(
    implicit toList: ToList.Aux[T, BigDecimal, List[BigDecimal]]
  ): Int = {

    val maxDims = data.head.toList.size - 1
    if(current == maxDims ) 0 else current + 1

//    val (minVals: List[BigDecimal], maxVals: List[BigDecimal]) = data
//      .tail
//      .foldLeft[(List[BigDecimal], List[BigDecimal])]((data.head.toList, data.head.toList)){case (cumulative, next)=>
//      (
//        cumulative._1.zip(next.toList).map{case (x0, x1)=> x0.min(x1)},
//        cumulative._2.zip(next.toList).map{case (x0, x1)=> x0.max(x1)}
//      )
//    }
//
//    minVals
//      .zip(maxVals)
//      .map{case (x, y)=> (x-y).abs}
//      .zipWithIndex
//      .maxBy{case (value, _)=> value}._2
  }

  implicit class Coordinate(t1: BigDecimal){

    def coordinateDistance(other: BigDecimal): BigDecimal = {
      (t1 - other).abs
    }

  }

}
