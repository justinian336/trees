package distance

import distance.Hello.data

sealed trait KDTree[T]{

  val upperBoundaries: List[Option[BigDecimal]]
  val lowerBoundaries: List[Option[BigDecimal]]

  def nnSearch(m: Int, target: Point, currentBest: List[(Point, BigDecimal)] = Nil): List[(Point, BigDecimal)]
  def traverse(point: Point): Leaf[T]
}

case class Leaf[T](point: Point, upperBoundaries: List[Option[BigDecimal]], lowerBoundaries: List[Option[BigDecimal]]) extends KDTree[T]{

  override def nnSearch(m: Int, target: Point, currentBest: List[(Point, BigDecimal)]): List[(Point, BigDecimal)] = {
    val distanceToTarget: BigDecimal = target.distance(point)
//   Take the m points with the shortest distances and then sort the list in descending order, so that the relevant neighbor is
//    in the head of the list.
    ((point, distanceToTarget)::currentBest).sortBy{case (_, dist) => dist}.take(m).reverse
  }

  override def traverse(point: Point): Leaf[T] = this
}

case class Node[T](axis: Int,
                   splitValue: BigDecimal,
                   upperBoundaries: List[Option[BigDecimal]],
                   lowerBoundaries: List[Option[BigDecimal]],
                   left: KDTree[T],
                   right: KDTree[T]) extends KDTree[T]{

  import KDTree._

  def nnSearch(m: Int, target: Point, currentBest: List[(Point, BigDecimal)] = Nil): List[(Point, BigDecimal)] = {

//  Search left first, then right
    if(sortByAxis(axis)(target) <= splitValue){

      val updatedBest = left.nnSearch(m, target, currentBest)
      if(
        !ballWithinBounds(target, left.upperBoundaries, left.lowerBoundaries, updatedBest) &&
        boundsOverlapBall(m, target, right.upperBoundaries, right.lowerBoundaries, updatedBest)
      ){
        right.nnSearch(m, target, updatedBest)
      } else updatedBest

//  Search right first, then right
    } else {

      val updatedBest = right.nnSearch(m, target, currentBest)
      if(
        !ballWithinBounds(target, right.upperBoundaries, right.lowerBoundaries, updatedBest) &&
          boundsOverlapBall(m, target, left.upperBoundaries, left.lowerBoundaries, updatedBest)
      ){
        left.nnSearch(m, target, updatedBest)
      } else updatedBest

    }
  }


  def traverse(point: Point): Leaf[T] = {
    if(lowerThanByAxis(point, axis, splitValue)) traverseOrReturn(point, left) else traverseOrReturn(point, right)
  }

  private def nodesList(): List[KDTree[T]] = {
    List(left, right)
  }

  private def lowerThanByAxis(point: Point, axis: Int, split: BigDecimal) = {
    if(axis == 0)
      point.x <= split
    else
      point.y <= split
  }

  private def traverseOrReturn[L <: KDTree[T]](point: Point, nextNode: L): Leaf[T] = {
    nextNode match{
      case leaf: Leaf[T] => leaf
      case node: Node[T] => node.traverse(point)
    }
  }

}

object KDTree {

  def grow(data: List[Point], axis: Int = getNextAxis(data), upperBoundaries: List[Option[BigDecimal]] = List(None, None), lowerBoundaries: List[Option[BigDecimal]] = List(None, None)): KDTree[Point] ={

    data match{
      case Nil => throw new Exception("No data to index.")
      case point::Nil => Leaf(point, upperBoundaries, lowerBoundaries)
      case l: List[Point] =>
        val split = median(l, axis)(mapperForAxis(axis))

        val leftData: List[Point] = data.filter(filterByAxis(axis, split, true))
        val rightData: List[Point] = data.filter(filterByAxis(axis, split, false))

        val leftUpperBoundaries = updateBoundary(upperBoundaries, split, axis)
        val rightLowerBoundaries = updateBoundary(lowerBoundaries, split, axis)

        Node(
          axis,
          split,
          upperBoundaries,
          lowerBoundaries,
          grow(leftData, getNextAxis(leftData), leftUpperBoundaries, lowerBoundaries),
          grow(rightData, getNextAxis(rightData), upperBoundaries, rightLowerBoundaries)
        )
    }
  }

  def median(data: List[Point], axis: Int)(mapper: (Point) => BigDecimal): BigDecimal = {
    val mapped = data.sortBy(sortByAxis(axis)).par.map(mapper)
    if(mapped.length % 2 == 0){
      mapped(mapped.size/2 - 1) + (mapped(mapped.size - mapped.size/2) - mapped(mapped.size/2 - 1))/2.0
    } else{
      mapped.drop(mapped.size/2).head
    }
  }

  private def mapperForAxis(axis: Int): (Point) => BigDecimal = {
    if(axis == 0) (p: Point) => p.x
    else (p: Point) => p.y
  }

  private def filterByAxis(currentAxis: Int, split: BigDecimal, left: Boolean): (Point) => Boolean = {
    if(currentAxis == 0) {
      (p: Point) =>
        if(left) p.x <= split
        else p.x > split
    } else {
      (p: Point) =>
        if(left) p.y <= split
        else p.y > split
    }
  }

  private def updateBoundary(boundary: List[Option[BigDecimal]], splitValue: BigDecimal, dimension: Int): List[Option[BigDecimal]] = {
    if(dimension == 0){
      Some(splitValue)::boundary.tail
    } else{
      boundary.head::List(Some(splitValue))
    }
  }

  def sortByAxis(currentAxis: Int) = {
    if(currentAxis == 0) (p: Point) => p.x
    else (p: Point) => p.y
  }

  def ballWithinBounds(query: Point,
                               upperBound: List[Option[BigDecimal]],
                               lowerBound: List[Option[BigDecimal]],
                               mNN: List[(Point, BigDecimal)]): Boolean = {
    val flatUpperBound = upperBound.flatten
    val flatLowerBound = lowerBound.flatten

    if(flatUpperBound.size == 2 && flatLowerBound.size == 2){

      // Try to terminate early
      if(query.x.coordinateDistance(flatLowerBound.head) <= mNN.head._2 || query.x.coordinateDistance(flatUpperBound.head) <= mNN.head._2) true
      else query.y.coordinateDistance(flatLowerBound.tail.head) <= mNN.head._2 || query.y.coordinateDistance(flatUpperBound.tail.head) <= mNN.head._2

    } else false
  }

  def boundsOverlapBall(m: Int,
                        query: Point,
                        upperBound: List[Option[BigDecimal]],
                        lowerBound: List[Option[BigDecimal]],
                        mNN: List[(Point, BigDecimal)]): Boolean = {

    if(mNN.size < m) true
    else {

      val sum = BigDecimal(0) + {
        if(lowerBound.head.exists(_ > query.x)) lowerBound.head.map(query.x.coordinateDistance).getOrElse(0)
        else if(upperBound.head.exists( _ < query.x)) upperBound.head.map(query.x.coordinateDistance).getOrElse(0)
        else 0
      } + {
        if(lowerBound.tail.head.exists(_ > query.y)) lowerBound.tail.head.map(query.y.coordinateDistance).getOrElse(0)
        else if(upperBound.tail.head.exists(_ < query.y)) upperBound.tail.head.map(query.y.coordinateDistance).getOrElse(0)
        else 0
      }
      scala.math.pow(sum.toDouble, (1/BigDecimal(2)).toDouble) > mNN.head._2
    }
  }

  def getNextAxis(data: List[Point]): Int = {
    // Choose the dimension with the highest variance
    val varX = data.maxBy(_.x).x - data.minBy(_.x).x
    val varY = data.maxBy(_.y).y - data.minBy(_.y).y

    // Choose a pivot point along this dimension by obtaining the median
      if(varX >= varY) 0 else 1
  }

  implicit class Coordinate(t1: BigDecimal){

    def coordinateDistance(other: BigDecimal): BigDecimal = {
      (t1 - other).pow(2)
    }

  }

}
