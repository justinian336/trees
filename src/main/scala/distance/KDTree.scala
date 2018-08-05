package distance

import distance.Hello.data

import scala.concurrent.{ExecutionContext, Future}

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
      val ballOutsideBounds = !ballWithinBounds(m, target, left.upperBoundaries, left.lowerBoundaries, updatedBest)
//      val boundsAndBallOverlap = boundsOverlapBall(m, target, right.upperBoundaries, right.lowerBoundaries, updatedBest)
//      if(ballOutsideBounds && boundsAndBallOverlap){
      if(ballOutsideBounds){
        right.nnSearch(m, target, updatedBest)
      } else updatedBest

//  Search right first, then right
    } else {

      val updatedBest = right.nnSearch(m, target, currentBest)
      val ballOutsideBounds = !ballWithinBounds(m, target, right.upperBoundaries, right.lowerBoundaries, updatedBest)
//      val boundsAndBallOverlap = boundsOverlapBall(m, target, left.upperBoundaries, left.lowerBoundaries, updatedBest)
//      if(ballOutsideBounds && boundsAndBallOverlap){
      if(ballOutsideBounds){
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

  def grow(data: List[Point],
           axis: Int = getNextAxis(data),
           upperBoundaries: List[Option[BigDecimal]] = List(None, None),
           lowerBoundaries: List[Option[BigDecimal]] = List(None, None))
          (implicit ec: ExecutionContext): Future[KDTree[Point]] ={

    data match{
      case Nil => throw new Exception("No data to index.")
      case point::Nil =>
        Future.successful(
        Leaf(point, upperBoundaries, lowerBoundaries)
      )
      case l: List[Point] =>
        val split = median(l, axis)(mapperForAxis(axis))

        val leftData: List[Point] = data.filter(filterByAxis(axis, split, true))
        val rightData: List[Point] = data.filter(filterByAxis(axis, split, false))

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

  def median(data: List[Point], axis: Int)(mapper: Point => BigDecimal): BigDecimal = {
    val mapped = data.sortBy(sortByAxis(axis)).par.map(mapper)
    if(mapped.length % 2 == 0){
      mapped(mapped.size/2 - 1) + (mapped(mapped.size - mapped.size/2) - mapped(mapped.size/2 - 1))/2.0
    } else{
      mapped.drop(mapped.size/2).head
    }
  }

  private def mapperForAxis(axis: Int): Point => BigDecimal = {
    if(axis == 0) (p: Point) => p.x
    else (p: Point) => p.y
  }

  private def filterByAxis(currentAxis: Int, split: BigDecimal, left: Boolean): Point => Boolean = {
    if(currentAxis == 0) {
      p: Point =>
        if(left) p.x <= split
        else p.x > split
    } else {
      p: Point =>
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

  def ballWithinBounds(m: Int,
                       query: Point,
                       upperBound: List[Option[BigDecimal]],
                       lowerBound: List[Option[BigDecimal]],
                       mNN: List[(Point, BigDecimal)]): Boolean = {

    mNN match{
      case _: List[(Point, BigDecimal)] if mNN.size < m => false
      case _: List[(Point, BigDecimal)] =>

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

  def boundsOverlapBall(m: Int,
                        query: Point,
                        upperBound: List[Option[BigDecimal]],
                        lowerBound: List[Option[BigDecimal]],
                        mNN: List[(Point, BigDecimal)]): Boolean = {

    if(mNN.size < m) true
    else {



      val sum: Option[BigDecimal] =  {
        if(lowerBound.head.exists(_ > query.x)) lowerBound.head.map(query.x.coordinateDistance)
        else if(upperBound.head.exists( _ < query.x)) upperBound.head.map(query.x.coordinateDistance)
        else None
      }.flatMap{cumSum =>
        if(lowerBound.tail.head.exists(_ > query.y)) lowerBound.tail.head.map(query.y.coordinateDistance).map(_ + cumSum)
        else if(upperBound.tail.head.exists(_ < query.y)) upperBound.tail.head.map(query.y.coordinateDistance).map(_ + cumSum)
        else None
      }

      sum.forall{_ > mNN.head._2}
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
      (t1 - other).abs
    }

  }

}
