package distance

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import distance.Distance.Manhattan

import scala.concurrent.Future

// Note to self: not all dissimilarity measures are useful for implementing the k-d tree algorithm in FBF (1976)
// The dissimilarity measure must satisfy that the difference along a single axis is ALWAYS less or equal than
// the total dissimilarity. For example, Euclidean distance doesn't cut it, but Manhattan distance does.
case class Point(x: BigDecimal, y: BigDecimal) extends Manhattan[Point]{
  val id: UUID = UUID.randomUUID()

  override def canEqual(that: Any): Boolean = {
    that match{
      case other: Point => id.equals(other.id)
      case _ => false
    }
  }
}
case class StrPoint(x: String, y: String)

object Point{
  val seed = 1000
  val rnd = new scala.util.Random(seed)

  def random(xRange: Range, yRange: Range) = {
    val x = BigDecimal(rnd.nextDouble()*(xRange.end - xRange.start) + xRange.start)
    val y = BigDecimal(rnd.nextDouble()*(yRange.end - yRange.start) + yRange.start)
    Point(x, y)
  }
}

object Hello extends App {
  val query: Point = Point(0,100)
  val m: Int = 50
  val n: Int = 200000

  val (data, _) = time("Generate data"){
    (1 to n)
      .par
      .map{_=>Point.random(Range.inclusive(10, 2000), Range.inclusive(10, 2000))}
      .distinct
      .toList
  }



  val (tree, _) = Await.result(timeFuture("Tree growth"){KDTree.grow(data)}, 60 seconds)
  val (bruteMNN, bruteSearchTime) = time("Brute Force Search"){data
    .par
    .map{p=> (p, p.distance(query))}
    .toList
    .sortBy{case (_, d) => d}.take(m)
    .reverse
  }

  val (treeMNN, treeSearchTime) = time("Tree Search"){tree.nnSearch(m, query)}

  println(s"Tree Search Efficiency Gain: ${100*(bruteSearchTime - treeSearchTime)/bruteSearchTime}%")
  println(s"Neighborhood size: Tree: ${treeMNN.size}, Brute Force: ${bruteMNN.size}")
  println(s"m-Distance: Tree: ${treeMNN.head._2}, Brute Force: ${bruteMNN.head._2}")
  println(s"Farthest neighbor: Tree: ${treeMNN.head._1}, Brute Force: ${bruteMNN.head._1}")
  println(s"Neighbors Tree       : ${treeMNN.map(_._1.id)}")
  println(s"Neighbors Brute Force: ${bruteMNN.map(_._1.id)}")


  def time[R](name: String)(fn: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val result = fn
    val t1 = System.nanoTime()
    val lapsed: Double = (t1-t0)* 1.0 / 1E9
    println(s"Profiling $name: ${lapsed}s")
    (result, lapsed)
  }

  def timeFuture[R](name: String)(future: => Future[R]): Future[(R, Double)] = {
    val t0 = System.nanoTime()
    future.map{result=>
      val t1 = System.nanoTime()
      val lapsed: Double = (t1-t0)* 1.0 / 1E9
      println(s"Profiling $name: ${lapsed}s")
      (result, lapsed)
    }
  }

}
