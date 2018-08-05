package distance

import java.util.UUID

import distance.Distance.Euclidean

case class Point(x: BigDecimal, y: BigDecimal) extends Euclidean[Point]{
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
  def random(xRange: Range, yRange: Range) = {
    val x = BigDecimal(math.random()*(xRange.end - xRange.start) + xRange.start)
    val y = BigDecimal(math.random()*(yRange.end - yRange.start) + yRange.start)
    Point(x, y)
  }
}

object Hello extends App {

  val (data, _) = time("Generate data"){
    (1 to 1000)
      .par
      .map{_=>Point.random(Range.inclusive(10, 2000), Range.inclusive(10, 2000))}
      .distinct
      .toList
  }

  val query: Point = Point(150,150)
  val m: Int = 1

  val (tree, growthTime) = time("Tree growth"){KDTree.grow(data).asInstanceOf[Node[Point]]}

  val (treeMNN, treeTime) = time("Tree Search"){tree.nnSearch(m, query)}

  val (bruteMNN, bruteTime) = time("Brute Force Search"){data
    .par
    .map{p=> (p, p.distance(query))}
    .toList
    .sortBy{case (_, d) => d}.take(m)
    .reverse
  }

  println("Nearest Neighbors:", treeMNN.head._2, bruteMNN.head._2)

  def time[R](name: String)(fn: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val result = fn
    val t1 = System.nanoTime()
    val lapsed: Double = (t1-t0)* 1.0 / 1E9
    println(s"Profiling $name: ${lapsed}s")
    (result, lapsed)
  }

}
