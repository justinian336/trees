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

object Hello extends App {

  val data: List[Point] = List(
    (7,2),
    (5,4),
    (3,2),
    (4,7),
    (190,160),
    (8,1)
  ).map{case (a: Int, b: Int)=> (BigDecimal(a), BigDecimal(b))}
    .map(Point.tupled)

  val tree: Node[Point] = KDTree.grow(data).asInstanceOf[Node[Point]]

  println(tree.nnSearch(5, Point(3,3)))

}
