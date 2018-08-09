package distance

import java.util.UUID

import distance.Distance.{Euclidean, Manhattan}
import shapeless.ops.hlist.{LeftFolder, Mapper, Zip, ZipConst}
import shapeless.{::, Generic, HList, HNil, Nat, Poly1, Poly2}

import scala.math.{acos, cos, sin}

object Distance {
  object diffMap extends Poly1{
    type ZippedType = (BigDecimal, Int)
    implicit def atBigDecimal: Case.Aux[((BigDecimal, Int), BigDecimal), BigDecimal] = at{case ((x, k), y) =>
      (x - y).abs.pow(k)
    }
  }

  object sqDiffMap extends Poly1{
    type ZippedType = (BigDecimal, Int)
    implicit def atBigDecimal: Case.Aux[(BigDecimal, BigDecimal), BigDecimal] = at{case (x, y) =>
      (x - y).pow(2)
    }
  }

  object absDiffMap extends Poly1{
    type ZippedType = (BigDecimal, Int)
    implicit def atBigDecimal: Case.Aux[(BigDecimal, BigDecimal), BigDecimal] = at{case (x, y) =>
      (x - y).abs
    }
  }

  object reducerPoly extends Poly2{
    implicit def atBigDecimal: Case.Aux[BigDecimal, BigDecimal, BigDecimal] = at{case (acc, n)=>
      acc + n
    }
  }

  object maxReducer extends Poly2{
    implicit def atBigDecimal: Case.Aux[BigDecimal, BigDecimal, BigDecimal] = at{case (acc, n)=>
      acc.max(n)
    }
  }

  trait Generic[T]{self:T=>
    def repr[H <: HList]()(implicit gen: Generic.Aux[T, H]): H = {
      gen.to(this)
    }
  }

  trait Euclidean[T] extends Generic[T]{self:T =>

    def distance[H<: HList, K<:HList, L<: HList, N<:Nat](other: Euclidean[T])(
      implicit gen: Generic.Aux[T, H],
      zipper: Zip.Aux[H::H::HNil, L],
      diffMapper: Mapper.Aux[sqDiffMap.type, L, H],
      folder: LeftFolder.Aux[H, BigDecimal, reducerPoly.type, BigDecimal]
    ): BigDecimal = {

      BigDecimal(scala.math.pow(
        repr
          .zip(other.repr)
          .map(sqDiffMap)
          .foldLeft(BigDecimal(0))(reducerPoly).toDouble
        ,
        (1/BigDecimal(2)).toDouble
      )
      )
    }
  }


  trait Minkowski[T <: Minkowski[T]]{self: T=>

    def distance[H<: HList, K<:HList, L<: HList, N<:Nat](other: T, p: Int)(
      implicit gen: Generic.Aux[T, H],
      constZipper: ZipConst.Aux[Int, H, K],
      zipper: Zip.Aux[K::H::HNil, L],
      diffMapper: Mapper.Aux[diffMap.type, L, H],
      folder: LeftFolder.Aux[H, BigDecimal, reducerPoly.type, BigDecimal]
    ): BigDecimal = {
      val repr = gen.to(this)

      scala.math.pow(
        repr
          .zipConst(p)
          .zip(gen.to(other))
          .map(diffMap)
          .foldLeft(BigDecimal(0))(reducerPoly).toDouble
        ,
        (1/BigDecimal(p)).toDouble
      )
    }
  }

  trait Manhattan[T]{self: T=>

    def distance[H<: HList, K<:HList, L<: HList, N<:Nat](other: T)(
      implicit gen: Generic.Aux[T, H],
      zipper: Zip.Aux[H::H::HNil, L],
      diffMapper: Mapper.Aux[absDiffMap.type, L, H],
      folder: LeftFolder.Aux[H, BigDecimal, reducerPoly.type, BigDecimal]
    ): BigDecimal = {
      val repr = gen.to(this)

      repr
        .zip(gen.to(other))
        .map(absDiffMap)
        .foldLeft(BigDecimal(0))(reducerPoly)
    }

  }

  trait Chebyshev[T]{self: T=>

    def distance[H <: HList, K<:HList, L<: HList](other: T)(
      implicit gen: Generic.Aux[T, H],
      zipper: Zip.Aux[H::H::HNil, L],
      diffMapper: Mapper.Aux[absDiffMap.type, L, H],
      folder: LeftFolder.Aux[H, BigDecimal, maxReducer.type, BigDecimal]
    ): BigDecimal = {

      val repr = gen.to(this)

      repr
        .zip(gen.to(other))
        .map(absDiffMap)
        .foldLeft(BigDecimal(0))(maxReducer)

    }

  }

  implicit class Levenshtein(t1: String){

    def distance(other: String): Int = {
      if(t1.length.min(other.length) == 0){
        t1.length.max(other.length)
      } else {
        (t1.tail.distance(other) + 1).min(
          other.tail.distance(t1) + 1
        ).min(
          t1.tail.distance(t1.tail) + (if(t1.head == other.head) 1 else 0)
        )
      }
    }

  }

  //TODO: WIP
  trait Orthodromic[T]{

    val latitude: Double
    val longitude: Double

    def distance(other: Orthodromic[T], radius: BigDecimal): BigDecimal = {
      radius*acos(
        sin(latitude)*sin(other.latitude)
          + cos(longitude)*cos(other.longitude)*cos((longitude - other.longitude).abs)
      )
    }
  }
}

// Note to self: not all dissimilarity measures are useful for implementing the k-d tree algorithm in FBF (1976)
// The dissimilarity measure must satisfy that the difference along a single axis is ALWAYS less or equal than
// the total dissimilarity. For example, Euclidean distance doesn't cut it, but Manhattan distance does.
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
  val seed = 1000
  val rnd = new scala.util.Random(seed)

  def random(xRange: Range, yRange: Range) = {
    val x = BigDecimal(rnd.nextDouble()*(xRange.end - xRange.start) + xRange.start)
    val y = BigDecimal(rnd.nextDouble()*(yRange.end - yRange.start) + yRange.start)
    Point(x, y)
  }
}
