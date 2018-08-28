package distance

import java.util.UUID

import distance.Distance.{GenericDataPoint, Manhattan}
import shapeless.ops.hlist.{LeftFolder, Mapper, Zip, ZipConst}
import shapeless.ops.tuple.ToList
import shapeless.{::, Generic, HList, HNil}
import generic.GenericUtils._

import scala.math.{acos, cos, sin}

object Distance {

  trait GenericDataPoint[T]{self:T=>
    def repr[H <: HList]()(implicit gen: Generic.Aux[T, H]): H = {
      gen.to(this)
    }

    def toList(implicit toList: ToList.Aux[T, BigDecimal, List[BigDecimal]]) = {
      toList(self)
    }
  }

  trait Euclidean[T] extends GenericDataPoint[T]{self:T =>

    def distance[H<: HList, L<: HList](other: T)(
      implicit gen: Generic.Aux[T, H],
      zipper: Zip.Aux[H::H::HNil, L],
      diffMapper: Mapper.Aux[sqDiffMap.type, L, H],
      folder: LeftFolder.Aux[H, BigDecimal, reducerPoly.type, BigDecimal]
    ): BigDecimal = {

      BigDecimal(scala.math.pow(
        repr
          .zip(gen.to(other))
          .map(sqDiffMap)
          .foldLeft(BigDecimal(0))(reducerPoly).toDouble
        ,
        (1/BigDecimal(2)).toDouble
      )
      )
    }
  }

  trait Minkowski[T <: Minkowski[T]] extends GenericDataPoint[T]{self: T=>

    def distance[H<: HList, K<:HList, L<: HList](other: T, p: Int)(
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

  trait Manhattan[T] extends GenericDataPoint[T]{self: T=>

    def distance[H<: HList, K<:HList, L<: HList](other: T)(
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

  trait Chebyshev[T] extends GenericDataPoint[T]{self: T=>

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
  trait Orthodromic[T] extends GenericDataPoint[T]{self:T=>

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
case class ThreeDimensionalPoint(x: BigDecimal, y: BigDecimal, z: BigDecimal) extends Manhattan[ThreeDimensionalPoint] with Identifiable
case class TwoDimensionalPoint(x: BigDecimal, y: BigDecimal) extends Manhattan[TwoDimensionalPoint] with Identifiable
case class FourDimensionalPoint(d0: BigDecimal,
                                d1: BigDecimal,
                                d2: BigDecimal,
                                d3: BigDecimal
                              ) extends Manhattan[FourDimensionalPoint] with Identifiable

case class StrPoint(x: String, y: String)

trait Identifiable{
  val id: UUID = {
    val byteArray = new Array[Byte](16)
    Point.rnd.nextBytes(byteArray)
    UUID.nameUUIDFromBytes(byteArray)
  }

  def canEqual(that: Any): Boolean = {
    that match{
      case other: Identifiable => id.equals(other.id)
      case _ => false
    }
  }
}

object Point{
  val seed = 1000
  val rnd = new scala.util.Random(seed)

  private def getRandomBigDecimal(range: Range) =
    BigDecimal(rnd.nextDouble()*(range.end - range.start) + range.start)

  def random2D(range: Range) = {
    TwoDimensionalPoint(
      getRandomBigDecimal(range),
      getRandomBigDecimal(range)
    )
  }

  def random3D(range: Range) = {
    ThreeDimensionalPoint(
      getRandomBigDecimal(range),
      getRandomBigDecimal(range),
      getRandomBigDecimal(range)
    )
  }

  def random10D(range: Range) = {
    FourDimensionalPoint(
      getRandomBigDecimal(range),
      getRandomBigDecimal(range),
      getRandomBigDecimal(range),
      getRandomBigDecimal(range)
    )
  }

  def findMaxRangeDimension[T <: GenericDataPoint[T]](data: List[T])(
    implicit toList: ToList.Aux[T, BigDecimal, List[BigDecimal]]): Int = {

    val (minVals: List[BigDecimal], maxVals: List[BigDecimal]) = data
      .tail
      .foldLeft[(List[BigDecimal], List[BigDecimal])]((data.head.toList, data.head.toList)){case (cumulative, next)=>
      (
        cumulative._1.zip(next.toList).map{case (x0, x1)=> x0.min(x1)},
        cumulative._2.zip(next.toList).map{case (x0, x1)=> x0.max(x1)}
      )
    }

    minVals
      .zip(maxVals)
      .map{case (x, y)=> (x-y).abs}
      .zipWithIndex
      .maxBy{case (value, i)=> value}._2
  }
}
