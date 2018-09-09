package distance

import java.util.UUID

import distance.ShapelessDistance.{GenericDataPoint, Manhattan}
import shapeless.ops.hlist.{LeftFolder, Mapper, Zip, ZipConst}
import shapeless.ops.tuple.ToList
import shapeless.{::, Generic, HList, HNil}
import generic.GenericUtils._

import scala.math.{acos, cos, sin}

object ShapelessDistance {

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
}

trait Identifiable{
  val id: UUID = UUID.randomUUID()

  def canEqual(that: Any): Boolean = {
    that match{
      case other: Identifiable => id.equals(other.id)
      case _ => false
    }
  }
}


