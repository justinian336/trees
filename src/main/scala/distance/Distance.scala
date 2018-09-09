package distance

import distance.ShapelessDistance.GenericDataPoint

import scala.math.{acos, cos, sin}

trait Distance[T]{self: T =>

  def distance(other: T): BigDecimal

  val features: List[BigDecimal]
}

object Distance{

  trait Manhattan[T <: Distance[T]] extends Distance[T]{self:T=>

    override def distance(other: T): BigDecimal = {
      features
        .zip(other.features)
        .map{case (x, y)=>
          (x-y).abs
        }
        .foldLeft(BigDecimal(0))(_ + _)
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
