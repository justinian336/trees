package generic

import shapeless.{Poly1, Poly2}

object GenericUtils {

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

}
