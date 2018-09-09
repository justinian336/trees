package distance

object Point{
  val seed = 1000
  val rnd = new scala.util.Random(seed)

  // Note to self: not all dissimilarity measures are useful for implementing the k-d tree algorithm in FBF (1976)
  // The dissimilarity measure must satisfy that the difference along a single axis is ALWAYS less or equal than
  // the total dissimilarity. For example, Euclidean distance doesn't cut it, but Manhattan distance does.
  case class DataPoint(features: List[BigDecimal]) extends Distance.Manhattan[DataPoint] with Identifiable

  case class StrPoint(x: String, y: String)

  private def getRandomBigDecimal(range: Range) =
    BigDecimal(rnd.nextDouble()*(range.end - range.start) + range.start)

  def randomPoint(range: Range, dimension: Int): DataPoint = {
    DataPoint(
      (1 to dimension).toList.map(_=>getRandomBigDecimal(range))
    )
  }
}
