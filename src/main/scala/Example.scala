import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import distance.{Point, TenDimensionalPoint, ThreeDimensionalPoint, TwoDimensionalPoint}
import tree.KDTree

import scala.concurrent.Future

object Example extends App {
  val query = TenDimensionalPoint(
    0,
    10,
    5,
    20,
//    50,
//    2,
//    9,
//    100,
//    50,
//    23,
//    8
  )

  val m: Int = 20
  val n: Int = 1000000

  val (data, _) = time("Generate data"){
    (1 to n)
      .par
      .map{_=>Point.random10D(Range.inclusive(10, 2000))}
      .distinct
      .toList
  }
//
//  println(data)
// Case 1:
//  val data = List(TenDimensionalPoint(1893.3742509642152,977.955660030486,1205.6199944801417,1319.5362250394828), TenDimensionalPoint(1218.8880545801535,800.4541297758566,633.8129949963233,942.3068595155291), TenDimensionalPoint(354.7107880477819,1231.3462404494605,1140.4790767133825,915.2715384914015), TenDimensionalPoint(255.38651590679956,1209.5431260809958,60.93122293876118,1078.019915533872), TenDimensionalPoint(1359.4689061306292,1067.6335755588289,1562.1642718419787,1361.8810308900809), TenDimensionalPoint(41.695980257061706,720.6179010356992,827.5893868774418,27.126635964562404), TenDimensionalPoint(1954.8266296157913,1915.9426099419688,1699.4104489668562,748.2289778756423), TenDimensionalPoint(195.86605747924773,1184.7555883682655,1894.841661648757,711.5349607859688), TenDimensionalPoint(620.1724018839553,563.5001541913509,737.2779230370123,678.5343754762877), TenDimensionalPoint(553.3879676405838,1944.9367358012082,1598.4970897682842,218.64127293947263))

//Case 2:
//  val data = List(TenDimensionalPoint(1218.888054860035,1936.0441039571838,1915.942602015348,1913.366330301771), TenDimensionalPoint(748.2289712474771,531.4947679209037,1570.3141897580726,28.371226975028467), TenDimensionalPoint(1359.4689061306292,1067.6335755588289,1562.1642718419787,1361.8810308900809), TenDimensionalPoint(354.7107880477819,1231.3462404494605,1140.4790767133825,915.2715384914015), TenDimensionalPoint(255.38651590679956,1209.5431260809958,60.93122293876118,1078.019915533872), TenDimensionalPoint(1893.3742268636227,88.41784907965935,977.9556574019479,897.0161060477823), TenDimensionalPoint(1319.5361963976418,1949.2480429071684,1263.8559892005135,1699.3978638813894), TenDimensionalPoint(195.86605747924773,1184.7555883682655,1894.841661648757,711.5349607859688), TenDimensionalPoint(620.1724018839553,563.5001541913509,737.2779230370123,678.5343754762877), TenDimensionalPoint(553.3879676405838,1944.9367358012082,1598.4970897682842,218.64127293947263))

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

//  println(treeMNN)
//  println(bruteMNN)

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
