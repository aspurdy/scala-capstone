package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import Manipulation._

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {

  test("makeGrid") {
    val t1 = 0.0
    val t2 = -60.0
    val x = Location(0, 0)
    val y = Location(90, 90)
    val temperatures = List(
      (x, t1),
      (y, t2)
    )
    val grid: (Int, Int) => Double = makeGrid(temperatures)


    assert(grid(0, 0) == 0.0)
    assert(grid(90, 90) == -60.0)

    val intermediateResult = grid(45, 45)
    assert(intermediateResult < 0.0 && intermediateResult > -60.0)
  }

  test("average") {
    val t1 = 0.0
    val t2 = -60.0
    val x = Location(0, 0)
    val y = Location(90, 90)
    val temps1 = List(
      (x, t1),
      (y, t2)
    )
    val temps2 = List(
      (x, t2),
      (y, t1)
    )
    val avg = average(Seq(temps1, temps2))

    assert(avg(0, 0) == -30.0)
  }

  test("deviation") {
    val t1 = 0.0
    val t2 = -60.0
    val x = Location(0, 0)
    val y = Location(90, 90)
    val temps1 = List(
      (x, t1),
      (y, t2)
    )
    val temps2 = List(
      (x, t2),
      (y, t1)
    )
    val avg = average(Seq(temps1, temps2))

    val dvs = deviation(temps1, avg)

    assert(dvs(0, 0) == 30.0)
  }

}