package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  import Visualization._

  val white = Color(255, 255, 255)
  val black = Color(0, 0, 0)

  val colorScale = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

  test("degenerate case") {
    val temperatures = List(
      (Location(0, 0), 10.0),
      (Location(10, -10), 0.0)
    )

    val result = predictTemperature(temperatures, Location(0, 0))
    assert(result === 10.0)
  }

  test("midpoint between antipodal points should give average") {
    val temperatures = List(
      (Location(10, 10), 0.0),
      (Location(-10, -10), 10.0)
    )

    val result = predictTemperature(temperatures, Location(0, 0))
    assert(result === 5.0)
  }

  test("should interpolate colors") {
    val points = List((0.0, white), (10.0, black))

    val res = interpolateColor(points, 5.0)
    assert(res === Color(128, 128, 128))
  }

  test("should interpolate colors - color order reversed") {
    val points = List((0.0, black), (10.0, white))

    val res = interpolateColor(points, 5.0)
    assert(res === Color(128, 128, 128))
  }

  test("interpolate over max range ") {
    val points = List((0.0, black), (10.0, white))

    val res = interpolateColor(points, 11.0)
    assert(res === white)
  }
  test("interpolate under min range ") {
    val points = List((0.0, black), (10.0, white))

    val res = interpolateColor(points, -1.0)
    assert(res === black)
  }
}
