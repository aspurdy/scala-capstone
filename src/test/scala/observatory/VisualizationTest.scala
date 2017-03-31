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

  def sumAbsDiff(c1: Color, c2: Color): Int = {
    val rd = math.abs(c1.red - c2.red)
    val gd = math.abs(c1.green - c2.green)
    val bd = math.abs(c1.blue - c2.blue)
    rd + gd + bd
  }

  test("predict temperature - degenerate case") {
    val temperatures = List(
      (Location(0, 0), 10.0),
      (Location(10, -10), 0.0)
    )

    val result = predictTemperature(temperatures, Location(0, 0))
    assert(result === 10.0)
  }

  test("predict temperature - midpoint between antipodal points should give simple average") {
    val temperatures = List(
      (Location(10, 10), 0.0),
      (Location(-10, -10), 10.0)
    )

    val result = predictTemperature(temperatures, Location(0, 0))
    assert(result === 5.0)
  }

  test("interpolate colors - midpoint between white and black returns grey") {
    val points = List((0.0, white), (10.0, black))

    val res = interpolateColor(points, 5.0)
    assert(res === Color(128, 128, 128))
  }

  test("interpolate colors - midpoint between black and white returns grey") {
    val points = List((0.0, black), (10.0, white))

    val res = interpolateColor(points, 5.0)
    assert(res === Color(128, 128, 128))
  }

  test("great circle distance between identical locations should return 0") {
    val l1 = Location(0, 0)
    val l2 = Location(0, 0)
    assert(l1.sphericalCosineDistance(l2) == 0)
  }
  test("great circle distance should return known result") {
    val l1 = Location(0, 0)
    val l2 = Location(90, 90)
    assert(l1.sphericalCosineDistance(l2) == math.toRadians(90) * 6371.0)
  }

  test("interpolate color - degenerate min") {
    val scale = List(
      (-54.29080717075983, Color(255, 0, 0)),
      (-1.0, Color(0, 0, 255))
    )
    val c = interpolateColor(scale, -54.29080717075983)
    assert(c === Color(255, 0, 0))
  }
  test("interpolate color - degenerate max") {
    val scale = List(
      (-54.29080717075983, Color(255, 0, 0)),
      (-1.0, Color(0, 0, 255))
    )
    val c = interpolateColor(scale, -1.0)
    assert(c === Color(0, 0, 255))
  }
  test("interpolate color - below min") {
    val scale = List(
      (-54.29080717075983, Color(255, 0, 0)),
      (-34.29080717075983, Color(0, 255, 0)),
      (-1.0, Color(0, 0, 255))
    )
    val c = interpolateColor(scale, -164.29080717075983)
    assert(c === Color(255, 0, 0))
  }
  test("interpolate color - above max") {
    val scale = List(
      (-54.29080717075983, Color(255, 0, 0)),
      (-1.0, Color(0, 0, 255))
    )
    val c = interpolateColor(scale, 0.0)
    assert(c === Color(0, 0, 255))
  }

  test("interpolated color - closer to min") {
    val scale = List(
      (-54.29080717075983, Color(255, 0, 0)),
      (-1.0, Color(0, 0, 255))
    )
    val c = interpolateColor(scale, -43.0)
    assert(sumAbsDiff(c, scale.head._2) < sumAbsDiff(c, scale(1)._2))
  }

  test("interpolate color - closer to max") {
    val scale = List(
      (-54.29080717075983, Color(255, 0, 0)),
      (-1.0, Color(0, 0, 255))
    )
    val c = interpolateColor(scale, -4.0)
    assert(sumAbsDiff(c, scale.head._2) > sumAbsDiff(c, scale(1)._2))
  }

  test("interpolate color - exactly half temp") {
    val c1 = Color(255, 0, 0)
    val c2 = Color(0, 0, 255)
    val t1 = -1.0
    val t2 = 36.64568576176336
    val scale = List(
      (t1, c1),
      (t2, c2)
    )
    val v = (t2 - t1) / 2 + t1

    val c = interpolateColor(scale, v)
    assert(c == Color(128, 0, 128))
    assert(sumAbsDiff(c, c2) == sumAbsDiff(c, c1))
  }

  test("predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {
    val t1 = 0.0
    val t2 = -60.0
    val x = Location(0, 0)
    val y = Location(90, 90)
    val temperatures = List(
      (x, t1),
      (y, t2)
    )

    // closer to x
    val z1 = Location(10, 10)
    val pt1 = predictTemperature(temperatures, z1)
    assert(math.abs(t1 - pt1) < math.abs(t2 - pt1))

    // closer to y
    val z2 = Location(80, 80)
    val pt2 = predictTemperature(temperatures, z2)
    assert(math.abs(t1 - pt2) > math.abs(t2 - pt2))
  }
}

