package observatory

import java.io.File

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import Visualization2._

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite with Checkers {

  val colorScale = List(
    (60.0, Color(255, 255, 255)),
    (-60.0, Color(0, 0, 0))
  )

  def euclideanDistance(c1: Color, c2: Color): Double = {
    val rd = c2.red - c1.red
    val gd = c2.green - c1.green
    val bd = c2.blue - c1.blue
    math.sqrt(rd * rd + gd * gd * bd * bd)
  }

  test("bilinearInterpolation") {
    val result = bilinearInterpolation(0.5, 0.5, 5, 10, -5, -10)
    assert(result == 0.0)
  }

  test("visualizeGrid - zoom 0") {
    import Manipulation.makeGrid

    val (temp1, color1) = colorScale.head
    val (temp2, color2) = colorScale.last

    val temperatures = List(
      (Location(45, -50), temp1), // upper left
      (Location(-45, 50), temp2) // lower right
    )

    val grid = makeGrid(temperatures)
    val img = visualizeGrid(grid, colorScale, zoom = 0, x = 0, y = 0)
    //    img.output(new File("/home/alex/IdeaProjects/scala-capstone/observatory/test-zoom-0.png"))
    val pixel = img.pixel(25, 25)
    val color = Color(pixel.red, pixel.green, pixel.blue)
    assert(euclideanDistance(color, color1) < euclideanDistance(color, color2))
  }

  test("visualizeGrid - zoom 1") {
    import Manipulation.makeGrid

    val (temp1, color1) = colorScale.head
    val (temp2, color2) = colorScale.last

    val temperatures = List(
      (Location(45, -50), temp1), // upper left
      (Location(-45, 50), temp2) // lower right
    )

    val grid = makeGrid(temperatures)
    // lower right quadrant
    val img = visualizeGrid(grid, colorScale, zoom = 1, x = 1, y = 1)
    //    img.output(new File("/home/alex/IdeaProjects/scala-capstone/observatory/test-zoom-1.png"))
    val pixel = img.pixel(25, 25)
    val color = Color(pixel.red, pixel.green, pixel.blue)
    assert(euclideanDistance(color, color2) < euclideanDistance(color, color1))
  }


}
