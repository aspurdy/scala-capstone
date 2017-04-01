package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel}
import Interaction.tileLocation
import Visualization.interpolateColor

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x   X coordinate between 0 and 1
    * @param y   Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             x: Double,
                             y: Double,
                             d00: Double,
                             d01: Double,
                             d10: Double,
                             d11: Double
                           ): Double = {
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param zoom   Zoom level of the tile to visualize
    * @param x      X value of the tile to visualize
    * @param y      Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: (Int, Int) => Double,
                     colors: Iterable[(Double, Color)],
                     zoom: Int,
                     x: Int,
                     y: Int,
                     tileSize: Int = 256
                   ): Image = {

    def clampLongitude(lon: Int): Int = ((lon + 180) % 360) - 180

    def clampLatitude(lat: Int): Int = ((lat - 90) % 180) + 90

    val pixels = for {
      yOffset <- (0 until tileSize).par
      xOffset <- (0 until tileSize).par
      location = tileLocation(zoom, x + (xOffset / tileSize.toDouble), y + (yOffset / tileSize.toDouble))
      // find bounding grid coordinates
      // boundary conditions: latitude in [-89, 90] and a longitude in [-180, 179]
      x0 = clampLongitude(math.floor(location.lon).toInt)
      x1 = clampLongitude(math.ceil(location.lon).toInt)
      y0 = clampLatitude(math.ceil(location.lat).toInt)
      y1 = clampLatitude(math.floor(location.lat).toInt)
      // grid temperatures
      t00 = grid(y0, x0)
      t01 = grid(y1, x0)
      t10 = grid(y0, x1)
      t11 = grid(y1, x1)

      // lat/lon offsets [0, 1] inside unit square
      deltaX = location.lon - x0
      deltaY = y0 - location.lat
      t = bilinearInterpolation(deltaX, deltaY, t00, t01, t10, t11)
      color = interpolateColor(colors, t)
    } yield Pixel(color.red, color.green, color.blue, 127)

    Image(tileSize, tileSize, pixels.toArray)
  }

  def generateTileToDisk(year: Int, zoom: Int, x: Int, y: Int, grid: (Int, Int) => Double, colorScale: List[(Double, Color)], outDir: String, tileSize: Int = 256): Unit = {
    val dir = s"target/$outDir/$year/$zoom"
    val fileName = s"$x-$y.png"
    val dirFile = new File(dir)
    if (!dirFile.exists()) {
      dirFile.mkdirs()
    }
    val image = visualizeGrid(grid, colorScale, zoom, x, y, tileSize)
    val _ = image.output(new File(dir + File.separator + fileName))
  }
}

object GenerateTemperatureTiles extends App {

  import Extraction._
  import Visualization2._
  import Manipulation._

  // represents temperatures
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


  println("Computing yearly averages...")
  for {
    year <- (1975 to 1976) union (1990 to 1991)
    records = locateTemperatures(year, "/stations.csv", s"/$year.csv")
    yearlyAverages = locationYearlyAverageRecords(records)
    grid = makeGrid(yearlyAverages)
    zoom <- (0 to 3).par
    x <- 0 until 1 << zoom
    y <- 0 until 1 << zoom
  } yield generateTileToDisk(year, zoom, x, y, grid, colorScale, "temperatures", tileSize = 128)

}

object GenerateDeviationTiles extends App {

  import Extraction._
  import Visualization2._
  import Manipulation._


  // represents temperature deviations
  val colorScale: List[(Double, Color)] = List(
    (7.0, Color(0, 0, 0)),
    (4.0, Color(255, 0, 0)),
    (2.0, Color(255, 255, 0)),
    (0.0, Color(255, 255, 255)),
    (-2.0, Color(0, 255, 255)),
    (-7.0, Color(0, 0, 255))
  )

  println("Computing yearly averages...")
  val yearlyAverages = for {
    year <- 1975 to 1976
    temperatures = locateTemperatures(year, "/stations.csv", s"/$year.csv")
  } yield locationYearlyAverageRecords(temperatures)

  println("Computing normals...")
  // mean average temperatures for years between 1975 and 1989
  val normals = average(yearlyAverages)


  // incremental data manipulation
  for {
    year <- 1990 to 1991
    temperatures = locateTemperatures(year, "/stations.csv", s"/$year.csv")
    averages = locationYearlyAverageRecords(temperatures)
    _ = println(s"Generating deviation tile for year $year...")
    deviations = deviation(averages, normals)
    zoom <- (0 to 3).par
    x <- 0 until 1 << zoom
    y <- 0 until 1 << zoom
  } yield generateTileToDisk(year, zoom, x, y, deviations, colorScale, "deviations", tileSize = 128)
}
