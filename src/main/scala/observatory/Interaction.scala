package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization._

import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
  val tileSize = 256

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Double, y: Double): Location = {
    val n = (1 << zoom).toDouble
    val lonDegree = x / n * 360.0 - 180.0
    val latRadians = atan(sinh(Pi * (1 - 2 * y / n)))
    val latDegrees = toDegrees(latRadians)
    Location(latDegrees, lonDegree)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val locations = for {
      yOffset <- Range(0, tileSize)
      xOffset <- Range(0, tileSize)
    } yield tileLocation(zoom, x + (xOffset / tileSize.toDouble), y + (yOffset / tileSize.toDouble))

    val pixels = locations.map { location =>
      val predictedTemp = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, predictedTemp)
      Pixel(color.red, color.green, color.blue, 127)
    }

    Image(tileSize, tileSize, pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit,
                           zoom: Int = 3
                         ): Unit = {
    val params = for {
      tpl <- yearlyData
      zoom <- Range.inclusive(0, zoom)
      x <- Range(0, 1 << zoom)
      y <- Range(0, 1 << zoom)
    } yield (tpl._1, zoom, x, y, tpl._2)

    params.par.foreach(generateImage.tupled)
  }

}

object GenerateTiles extends App {

  import Extraction._
  import Interaction._

  val results = locateTemperatures(2000, "/stations.csv", "/2000.csv")
  val averages = locationYearlyAverageRecords(results)
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

  val yearlyData = List((2000, averages))


  def generateTileToDisk(year: Int, zoom: Int, x: Int, y: Int, temperatureData: Iterable[(Location, Double)]): Unit = {
    val dir = s"target/temperatures/$year/$zoom"
    val fileName = s"$x-$y.png"
    val dirFile = new File(dir)
    if (!dirFile.exists()) {
      dirFile.mkdirs()
    }
    val image = tile(temperatureData, colorScale, zoom, x, y)
    val _ = image.output(new File(dir + File.separator + fileName))
  }

  generateTiles[Iterable[(Location, Double)]](yearlyData, generateTileToDisk, zoom = 1)
}
