package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization._

import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
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

  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int, tileSize: Int = 256): Image = {
    val temperaturesMap = temperatures.par.toMap
    val pixels = for {
      yOffset <- (0 until tileSize).par
      xOffset <- (0 until tileSize).par
      location = tileLocation(zoom, x + (xOffset / tileSize.toDouble), y + (yOffset / tileSize.toDouble))
      predictedTemp = idw(temperaturesMap, location)
      color = interpolateColor(colors, predictedTemp)
    } yield Pixel(color.red, color.green, color.blue, 127)

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
                           maxZoomLevel: Int = 3,
                           tileSize: Int = 256
                         ): Unit = {
    val params = for {
      (year, data) <- yearlyData
      zoom <- 0 to maxZoomLevel
      x <- 0 until (1 << zoom)
      y <- 0 until (1 << zoom)
    } yield (year, zoom, x, y, data)

    params.par.foreach(generateImage.tupled)
  }
}
