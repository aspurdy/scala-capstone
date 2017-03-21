package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  implicit class RichLocation(loc: Location) {

    import math.{cos, abs, acos, sin}

    val earthRadius = 6371

    def greatCircleDistance(other: Location): Double = {
      val lonDelta = abs(loc.lon - other.lon)
      val centralAngle = acos(sin(loc.lat) * sin(other.lat) + cos(loc.lat) * cos(other.lat) * cos(lonDelta))
      earthRadius * centralAngle
    }
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    temperatures.find(_._1 == location) match {
      case Some((_, temperature)) => temperature
      case None =>
        // inverse distance weighting (basic form)
        // see https://en.wikipedia.org/wiki/Inverse_distance_weighting
        val (totalWeighted, totalWeights) = temperatures.foldLeft(0.0, 0.0) { (acc, point) =>
          val weight = 1 / location.greatCircleDistance(point._1)
          val weighted = weight * point._2
          (acc._1 + weighted, acc._2 + weight)
        }
        totalWeighted / totalWeights
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    import math._

    def clamp(x: Double) = 0.0.max(x).min(1.0)

    def scale(xMin: Double, xMax: Double, x: Double) = (x - xMin) / (xMax - xMin)

    def lerp(v0: Double, v1: Double, t: Double) = (1 - t) * v0 + t * v1

    val pointsMap = points.toMap
    val (t1, c1, t2, c2) = {
      val first = pointsMap.minBy(point => abs(point._1 - value))
      val second = pointsMap - first._1 minBy (point => abs(point._1 - value))
      if (first._1 < second._1) (first._1, first._2, second._1, second._2)
      else (second._1, second._2, first._1, first._2)
    }

    val t = clamp(scale(min(t1, t2), max(t1, t2), value))

    val r = lerp(c1.red, c2.red, t)
    val g = lerp(c1.green, c2.green, t)
    val b = lerp(c1.blue, c2.blue, t)
    Color(round(r).toInt, round(g).toInt, round(b).toInt)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val locations = for {
      y <- Range(0, 180)
      x <- Range(0, 360)
    } yield Location(x - 180, -y + 90)

    val pixels = locations.par.map { location =>
      val predictedTemp = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, predictedTemp)
      Pixel(color.red, color.green, color.blue, 255)
    }

    Image(360, 180, pixels.toArray)
  }
}

object VisApp extends App {

  import Extraction._

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

  val image = Visualization.visualize(temperatures = averages, colors = colorScale)

  image.output(new File("target/test.png"))
}
