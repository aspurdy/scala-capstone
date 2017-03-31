package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.parallel.ParIterable

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  implicit class RichLocation(loc: Location) {

    val earthRadius = 6371

    import math._

    def toRadians: (Double, Double) = (math.toRadians(loc.lat), math.toRadians(loc.lon))

    def sphericalCosineDistance(other: Location): Double = {
      val (lat1, lon1) = loc.toRadians
      val (lat2, lon2) = other.toRadians
      val lonDelta = abs(lon1 - lon2)
      val centralAngle = acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lonDelta))
      centralAngle * earthRadius
    }

    def haversineDistance(other: Location): Double = {
      val (lat1, lon1) = loc.toRadians
      val (lat2, lon2) = other.toRadians
      val lonDelta = abs(lon1 - lon2)
      val latDelta = abs(lat1 - lat2)
      val centralAngle = 2 * asin(pow(sin(0.5 * latDelta), 2) + cos(lat1) * cos(lat2) * pow(sin(0.5 * lonDelta), 2))
      centralAngle * earthRadius
    }

    // accurate for all distances
    def vincentyDistance(other: Location): Double = {
      val (lat1, lon1) = loc.toRadians
      val (lat2, lon2) = other.toRadians
      val lonDelta = abs(lon1 - lon2)
      val y = sqrt(
        pow(cos(lat2) * sin(lonDelta), 2) +
          pow(cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lonDelta), 2))
      val x = sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lonDelta)
      val centralAngle = atan2(y, x)
      earthRadius * centralAngle
    }
  }

  def idw(samples: ParIterable[(Location, Double)], value: Location, power: Double = 4): Double = {
    val distances = samples.map { case (location, temperature) => (value.vincentyDistance(location), temperature) }

    distances.find { case (distance, _) => distance == 0 } match {
      case Some((_, temperature)) => temperature
      case None =>
        val (weighted, weights) = distances.aggregate((0.0, 0.0))(seqop = (acc, record) => {
          val (weightedAcc, weightsAcc) = acc
          val (dist, temp) = record
          val weight = math.pow(1.0 / dist, power)
          val weighted = weight * temp
          (weightedAcc + weighted, weightsAcc + weight)
        }, combop = (w1, w2) => (w1._1 + w2._1, w1._2 + w2._2))

        weighted / weights
    }
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    idw(temperatures.par, location)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    import math._

    def scale(xMin: Double, xMax: Double, x: Double) = (x - xMin) / (xMax - xMin)

    def lerp(v0: Double, v1: Double, t: Double) = (1 - t) * v0 + t * v1

    val pointsMap = points.toMap

    // t1 always <= t2
    val (t1, c1, t2, c2) = {
      val first = pointsMap.minBy(point => abs(point._1 - value))
      val second = pointsMap - first._1 minBy (point => abs(point._1 - value))
      if (first._1 < second._1) (first._1, first._2, second._1, second._2)
      else (second._1, second._2, first._1, first._2)
    }

    value match {
      case x if x < t1 => c1
      case x if x > t2 => c2
      case _ =>
        val t = scale(t1, t2, value)
        val r = lerp(c1.red, c2.red, t)
        val g = lerp(c1.green, c2.green, t)
        val b = lerp(c1.blue, c2.blue, t)
        Color(round(r).toInt, round(g).toInt, round(b).toInt)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val locations = for {
      y <- Range(90, -90, -1)
      x <- Range(-180, 180)
    } yield Location(y, x)

    val pixels = locations.par.map { location =>
      val predictedTemp = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, predictedTemp)
      Pixel(color.red, color.green, color.blue, 255)
    }

    Image(360, 180, pixels.toArray)
  }
}


object VisualizationBenchmark extends App {

  import Extraction._
  import Visualization._
  import org.scalameter
  import org.scalameter._

  val results = locateTemperatures(2000, "/stations.csv", "/2000.csv").take(16).par
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

  val time = config(
    Key.exec.benchRuns -> 5,
    Key.verbose -> true
  ) withWarmer {
    new scalameter.Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  }

  val visualizeTime = time measure {
    visualize(averages, colorScale)
  }
  println(s"visualize took $visualizeTime")
}