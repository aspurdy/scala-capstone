package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /** Fast approximation of the inverse cosine function.
    *
    * @see http://http.developer.nvidia.com/Cg/acos.html
    * @return computes the inverse cosine of the given angle in radians.
    * @param radians the input angle in radians [-1, 1]
    **/
  def acos(radians: Double): Double = {
    val negate = if (radians < 0) 1.0 else 0.0
    val d = math.abs(radians)
    val expansion = ((((((-0.0187293 * d) + 0.0742610) * d) - 0.2121144) * d) + 1.5707288) * math.sqrt(1.0 - d)
    negate * 3.14159265358979 + (expansion - (2 * negate * expansion))
  }

  def sphericalCosineDistance(location: Location, other: Location): Double = {
    import math.{abs, cos, sin}
    val (lat1, lon1) = (location.lat.toRadians, location.lon.toRadians)
    val (lat2, lon2) = (other.lat.toRadians, other.lon.toRadians)
    val lonDelta = abs(lon1 - lon2)
    val centralAngle = acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lonDelta))
    centralAngle * 6371
  }

  def idw(temperatures: Iterable[(Location, Double)],
          gridLocation: Location,
          power: Double = 1): Double = {
    val distances = temperatures.map(tpl => (sphericalCosineDistance(tpl._1, gridLocation), tpl._2))
    distances.find(_._1 == 0) match {
      case Some((_, temp)) => temp
      case None =>
        val (weightedSum, weightSum) = distances.aggregate((0.0, 0.0))(
          seqop = (acc, record) => {
            val (weightedSumAcc, weightSumAcc) = acc
            val (distance, temperature) = record
            //            val weight = math.pow(1.0 / dist, power)
            val weight = 1.0 / distance
            (weightedSumAcc + weight * temperature, weightSumAcc + weight)
          }, combop = (w1, w2) => (w1._1 + w2._1, w1._2 + w2._2))
        weightedSum / weightSum
    }
  }

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val parGridData = for {
      latitude <- (-89 to 90).par
      longitude <- (-180 to 179).par
    } yield (latitude, longitude) -> idw(temperatures, Location(latitude, longitude))

    Function.untupled(parGridData.toMap.apply)
  }

  /**
    * @param temperaturess Sequence of known temperatures
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val grids = temperaturess.map(makeGrid)
    val n = temperaturess.size
    val averagedData = for {
      latitude <- (-89 to 90).par
      longitude <- (-180 to 179).par
    } yield (latitude, longitude) -> grids.map(_ (latitude, longitude)).sum / n
    Function.untupled(averagedData.toMap.apply)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val grid = makeGrid(temperatures)
    val deviationData = for {
      latitude <- (-89 to 90).par
      longitude <- (-180 to 179).par
    } yield (latitude, longitude) -> (grid(latitude, longitude) - normals(latitude, longitude))
    Function.untupled(deviationData.toMap.apply)
  }
}

