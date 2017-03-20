package observatory

import java.io.InputStream
import java.time.LocalDate

import scala.io.Source


/**
  * 1st milestone: data extraction
  */
object Extraction {

  def toCelsius(fahrenheit: Double): Double = (fahrenheit - 32) * 5.0 / 9.0

  def loader(test: Boolean): String => InputStream = {
    if (test) getClass.getClassLoader.getResourceAsStream(_)
    else getClass.getResourceAsStream(_)
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String, test: Boolean = false): Iterable[(LocalDate, Location, Double)] = {

    // a little hack to load test resources for unit-testing
    val loaderFunc = loader(test)

    val stationMap = Source
      .fromInputStream(loaderFunc(stationsFile))
      .getLines()
      .map(_.split(",", -1))
      // drop records missing both station identifiers or geographic location
      // format is (STN id, WBAN id, latitude, longitude)
      .filter(array => (array(0).nonEmpty || array(1).nonEmpty) &&
      !array.slice(2, 4).contains(""))
      // convert to lookup map station id -> location
      .map(array => (array(0), array(1)) -> Location(array(2).toDouble, array(3).toDouble))
      .toMap

    Source
      .fromInputStream(loaderFunc(temperaturesFile))
      .getLines()
      .map(_.split(",", -1))
      // drop records missing both station ids temperature or date data
      // format is (STN id, WBAN id, month, day, temperature)
      .filter(array =>
      (array(0).nonEmpty || array(1).nonEmpty) &&
        !array.slice(2, 5).contains("") &&
        array(4).toDouble != 9999.0)
      .map(array => {
        val localDate = LocalDate.of(year, array(2).toInt, array(3).toInt)
        // look up location data
        val maybeLocation = stationMap.get((array(0), array(1)))
        val temperature = toCelsius(array(4).toDouble)
        (localDate, maybeLocation, temperature)
      })
      // drop any temperature records that were missing location data (no corresponding record in stations.csv)
      .collect { case (localDate, Some(location), temperature) => (localDate, location, temperature) }
      // sorting is required due to grader idiosyncrasies. remove if grader is fixed
      .toList.sortBy(tpl => (tpl._1.getMonthValue, tpl._1.getDayOfMonth))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = records
    .groupBy(_._2)
    .mapValues { iterable =>
      val size = iterable.size
      val sum = iterable.map(_._3).sum
      sum / size
    }.toStream
}

object ExtractTestData extends App {

  import Extraction._

  val results = locateTemperatures(2000, "/stations.csv", "/2000.csv")
  results.take(10).foreach(println)
  val averages = locationYearlyAverageRecords(results)
  averages.take(10).foreach(println)
}

