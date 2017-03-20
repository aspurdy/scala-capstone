package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import Extraction._

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  val year = 2000
  val results: Iterable[(LocalDate, Location, Double)] =
    locateTemperatures(year, "test_stations.csv", "test_temperatures.csv", test = true)

  test("weather stations are identified by the composite (STN, WBAN)") {
    assert(results.size === 9)
  }

  test("average temperature by location") {
    val averageTemps = locationYearlyAverageRecords(results)
    val sorted = averageTemps.toList.sortBy(_._1.lat)
    assert(sorted.head._1 === Location(1.0, -1.0))
    assert(sorted.head._2 === toCelsius(22))
  }
}