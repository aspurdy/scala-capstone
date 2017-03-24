package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import Extraction._

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("average temperature by location") {
    val records: Seq[(LocalDate, Location, Double)] = Seq(
      (LocalDate.of(2000, 1, 1), Location(-1.0, -1.0), 10),
      (LocalDate.of(2000, 1, 2), Location(-1.0, -1.0), 20),
      (LocalDate.of(2000, 1, 3), Location(-1.0, -1.0), 30),
      (LocalDate.of(2000, 1, 1), Location(1.0, 1.0), 20),
      (LocalDate.of(2000, 1, 2), Location(1.0, 1.0), 20),
      (LocalDate.of(2000, 1, 3), Location(1.0, 1.0), 20)
    )


    val averageTemps = locationYearlyAverageRecords(records)
    assert(averageTemps.forall(_._2 == 20))
  }
}