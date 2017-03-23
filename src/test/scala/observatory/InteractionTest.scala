package observatory

import java.util.concurrent.atomic.AtomicInteger

import observatory.Interaction._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("generateTiles - should have complete coverage") {
    val atomicInt = new AtomicInteger()

    def trackCalls(year: Int, zoom: Int, x: Int, y: Int, data: Any): Unit = {
      val _ = atomicInt.incrementAndGet()
    }

    val years = Range.inclusive(1901, 2000).map(year => (year, Nil))

    generateTiles[Any](years, trackCalls)
    assert(atomicInt.get() === (1 + 4 + 16 + 64) * years.size)
  }
}
