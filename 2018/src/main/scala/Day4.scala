import scala.io.Source
import scala.collection.mutable.SortedSet
import scala.collection.immutable.Stream

object Day4 {
  case class TS(y: Int, m: Int, d: Int, hh: Int, mm: Int) extends Ordered[TS] {
    import scala.math.Ordered.orderingToOrdered

    def compare(that: TS): Int =
      (this.y, this.d, this.m, this.hh, this.mm) compare (that.y, that.d, that.m, that.hh, that.mm)
  }

  sealed trait Event {
    import scala.math.Ordered.orderingToOrdered

    def ts: TS
    def compare(that: Event): Int = this.ts compare that.ts
  }

  case class SetGuard(ts: TS, id: Int) extends Event
  case class Asleep(ts: TS) extends Event
  case class Awake(ts: TS) extends Event

  def parse(s: String) = {
    val tsString = s.take(18)
    val groups = Utils.numbers(tsString)
    val ts = TS(groups(0), groups(1), groups(2), groups(3), groups(4))
    val action = s.drop(19)
    action match {
      case "wakes up" =>
        Awake(ts)
      case "falls asleep" =>
        Asleep(ts)
      case _ =>
        val guardNum = Utils.numbers(action)(0)
        SetGuard(ts, guardNum)
    }
  }

  val input =
    Source.fromFile("src/main/resources/Day4.input").getLines.map(parse).toList.sortBy(_.ts)
}

class Day4 {
  import Day4._

  def run: Unit = {
  }

  def run2: Unit = {}
}
