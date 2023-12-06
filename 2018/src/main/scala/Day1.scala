import scala.io.Source
import scala.collection.mutable.SortedSet
import scala.collection.immutable.Stream

class Day1 {
  val input = Source.fromFile("src/main/resources/Day1.input").getLines.map(_.toInt).toList

  def run: Unit =
    Console.println(input.sum)

  @annotation.nowarn def loop[T](list: List[T]): Stream[T] = {
    Stream.continually(list).flatMap(Stream.from(_))
  }

  def run2: Unit = {
    val stream = loop(input)
    val sums = stream.scanLeft(0)(_ + _)
    val seen = SortedSet[Int]()

    sums.find(sum => {
      if (seen(sum)) {
        true
      } else {
        seen += sum
        false
      }
    }) match {
      case None => Console.println("nothing")
      case Some(found) => Console.println(found)
    }
  }
}
