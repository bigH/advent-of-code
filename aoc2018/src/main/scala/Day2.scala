import scala.io.Source
import scala.collection.mutable.SortedSet
import scala.collection.immutable.Stream

class Day2 {
  val input = Source.fromFile("src/main/resources/Day2.input").getLines.toList

  def has(num: Int)(s: String) = {
    s.groupBy(identity).exists {
      case (_, v) => v.length == num
    }
  }

  def run: Unit = {
    val num2 = input.count(has(2))
    val num3 = input.count(has(3))

    Console.println(num2 * num3)
  }

  def run2: Unit = {
    // when not reversed, didn't find the answer,
    // so reversing helped (reason being that we're
    // looking for a string with a 1 letter difference,
    // so if it's one of the first few, it'll hurt
    // our sort approach
    //
    // reversing moves the issue to the opposite side
    // of the string
    //
    // obviously, i think we got sort-of lucky here
    //
    // ultimately, it's about finding the answer not
    // writing perfect code
    val sorted = input.map(_.reverse).sorted
    val pairs = sorted.sliding(2)

    for (pair <- pairs)
      pair match {
        case a :: b :: _ =>
          val zipped = a.zip(b)
          val same = zipped.filter {
            case (a, b) => a == b
          }

          val sameLetters = same.foldLeft("") {
            case (s, (a, _)) => s + a
          }

          if (a.size - same.size == 1) {
            Console.println(sameLetters.reverse)
          }
        case _ =>
          throw Exception("oh no")
      }
  }
}
