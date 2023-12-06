trait Utils {
  def numbers(s: String): List[Int] = {
    var inNumber = false
    var currentNumber = 0
    var numbers: List[Int] = Nil

    val zero = '0'.toInt

    for (c <- s) {
      val asDigit = c.toInt - zero
      val isDigit = c.isDigit

      if (isDigit) {
        inNumber = true
        currentNumber = currentNumber * 10 + asDigit
      } else if (inNumber) {
        numbers = numbers :+ currentNumber
        inNumber = false
        currentNumber = 0
      }
    }

    if (inNumber) {
      numbers :+ currentNumber
    } else {
      numbers
    }
  }
}

object Utils extends Utils
