import io.StdIn._

object Mastermind extends App {

  var s = getS()
  var allCodes = getS()

  println("Come up with a 4 digit password using the digits {1, 2, 3, 4, 5, 6}")
  println("Guess: 1122")
  removeGuess(Array[Int](1, 1, 2, 2))
  println("How many colored pegs?")
  val black = readInt
  if (black == 4) println("Good game")
  else {
    println("How many white pegs?")
    val white = readInt
    if (white == 4) {
      println("Guess: 2211")
      println("How many black?")
      readInt
      println("Good game")
    } else guess(Array(1, 1, 2, 2), black, white)
  }

  def guess(lastGuess: Array[Int], black: Int, white: Int) {
    removeFromS(lastGuess, black, white)
    val newGuess = maxG()
    println("Guess: " + newGuess(0) + "" + newGuess(1) + "" + newGuess(2) + "" + newGuess(3))
    println("How many colored pegs?")
    val g = readInt
    if (g == 4) println("Good game")
    else {
      println("how many white pegs?")
      val si = readInt
      println()
      removeGuess(newGuess)
      guess(newGuess, g, si)
    }
  }

  def removeGuess(guess: Array[Int]) {
    for (x <- 0 until allCodes.size reverse) {
      if (allCodes(x)(0) == guess(0) && allCodes(x)(1) == guess(1) && allCodes(x)(2) == guess(2) && allCodes(x)(3) == guess(3)) {
        allCodes = allCodes.patch(x, Nil, 1)
      }
    }
  }

  def removeFromS(lg: Array[Int], black: Int, silv: Int) {
    for (i <- 0 until s.size reverse) {
      val (go, si) = getGS(s(i), lg)
      if (!(go == black && si == silv)) {
        s = s.patch(i, Nil, 1)
      }
    }
  }

  def minG(code: Array[Int]): Int = {
    var min = 2000
    for (go <- 0 to 3) {
      for (si <- 0 to 4 - go) {
        if (!(si == 1 && go == 3)) {
          var m = numRemoved(code, go, si)
          if (m < min) min = m
        }
      }
    }
    //print(min + " ")
    min
  }

  def maxG(): Array[Int] = {
    var max = 0
    var maxCode = Array[Int](0, 0, 0, 0)
    for (x <- 0 until allCodes.length) {
      val score = minG(allCodes(x))
      if (score > max) {
        max = score
        maxCode = allCodes(x)
      }
    }
    maxCode
  }

  def numRemoved(lg: Array[Int], black: Int, silv: Int): Int = {
    var count = 0
    for (i <- 0 until s.length) {
      val (go, si) = getGS(s(i), lg)
      if (go != black || si != silv) count = count + 1
    }
    //print(count + " ")
    count
  }

  def getGS(newS: Array[Int], code: Array[Int]): (Int, Int) = {
    var black = 0
    var white = 0
    var usedC = Array[Boolean](false, false, false, false)
    var usedS = Array[Boolean](false, false, false, false)
    for (x <- 0 until code.size) {
      if (newS(x) == code(x)) {
        black += 1
        usedC(x) = true
        usedS(x) = true
      }
    }
    for (y <- 0 until newS.size; x <- 0 until code.size) {
      if (newS(y) == code(x) && !usedC(x) && !usedS(y)) {
        white += 1
        usedC(x) = true
        usedS(y) = true
      }
    }
    (black, white)
  }

  def getS(): Array[Array[Int]] = {
    var s = Array.ofDim[Int](1296, 4)
    var num = 0
    for (a <- 1 to 6; b <- 1 to 6; c <- 1 to 6; d <- 1 to 6) {
      s(num)(0) = a
      s(num)(1) = b
      s(num)(2) = c
      s(num)(3) = d
      num += 1
    }
    s
  }

}