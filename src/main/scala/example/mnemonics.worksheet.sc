/**
 * Task
 *
 * Once upon a time, before smartphones, phone keys had mnemonics assigned to them.
 *
 * val mnemonics = Map('2' -> "ABC”, '3' -> "DEF”, '4' -> "GHI”, '5' -> ”JKL”, '6' -> "MNO”, '7' -> ”PQRS”, '8' -> ”TUV”, '9' -> "WXYZ”)
 *
 * Assume you are given a dictionary words as a list of words. Design a method encode such that
 * encode (phoneNumber)
 * produces all phrases of words that can serve as mnemonics for the phone number.
 *
 * Example: The phone number "7225247386" should have the mnemonic Scala is fun as one element of the set of solution phrases.
 */

class Coder(words: List[String]):
  val mnemonics = Map('2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ")

  /** Maps a letter to the digit it represents */
  private val charCode: Map[Char, Char] =
    for (digit, str) <- mnemonics; ltr <- str yield ltr -> digit

  /** Maps a word to the digit string it can represent */
  private def wordCode(word: String): String = word.toUpperCase.map(charCode)

  /** Maps a digit string to all words in the dictionary that represent it */
  private val wordsForNum: Map[String, List[String]] =
    words.groupBy(wordCode).withDefaultValue(Nil)

  /** All ways to encode a number as a list of words */
  def encode(number: String): Set[List[String]] =
    if number.isEmpty then Set(Nil)
    else
      for
        splitPoint <- (1 to number.length).toSet
        word <- wordsForNum(number.take(splitPoint))
        rest <- encode(number.drop(splitPoint))
      yield word :: rest

val coder = Coder(List("Scala", "Python", "Ruby", "C", "rocks", "socks", "works", "packs"))
coder.encode("7225276257").map(_.mkString(" "))