package exercises02

object Counter {
  private val wordsSplitter     = "[\\s.,!?:\n\t\r()]+"
  private val numbersSplitter   = "[\\s!?:\\n\\t\\r]"
  private val englishWordsRegex = "([A-z]+[-|'][A-z]+)|([A-z]+)".r
  private val numbersRegex      = "(\\d+)|(\\d+).(\\d+)|(\\d+),(\\d+)".r

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = splitAndFilterAndCollectToMap(text, wordsSplitter, word => !word.isBlank)

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] =
    splitAndFilterAndCollectToMap(text, wordsSplitter, word => englishWordsRegex.matches(word))

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] =
    splitAndFilterAndCollectToMap(text, numbersSplitter, number => numbersRegex.matches(number))

  private def splitAndFilterAndCollectToMap(text: String, splitter: String, filter: String => Boolean) =
    text
      .split(splitter)
      .filter(filter)
      .groupMapReduce(_.toLowerCase)(_ => 1)(_ + _)
}
