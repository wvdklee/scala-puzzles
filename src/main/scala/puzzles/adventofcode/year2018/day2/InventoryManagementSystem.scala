package puzzles.adventofcode.year2018.day2

object InventoryManagementSystem extends App {

  val boxIds =
    scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day2/input.txt").getLines().toList

  /*
   ********************************************************************************
   *                                                                              *
   * PART 1                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * Reports whether a string contains a specified number of specific characters. This works by
   * grouping like characters together and checking whether any of the groupings has a size that matches the desired value
   */
  private def stringContainsThisNumberOfRepeatedCharacters(str: String, numRepeats: Int) : Boolean =
    str.groupBy(identity).exists{case(_, repetitions)=>repetitions.length == numRepeats}

  /*
   * Count the number of box ids that have exactly the desired number of repeated characters
   */
  private def numberOfBoxIdsWithThisNumberOfRepeatingCharacters(numRepeats: Int): Int =
    boxIds.count(boxId=>stringContainsThisNumberOfRepeatedCharacters(boxId, numRepeats))

  /*
   * Calculate the checksum as per the instructions
   */
  val checksum = numberOfBoxIdsWithThisNumberOfRepeatingCharacters(2) * numberOfBoxIdsWithThisNumberOfRepeatingCharacters(3)
  assert(checksum == 9633)

  /*
   ********************************************************************************
   *                                                                              *
   * PART 2                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * Works out whether 2 strings differ by exactly one character at the same position.
   * The strings are assumed to be the same length so that characters from each can be assigned a value of 1 if they are different
   * or 0 if they are the same. The result follows if the sum of the 1's and 0's is 1
   */
  private def indexOfDifferingCharacter(x: String, y: String): Option[Int] = {
    val differenceIndicators = x.zip(y).map{case(xchr, ychr)=>if (xchr == ychr) 0 else 1}
    if (differenceIndicators.sum == 1)
      Some(differenceIndicators.indexOf(1))
    else
      None
  }

  /*
   * Rips a character out from the middle of a string
   */
  private def removeCharacterFromString(s: String, idx: Int): String = {
    val (firstPart, secondPart) = s.splitAt(idx)
    firstPart + secondPart.substring(1)
  }

  /*
   * All combinations of boxIds need to be compared with each other and only the ones that differ by one character considered.
   * Note that pairing will give 2 duplicates for each box id and so Sets are used to keeps boxIds distinct
   */
  val condenders = boxIds.toSet
  val correctBoxIds =
    for {
      boxId1 <- condenders
      boxId2 <- condenders
      if boxId1 != boxId2
      differingChrPos = indexOfDifferingCharacter(boxId1, boxId2)
      if differingChrPos.isDefined
  } yield (Set(boxId1, boxId2), differingChrPos.get)

  /*
   * There should only be one contender
   */
  val validBoxIds = correctBoxIds.head
  /*
   * To get to the common parts of the id only one of the ids is required together with the position of of differing character
   */
  val (oneOfTheBoxIds, positionOfDifferingChar) = (validBoxIds._1.head, validBoxIds._2)
  val lettersInCommonBetweenBoxIds = removeCharacterFromString(oneOfTheBoxIds, positionOfDifferingChar)

  assert(lettersInCommonBetweenBoxIds == "lujnogabetpmsydyfcovzixaw")
}
