package puzzles.adventofcode.year2018.day1


object ChronalCalibrationPart extends App {

  val frequencyChanges =
    scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day1/input.txt")
      .getLines()
      .toList
      .map(_.toInt)

  /*
   ********************************************************************************
   *                                                                              *
   * PART 1                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  val resultingFrquency = frequencyChanges.sum
  assert(resultingFrquency == 490)

  /*
   ********************************************************************************
   *                                                                              *
   * PART 2                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * Work through the frequency changes using a recursive loop, maintaining state as we go along, until our exit
   * condition (we have seen a resulting frequency before) has been met.
   */

  /*
   * First, define the state that will be tracked
   */
  case class State(
    resultingFrequenciesSeenBefore: Set[Int],
    resultingFrequency: Int,
    remainingFrequencyChanges: List[Int]
  )

  /*
   * Now define the recursive loop that will work through the changing frequencies, calculating the
   * resulting frequencies until one has been seen before
   */
  def firstFrequencyReachedTwice(currentState: State): Int =
    if (currentState.resultingFrequenciesSeenBefore.contains(currentState.resultingFrequency))
      currentState.resultingFrequency
    else {
      /*
       * Reset the changed frequency list if it has been exhausted
       */
      val cyclicFrequencyList =
        if (currentState.remainingFrequencyChanges.isEmpty)
          frequencyChanges
        else
          currentState.remainingFrequencyChanges
      /*
       * Calculate the new state
       */
      val newState = State(
        resultingFrequenciesSeenBefore = currentState.resultingFrequenciesSeenBefore + currentState.resultingFrequency,
        resultingFrequency = currentState.resultingFrequency + cyclicFrequencyList.head,
        remainingFrequencyChanges = cyclicFrequencyList.tail
      )
      firstFrequencyReachedTwice(newState)
    }
  /*
   * Start processing the frequency changes
   */
  val initialState = State(
    resultingFrequenciesSeenBefore = Set(),
    resultingFrequency = 0,
    remainingFrequencyChanges = frequencyChanges)

  val finalState = firstFrequencyReachedTwice(initialState)
  assert(finalState == 70357)
}
