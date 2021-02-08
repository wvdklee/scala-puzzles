package puzzles.adventofcode.year2018.day12

import scala.annotation.tailrec

object SubteranianSustainability extends App {
	/*
	 * Read in the data. Pots that have plants are recorded as a set of pot numbers rather than a string of
	 * hashes, since the code is much cleaner when this is the case. Likewise, the notes are represented as a
	 * map of boolean flags to a boolean. For example, "#.#.." -> '.' is represented as
	 * (true, false, true, false, false) -> false.
	 * Although a pain to set up the code is much cleaner this way
	 */
	val rawInput = scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day12/input.txt").getLines().toList
	val rawPotStatus = rawInput.head.substring(15)
	val rawNotes = rawInput.drop(2).map(rule => {
		val Array(pattern, pot) = rule.split(" => ")
		pattern -> pot
	}).toMap

	val initialPotsWithPlants: Set[Int] = rawPotStatus.zipWithIndex.collect{ case (contents,pos) if contents == '#' => pos}.toSet
	val notes =
		rawNotes.map{case(pattern, contents) =>
			( pattern(0) == '#',
				pattern(1) == '#',
				pattern(2) == '#',
				pattern(3) == '#',
				pattern(4) == '#') -> (contents == "#")
		}

	/*
	 * When calculating the next generation be sure to consider the 2 pots before pot zero and those
	 * after the last pot.
	 */
	def calculateNextGenerationPotsWithPlants(potsWithPlants: Set[Int]): Set[Int] = {
		val (minPot, maxPot) = (potsWithPlants.minBy(identity), potsWithPlants.maxBy(identity))
		val nextGenPots = for {
			pot <- minPot - 2 to maxPot + 2
			key = (potsWithPlants.contains(pot - 2), potsWithPlants.contains(pot - 1), potsWithPlants.contains(pot), potsWithPlants.contains(pot + 1), potsWithPlants.contains(pot + 2))
			if notes.getOrElse(key, false)
		} yield pot
		nextGenPots.toSet
	}


	/*
	 * Part 1
	 */
	def finalStateOfPots = (1 to 20).foldLeft(initialPotsWithPlants){case (pots, _)  =>
		calculateNextGenerationPotsWithPlants(pots)}
	assert(finalStateOfPots.sum == 3890)


	/*
	 * Part 2
	 */

	/*
	 * It would be next to impossible to brute force the answer. Instead look for a repeating pattern
	 * after each generation and then simply use the same values for all remaining iterations. One pattern
	 * that works is when the next generation has the same number of pots with plants and the sum of the
	 * pot numbers has not changed between generations.
	 */
	case class State(gen: Int, potsWithPlants: Set[Int], differenceInSumOfPotsWithPlants: Int)
	@tailrec
	def lookForRepeatingPattern(currentState: State): State = {
		val nextGenPotsWithPlants = calculateNextGenerationPotsWithPlants(currentState.potsWithPlants)
		val sizeDiff = nextGenPotsWithPlants.sum - currentState.potsWithPlants.sum
		if (nextGenPotsWithPlants.size == currentState.potsWithPlants.size
			&& sizeDiff == currentState.differenceInSumOfPotsWithPlants)
			currentState
		else
			lookForRepeatingPattern(State(currentState.gen + 1, nextGenPotsWithPlants, sizeDiff))
	}
	val initialState = State(0, initialPotsWithPlants, initialPotsWithPlants.sum)
	val repeatingPattern = lookForRepeatingPattern(initialState)

	/*
	 * We can easily now sum up the pots with plants, since all of them after the repeating pattern will be the same
	 */
	val sumOfAllRemainingPots: Long = (50000000000L - repeatingPattern.gen) * repeatingPattern.differenceInSumOfPotsWithPlants
	val totalSum = repeatingPattern.potsWithPlants.sum + sumOfAllRemainingPots

	assert(totalSum == 4800000001087L)
}
