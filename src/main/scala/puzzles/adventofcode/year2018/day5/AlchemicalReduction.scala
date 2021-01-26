package puzzles.adventofcode.year2018.day5

object AlchemicalReduction extends App {

  val inputPolymer: String =
    scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day5/input.txt").mkString

  /*
   * Tests to see whether 2 units have the same but opposite polarity and therefore cancel.
   */
  def unitsHaveSameTypeButOppositePolarity(a: Char, b: Char): Boolean =
    a != b && (a.toUpper == b || a.toLower == b)

  /*
   * Scan through the polymer checking neighbouring units along the way for units that cancel. In practice this means
   * creating a new polymer by examining pairs and only including the pair in the new polymer if they don't cancel
   */

  def reducedPolymer(polymer: String): String = {
    val reducedPolymer = polymer.foldLeft(List[Char]()) { case (units, currUnit) =>
      units match {
        case Nil => currUnit :: units
        case nextUnit :: remainingUnits => if (unitsHaveSameTypeButOppositePolarity(nextUnit, currUnit)) remainingUnits else currUnit :: units
      }
    }
    reducedPolymer.mkString.reverse
  }

  /*
   ********************************************************************************
   *                                                                              *
   * PART 1                                                                       *
   *                                                                              *
   ********************************************************************************
   */
  val numPolymerUnits = reducedPolymer(inputPolymer).length
  assert(numPolymerUnits == 11242)

  /*
   ********************************************************************************
   *                                                                              *
   * PART 2                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  val allPossibleTypes = inputPolymer.toLowerCase.toSet
  /*
   * Look at reduced polymers that are calculated when the input polymer has a type removed
   */
  val lengthOfPolymersWhenTypesAreRemoved = for {
    polymerType <- allPossibleTypes
    polymerWithoutType = inputPolymer.replaceAll(polymerType.toString, "").replaceAll(polymerType.toString.toUpperCase, "")
    reducedPolymerWithoutType = reducedPolymer(polymerWithoutType)
  } yield reducedPolymerWithoutType.length

  val shortestCollapsablePolymer = lengthOfPolymersWhenTypesAreRemoved.min
  assert(shortestCollapsablePolymer == 5492)
}
