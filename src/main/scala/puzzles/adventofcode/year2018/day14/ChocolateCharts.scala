package puzzles.adventofcode.year2018.day14

import scala.annotation.tailrec

object ChocolateCharts extends App {

  val NumRecipieAttempts = 190221

  // Pre-compute some values to avoid having to krep re-calculating unnecessarily
  val numRecipieAttemptsAsDigits = asDigits(NumRecipieAttempts)
  val numRecipieAttemptsAsDigitsLen = numRecipieAttemptsAsDigits.length

  // I'm using the mostRecentScores variable for part 2 only (it tracks the last part of the tail of the scoreboard)
  case class State(scoreboard: Vector[Int],
                   elf1Pos: Int,
                   elf2Pos: Int,
                   mostRecentScores: Vector[Int] = Vector.empty[Int])

  // State transition
  def updateState(state: State): State = {

    val e1 = state.elf1Pos
    val e2 = state.elf2Pos
    val scoreboard = state.scoreboard
    val digits = asDigits(scoreboard(e1) + scoreboard(e2))
    val newScoreboard = scoreboard ++ digits
    val newElf1Pos = rotateByIndex(newScoreboard.length, e1, scoreboard(e1) + 1)
    val newElf2Pos = rotateByIndex(newScoreboard.length, e2, scoreboard(e2) + 1)
    // We might have 1 or 2 new scores added and so need to make sure the tail  includes both
    val newMostRecentScores = newScoreboard.takeRight(numRecipieAttemptsAsDigitsLen + 1)

    State(newScoreboard, newElf1Pos, newElf2Pos, newMostRecentScores)
  }

  val initialState = State(Vector(3, 7), 0, 1)

  /*
    * Part 1
    */

  @tailrec
  def makeRecipiesPart1(initialState: State): State = {
    val updatedState = updateState(initialState)
    if (updatedState.scoreboard.length > NumRecipieAttempts + 10)
      updatedState
    else
      makeRecipiesPart1(updatedState)
  }


  val finalState = makeRecipiesPart1(initialState)
  val part1Answer = finalState.scoreboard.slice(NumRecipieAttempts, NumRecipieAttempts + 10).mkString
  assert(part1Answer == "1191216109")

  /*
    * Part 2
    */

  @tailrec
  def makeRecipiesPart2(initialState: State): Int = {
    val updatedState = updateState(initialState)
    // Look for the pattern in the most recent scores, which could be in one of two positions
    val ourPatternPos = updatedState.mostRecentScores.indexOfSlice(numRecipieAttemptsAsDigits)
    if (ourPatternPos != -1)
      updatedState.scoreboard.length - 1 - numRecipieAttemptsAsDigitsLen + ourPatternPos
    else
      makeRecipiesPart2(updatedState)
  }

  // This takes about 10 seconds
  val part2Ans = makeRecipiesPart2(initialState)
  assert(part2Ans == 20268576)

  /*
    * Utilities
    */
  def rotateByIndex(listSize: Int, currIndex: Int, moveBy: Int): Int =
    (currIndex + moveBy) % listSize

  def asDigits(score: Int): Vector[Int] =
    score.toString.map(_.asDigit).toVector
}