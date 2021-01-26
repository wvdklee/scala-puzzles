package puzzles.adventofcode.year2018.day9

import scala.annotation.tailrec

object MarbleMania extends App {

  /*
   * The secret to this is to define your own linked list. I spent forever trying to get the standard collection
   * libraries to work, but could not get past part 2. I even experimented with Stream.continually using filter but
   * got into a mess. It's also important to get the order of your next/prev pointer logic correct! Also, don't forget
   * to annotate the tail recursive functions (another time waster if you don't remember to do this)
   */
  class Marble(val value: Int, var prev: Marble, var next: Marble) {
    def insert(marbleValue: Int): Marble = {
      val marbleToInsert = new Marble(marbleValue, next, next.next)
      next.next.prev = marbleToInsert
      next.next = marbleToInsert
      marbleToInsert
    }
    def remove(): Marble = {
      val marbleToRemove = (0 until 7).foldLeft(this){case(acc, _)=>acc.prev}
      marbleToRemove.prev.next = marbleToRemove.next
      marbleToRemove.next.prev = marbleToRemove.prev
      marbleToRemove
    }
  }
  object Marble {
    def apply(initialValue: Int): Marble = {
      val initialMarble = new Marble(initialValue, null, null)
      initialMarble.prev = initialMarble
      initialMarble.next = initialMarble
      initialMarble
    }
  }

  case class Game(numPlayers: Int, marbleValueThatEndsTheGame: Int) {

    @tailrec
    final def turn(player: Int, playerScores: Map[Int, Long], nextMarbleValue: Int, ring: Marble): Long = {
      if (nextMarbleValue > marbleValueThatEndsTheGame)
        playerScores.values.max
      else {
        if (nextMarbleValue % 23 == 0) {
          val marbleToRemove = ring.remove()
          val currPlayerScore = playerScores.getOrElse(player, 0L) + marbleToRemove.value + nextMarbleValue
          val updatedScores = playerScores.updated(player, currPlayerScore)
          turn(player % numPlayers + 1, updatedScores, nextMarbleValue + 1, marbleToRemove.next)
        } else {
          val marbleToInsert = ring.insert(nextMarbleValue)
          turn(player % numPlayers + 1, playerScores, nextMarbleValue + 1, marbleToInsert)
        }
      }
    }

    def highScore(): Long =
      turn(1, Map(), 1, Marble(0))
  }

  assert(Game(numPlayers = 426, marbleValueThatEndsTheGame = 72058).highScore() == 424112)
  assert(Game(numPlayers = 426, marbleValueThatEndsTheGame = 100*72058).highScore() == 3487352628L)

}
