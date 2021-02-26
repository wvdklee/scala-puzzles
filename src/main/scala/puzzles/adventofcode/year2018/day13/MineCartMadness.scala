package puzzles.adventofcode.year2018.day13

import scala.annotation.tailrec

object MineCartMadness extends App {

	type Direction = Int
	val Left = 0
	val Straight = 1
	val Right = 2
	val Stationary = 3

	val Carts = Set('^', 'v', '>', '<')
	val TrackPieces = Set('|', '-', '/', '\\', '+') ++ Carts

	val rawInput = scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day13/input.txt").getLines().toList
	val initialTrack: Map[Pos, Char] = (for {
		(row, y)      <- rawInput.zipWithIndex
		(contents, x) <- row.zipWithIndex
		if TrackPieces.contains(contents)
	} yield Pos(x, y) -> contents).toMap

	/*
	 * We can derive the empty track, since we know that cars are
	 * currently on a straight section to start with
	 */
	val emptyTrack = initialTrack.map{
		case (pos, path) if path == '>' || path == '<' => (pos, '-')
		case (pos, path) if path == '^' || path == 'v' => (pos, '|')
		case _@cell => cell
	}

	/*
	 * Data model, including an ordering of carts from top to bottom , left to right
	 */
	case class Pos(x: Int, y: Int)
	case class Cart(pos: Pos, orientation: Char, lastTurn: Direction)
	object Cart {
		implicit val cartOrdering: Ordering[Cart] = new Ordering[Cart] {
			def compare(c1: Cart, c2: Cart): Int = {
				val yCompare = c1.pos.y compare c2.pos.y
				if (yCompare == 0)
					c1.pos.x compare c2.pos.x
				else
					yCompare
			}
		}
	}

	/*
	 * Part 1
	 */
	case class State(tick: Int, carts: Seq[Cart])
	@tailrec
	def moveCartsUntilThereIsACollision(state: State): State = {
		val cartsHaveCrashed = state.carts.map(_.pos).distinct.size < state.carts.length
		if (cartsHaveCrashed)
			state
		else {
			val newCarts = state.carts.map(moveCart)
			val newState = State(state.tick + 1, newCarts)
			moveCartsUntilThereIsACollision(newState)
		}
	}

	val initialState = State(0, findCarts(initialTrack))
	val cartsAfterCollision: State = moveCartsUntilThereIsACollision(initialState)
	val collisionPosition = findCollisionDetails(cartsAfterCollision.carts).head._1
	assert(collisionPosition == Pos(32, 99))

	/*
	 * Part 2 - this defeated me, I seem to end up with 2 cars that crash into each other to leave none.
	 */
	@tailrec
	def moveCartsUntilOnlyOneRemains(carts: Seq[Cart]): Seq[Cart] = {
		if (carts.size == 1)
			carts
		else {
			val movedCarts = moveCarts(carts, Seq()).sorted
			moveCartsUntilOnlyOneRemains(movedCarts)
		}
	}

	@tailrec
	def moveCarts(cartsIn: Seq[Cart], cartsOut: Seq[Cart]): Seq[Cart] = {
		if (cartsIn.isEmpty)
			cartsOut
		else {
			val newCart = moveCart(cartsIn.head)
			val crashed = (cartsIn.tail ++ cartsOut).map(_.pos).contains(newCart.pos)
			if (crashed)
				moveCarts(cartsIn.tail, cartsOut.filter(cart => cart.pos != newCart.pos))
			else
				moveCarts(cartsIn.tail, newCart +: cartsOut)
		}
	}
//	val finalState = moveCartsUntilOnlyOneRemains(findCarts(initialTrack).sorted)

	/*
	 * Helper functions
	 */

	// Bonus method for displaying the track
	def showTrack(track: Map[Pos, Char], cars: Seq[Cart] = Seq()): String = {
		val trackWithCars = cars.foldLeft(track){case (currTrack, car) => currTrack.updated(car.pos, car.orientation)}
		val trackWidth = trackWithCars.keySet.maxBy(_.x).x
		val trackHeight = trackWithCars.keySet.maxBy(_.y).y
		(0 to trackHeight).map(y =>
			(0 to trackWidth).map(x =>
				trackWithCars.getOrElse(Pos(x, y), ' ')).mkString).mkString("\n")
	}

	def findCollisionDetails(carts: Seq[Cart]): Map[Pos, Seq[Cart]] =
		carts.groupBy(_.pos).filter{case (_, carts) => carts.length > 1}

	def findCarts(track: Map[Pos, Char]): Seq[Cart] =
		track.collect { case (Pos(x, y), contents) if Carts.contains(contents) =>
			Cart(Pos(x, y), contents, Stationary)}.toSeq.sorted

	def findNextPathForCart(cart: Cart): Char = cart match {
			case Cart(Pos(x, y), '>', _) => emptyTrack(Pos(x + 1, y))
			case Cart(Pos(x, y), '<', _) => emptyTrack(Pos(x - 1, y))
			case Cart(Pos(x, y), '^', _) => emptyTrack(Pos(x, y - 1))
			case Cart(Pos(x, y), 'v', _) => emptyTrack(Pos(x, y + 1))
		}

	def moveCart(cart: Cart): Cart = {
		(findNextPathForCart(cart), cart) match {
			// Straight line
			case ('-', Cart(Pos(x, y), '>', lastTurn)) => Cart(Pos(x + 1, y), '>', lastTurn)
			case ('-', Cart(Pos(x, y), '<', lastTurn)) => Cart(Pos(x - 1, y), '<', lastTurn)
			// Top left
			case ('/', Cart(Pos(x, y), '<', lastTurn)) => Cart(Pos(x - 1, y), 'v', lastTurn)
			case ('/', Cart(Pos(x, y), '^', lastTurn)) => Cart(Pos(x, y - 1), '>', lastTurn)
			// Bottom right
			case ('/', Cart(Pos(x, y), '>', lastTurn)) => Cart(Pos(x + 1, y), '^', lastTurn)
			case ('/', Cart(Pos(x, y), 'v', lastTurn)) => Cart(Pos(x, y + 1), '<', lastTurn)
			// Sides
			case ('|', Cart(Pos(x, y), '^', lastTurn)) => Cart(Pos(x, y - 1), '^', lastTurn)
			case ('|', Cart(Pos(x, y), 'v', lastTurn)) => Cart(Pos(x, y + 1), 'v', lastTurn)
			// Top Right
			case ('\\', Cart(Pos(x, y), '>', lastTurn)) => Cart(Pos(x + 1, y), 'v', lastTurn)
			case ('\\', Cart(Pos(x, y), '^', lastTurn)) => Cart(Pos(x, y - 1), '<', lastTurn)
			// Bottom Left
			case ('\\', Cart(Pos(x, y), '<', lastTurn)) => Cart(Pos(x - 1, y), '^', lastTurn)
			case ('\\', Cart(Pos(x, y), 'v', lastTurn)) => Cart(Pos(x, y + 1), '>', lastTurn)

			// Intersection
			case ('+', Cart(Pos(x, y), '>', lastTurn)) =>
				if (lastTurn == Left) Cart(Pos(x + 1, y), '>', Straight)
				else if (lastTurn == Straight) Cart(Pos(x + 1, y), 'v', Right)
				else Cart(Pos(x + 1, y), '^', Left)
			case ('+', Cart(Pos(x, y), '<', lastTurn)) =>
				if (lastTurn == Left) Cart(Pos(x - 1, y), '<', Straight)
				else if (lastTurn == Straight) Cart(Pos(x - 1, y), '^', Right)
				else Cart(Pos(x - 1, y), 'v', Left)
			case ('+', Cart(Pos(x, y), '^', lastTurn)) =>
				if (lastTurn == Left) Cart(Pos(x, y - 1), '^', Straight)
				else if (lastTurn == Straight) Cart(Pos(x, y - 1), '>', Right)
				else Cart(Pos(x, y - 1), '<', Left)
			case ('+', Cart(Pos(x, y), 'v', lastTurn)) =>
				if (lastTurn == Left) Cart(Pos(x, y + 1), 'v', Straight)
				else if (lastTurn == Straight) Cart(Pos(x, y + 1), '<', Right)
				else Cart(Pos(x, y + 1), '>', Left)
		}
	}
}
