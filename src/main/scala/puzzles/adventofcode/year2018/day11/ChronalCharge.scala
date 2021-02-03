package puzzles.adventofcode.year2018.day11

object ChronalCharge extends App {

	case class Square(x: Int, y: Int, size: Int, totalPower: Int)

	val serialNumber = 2866
	val gridSize = 300

	/*
	 * Part 1
	 */
	def squareWithTheMostPower(squareSize: Int): Square = {
		val allSquares: Seq[Square] =
			allCells(gridSize).collect{case (x, y) if squareFitsIntoGrid(x, y, squareSize) =>
				Square(x, y, squareSize, calculateTotalPowerOfSquare(x, y, squareSize))
			}
		allSquares.maxBy(_.totalPower)
	}
	assert(squareWithTheMostPower(squareSize = 3) == Square(20, 50, 3, 30))

	/*
	 * Part 2
	 */

	/*
	 * Attempting to extend part 1 using brute force won't work as there is just too much data to process.
	 * I tried various caching techniques but abandoned them in the end. I managed to find an efficient algorithm
	 * used in image processing to sum the contents of sub-rectangles within a grid:
	 * https://en.wikipedia.org/wiki/Summed-area_table
	 * https://www.codeproject.com/Articles/441226/Haar-feature-Object-Detection-in-Csharp (the example is helpful)
	 *
	 */
	val summedAreaTable: Map[(Int, Int), Int] = {
		val empty = Map.empty[(Int, Int), Int]
		def combine(currTable: Map[(Int, Int), Int], position: (Int, Int)): Map[(Int, Int), Int] = {
			val (x, y) = (position._1, position._2)
			val tableEntry = powerAt(x, y) +
				currTable.getOrElse((x, y - 1), 0) + currTable.getOrElse((x - 1, y), 0) - currTable.getOrElse((x - 1, y - 1), 0)
			currTable.updated((x, y), tableEntry)
		}
		allCells(gridSize).foldLeft(empty)(combine)
	}

	def powerOfSquare(squareSize: Int)(x: Int, y: Int): Int = {
		val a = (x - 1, y - 1)
		val b = (x + squareSize -1, y - 1)
		val c = (x - 1, y + squareSize - 1)
		val d = (x + squareSize - 1, y + squareSize - 1)
		summedAreaTable.getOrElse(d, 0) + summedAreaTable.getOrElse(a, 0) - summedAreaTable.getOrElse(b, 0) - summedAreaTable.getOrElse(c, 0)
	}

	/*
	 * Now we can calculate the power of all squares efficiently and pick the largest
	 */
	def maxPoweredSquare: Square = {
		val allValues: Seq[Square] = allCells(gridSize).map{ case (x,y) => getSquareFromThisPointThatHasTheMostPower(x, y)}
		allValues.maxBy(_.totalPower)
	}
	assert(maxPoweredSquare == Square(238,278,9,88))

	/*
	 * Helper functions
	 */

	def getSquareFromThisPointThatHasTheMostPower(x: Int, y: Int): Square = {
		val allSquares: Seq[Square] = (1 to Math.min(gridSize - x + 1, gridSize - y + 1))
			.filter(squareSize =>  squareFitsIntoGrid(x, y, squareSize))
			.map(squareSize => Square(x, y, squareSize, powerOfSquare(squareSize)(x, y)))
		allSquares.maxBy(_.totalPower)
	}


	def squareFitsIntoGrid(x: Int, y: Int, squareSize: Int): Boolean = {
		val xSide = x + squareSize - 1
		val ySide = y + squareSize - 1
		xSide <= gridSize && ySide <= gridSize
	}

	def allCells(gridSize: Int): Seq[(Int, Int)] = {
		for {
			x <- 1 to gridSize
			y <- 1 to gridSize
		} yield (x, y)
	}

	def powerAt(x: Int, y: Int): Int = {
			val rackId = x + 10
			val initialPower = rackId * y
			val level1Power = initialPower + serialNumber
			val level2Power = level1Power * rackId
			val hundredsDigit = getHundredsDigit(level2Power)
			val finalPowerLevel = hundredsDigit - 5

			finalPowerLevel
	}

	def getHundredsDigit(n: Int): Int = {
		val asString = Math.abs(n).toString
		if (asString.length > 2)
			Integer.parseInt(asString.reverse(2).toString)
		else
			0
	}

	def calculateTotalPowerOfSquare(x: Int, y: Int, squareSize: Int): Int = {
		val xSide = x + squareSize - 1
		val ySide = y + squareSize - 1
		val cellPowers = for {
			row <- y to ySide
			col <- x to xSide
		} yield powerAt(col, row)
		cellPowers.sum
	}
}

