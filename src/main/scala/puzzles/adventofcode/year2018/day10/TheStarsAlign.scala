package puzzles.adventofcode.year2018.day10

import scala.annotation.tailrec

object TheStarsAlign extends App {

	/*
	 * The points form a message when their boundary starts expanding following a period of contraction.
	 * The difficult part of this question for me was realising that this was the condition to look for
	 */

	case class Point(x: Int, y: Int, velx: Int, vely: Int)
	case class Boundary(minx: Int, maxx: Int, miny: Int, maxy: Int)
	object Boundary {
		val Initial = Boundary(Integer.MIN_VALUE, Integer.MAX_VALUE, Integer.MIN_VALUE, Integer.MAX_VALUE)
	}

	/*
	 * The point data can be extracted by parsing the numbers straight out of the txt file
	 */
	val rawInput = scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day10/input.txt").getLines()
	val rawData: Iterator[Vector[Int]] = rawInput.map(line => "(-)*(\\d)+".r.findAllIn(line).map(_.toInt).toVector)
	val initialPoints: Vector[Point] = rawData.map { case Vector(x, y, vx, vy) => Point(x, y, vx, vy) }.toVector


	/**
	 * Look at the sky after each second to see how the points are moving. They contract into a message and then
	 * start diverging again. The last set of points before divergence can be decoded into the message
	 * @param seconds The number of seconds that has elapsed
	 * @param points The set of points at this time
	 * @param boundary The bounds of the points at this time
	 * @return A new set of points, their boundary and the number of seconds after they were calculated (the answer to the puzzle)
	 */
	@tailrec
	def lookAtTheSkyUntilAMessageAppears(seconds: Int, points: Vector[Point], boundary: Boundary): (Vector[Point], Boundary, Int) = {
		val newPositions: Vector[Point] = calculateNewPositions(seconds)
		val newBoundary: Boundary = calculateBoundary(newPositions)

		if (isBoundaryIncreasing(boundary, newBoundary))
			(points, boundary, seconds)
		else
			lookAtTheSkyUntilAMessageAppears(seconds + 1, newPositions, newBoundary)
	}

	/*
	 * Start the recursion and decode the resulting message
	 */
	val (alignedStarts, boundary, timeTaken) = lookAtTheSkyUntilAMessageAppears(0, initialPoints, Boundary.Initial)
	println(s"After ${timeTaken-1} seconds...")
	println(decodeMessage(alignedStarts, boundary))


	/*
	 * Utility functions
	 */

	def calculateBoundary(points: Vector[Point]): Boundary = {
		val ys = points.map(_.y)
		val xs = points.map(_.x)
		Boundary(smallest(xs), largest(xs), smallest(ys), largest(ys))
	}

	def isBoundaryIncreasing(old: Boundary, curr: Boundary): Boolean =
		curr.miny < old.miny || curr.maxy > old.maxy ||
			curr.minx < old.minx || curr.maxx > old.maxx

	def calculateNewPositions(seconds: Int): Vector[Point] =
		initialPoints.map { case Point(x, y, vx, vy) =>
			Point(x + seconds * vx, y + seconds * vy, vx, vy)
		}


	/*
	 * Converts a sparse points array into a displayable grid of points
	 */
	def decodeMessage(points: Vector[Point], boundary: Boundary): String = {

		// Convert to positive cartesian coordinates
		val normalizedPoints = normalizePoints(points)

		// Only interested in points inside our boundary
		val xBound = Math.abs(boundary.maxx - boundary.minx)
		val yBound = Math.abs(boundary.maxy - boundary.miny)

		// Create a display grid as a string for the bounded area by filling in the gaps between points
		val positions: Set[(Int, Int)] = normalizedPoints.map(point => (point.x, point.y)).toSet
		val matrix = for {
			y <- 0 to yBound
			x <- 0 to xBound
			cellIsOccupied = positions.contains((x, y))
			chr = if (cellIsOccupied) '#' else '.'
		} yield chr

		val rows: Vector[String] = matrix.toVector.sliding(xBound + 1, xBound + 1).map(row => row.mkString).toVector
		rows.mkString("\n")
	}

	/*
	 * Translate all points so they are positive values. This means looking at the smallest
	 * points and shifting them so that the lowest value can't be less than zero
	 */
	def normalizePoints(points: Vector[Point]): Vector[Point] = {
		def offset(x: Int): Int = if (x >= 0) -x else Math.abs(x)
		val smallestX = smallest(points.map(_.x))
		val smallestY = smallest(points.map(_.y))
		points.map(point => point.copy(x = point.x + offset(smallestX), point.y + offset(smallestY)))
	}

	def smallest(points: Vector[Int]): Int =
		points.reduceLeft(_ min _)

	def largest(points: Vector[Int]): Int =
		points.reduceLeft(_ max _)

}



