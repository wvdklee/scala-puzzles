package puzzles.adventofcode.year2018.day6


object ChronalCoordinates extends App {

  /*
   * Read the input coordinates
   */
  case class Coord(x: Int, y: Int)

  val coordinates =
    scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day6/input.txt").getLines
      .map(_.trim.split(","))
      .map{case Array(x, y) => Coord(x.trim.toInt, y.trim.toInt)}
      .toList

  /*
   * The manhattan distance between coordinates a and b
   */
  def manhattanDistance(a: Coord, b: Coord): Int =
    Math.abs(b.x - a.x) + Math.abs(b.y - a.y)

  /*
   ********************************************************************************
   *                                                                              *
   * PART 1                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * The bounds of the visible grid are defined by the largest x and y coordinates of a location
   */
  val largestX = coordinates.map(_.x).max
  val largestY = coordinates.map(_.y).max

  /*
   * For each location in the grid, calculate the manhattan distances to each input coordinate and store the smallest of these at the location.
   * This is the grid shown in the question. The smallest distance for a given location is calculated by sorting the distances and
   * looking at the head of the resulting list (maybe not the most efficient way of doing it). One complication is when 2 or more
   * coordinates are the same distance from the location. In this case the value stored at the location is None to represent this (which mimics the '.' used
   * in the example in the question). This means the location's contents are of type Option so that the value represents either the distance to the closest
   * coordinate (the Some value) or None if the location is equally close to multiple coordinates
   */
  val gridDistancesToEachCoord = (
    for {
      x<-0 to largestX
      y<-0 to largestY

      /*
       *The distances from this grid coordinate (x, y) to each of the input coordinates (in order of shortest distance first)
       */
      distances: List[(Coord, Int)] =
        coordinates.map(coord=>(coord, manhattanDistance(coord, Coord(x, y)))).sortBy(_._2)
      /*
       * The shortest distance (value to be stored at the location) is the coordinate at the head of the list. However, a check needs
       * to be made to see if the next closest is at the same distance (since then the value stored will be None)
       */
      shortestDistance = distances match {
        case (_, shortest) :: (_, nextShortest) :: _ if shortest == nextShortest => None
        case (coord1, _) :: _ => Some(coord1)
        case _ => None
      }
    } yield (Coord(x,y), shortestDistance)).toList

  /*
   * Any coordinate that appears on the boundary of the known grid is unbounded i.e. represents an infinite area
   */
  val unboundedCoordinates: Set[Coord] =
    gridDistancesToEachCoord.collect{case(Coord(_, y), Some(contents)) if y == 0  => contents}.toSet ++
    gridDistancesToEachCoord.collect{case(Coord(_, y), Some(contents)) if y == largestY  => contents}.toSet ++
    gridDistancesToEachCoord.collect{case(Coord(x, _), Some(contents)) if x == 0  => contents}.toSet ++
    gridDistancesToEachCoord.collect{case(Coord(x, _), Some(contents)) if x == largestX  => contents}.toSet

  /*
   * Only interested in the coordinates that are bounded
   */
  val allBoundedCoordinates = gridDistancesToEachCoord.collect{case(_, Some(coord)) if !unboundedCoordinates.contains(coord)=> coord}

  /*
   * The area surrounding the bounded coordinates is merely a matter of counting the location values that contain the bounded coordinates
   */
  val boundedAreas = allBoundedCoordinates.groupBy(identity).map{case(coord, counts)=>(coord, counts.length)}

  /*
   * The largest of the bounded areas can be obtained by sorting the areas and taking the last value
   */
  val largestBoundedArea = boundedAreas.toList.sortBy(_._2).reverse.head._2
  assert(largestBoundedArea == 4589)


  /*
   ********************************************************************************
   *                                                                              *
   * PART 2                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  val locationsWithinDesiredRegion =
    for {
      x<-0 to largestX
      y<-0 to largestY
      sumOfDistances = coordinates.map(manhattanDistance(_, Coord(x, y))).sum
      if sumOfDistances < 10000
    } yield sumOfDistances

  val sizeOfLargestDesiredRegion = locationsWithinDesiredRegion.length
  assert(sizeOfLargestDesiredRegion == 40252)
}
