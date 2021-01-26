package puzzles.adventofcode.year2018.day3

import scala.collection.mutable

object NoMatterHowYouSliceIt extends App {

  /*
   * Create a model for a Claim
   */
  case class Claim(id: Int, offset: Offset, dimensions: Dimensions)
  object Claim {
    /*
     * Define a constructor that creates a claim from the formatted input string
     */
    def apply(claim: String): Claim = {
      val Array(idPart, measurementsPart) = claim.replaceAll(" ", "").split("@")
      val Array(offsetPart, dimensionsPart) = measurementsPart.split(":")
      val Array(leftOffset, topOffset) =  offsetPart.split(",")
      val Array(width, height) = dimensionsPart.split("x")

      new Claim(idPart.substring(1).toInt,Offset(leftOffset.toInt, topOffset.toInt), Dimensions(width.toInt, height.toInt))
    }
  }
  case class Offset(left: Int, top: Int)
  case class Dimensions(width: Int, height: Int)

  /*
   * Read in the claims
   */
  val claims: List[Claim] =
    scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day3/input.txt").getLines().toList.map(Claim(_))

  /*
   * Each claim is a rectangular fragment of the cloth, represented by a map (x, y)-> id, where (x, y) are the cartesian co-ordinates and id identifies the claimer
   */
  val claimedFragments: List[Map[(Int, Int), String]] = claims.map{case Claim(id, Offset(left, top), Dimensions(width, height)) =>
    (for {
      x <- left until left + width
      y <- top until top + height
    } yield ((x, y), id.toString)).toMap
  }

  /*
   * The claimed fragments are successively layered on top of each other to create a final image of the fabric and where each
   * claim lies. Points that are already occupied by another layer are marked with an 'x' so it is clear where the overlays are.
   */
  val allClaimedFragments = mutable.Map[(Int, Int), String]()
  /*
   * A recursive function is used to perform the layering
   */
  def layerClaimedFragmentsOnTopOfEachOther(claimedFragments: List[Map[(Int, Int), String]]): Unit =
    if (claimedFragments.nonEmpty) {
      claimedFragments.head.map{case((x, y), id)=>
        allClaimedFragments.put((x,y), if (allClaimedFragments.get((x,y)).isDefined) "x" else id)}
      layerClaimedFragmentsOnTopOfEachOther(claimedFragments.tail)
    }
  layerClaimedFragmentsOnTopOfEachOther(claimedFragments)


  /*
   ********************************************************************************
   *                                                                              *
   * PART 1                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * This involves finding the size of the overlaps i.e. the numer of 'x' characters
   */
  val sizeOfOverlappingClaims = allClaimedFragments.values.count(_ == "x")
  assert(sizeOfOverlappingClaims == 111935)


  /*
   ********************************************************************************
   *                                                                              *
   * PART 2                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * Finding the claim with no overlaps simply involves looking at the coordinates of each claimed fragment in the overlay to
   * see whether it has any overlaps i.e 'x' points. The first claimed fragment with no such 'x' points is the answer
   */
  val nonOverlappingClaim = claimedFragments.find{ claimedFragment=>{
    val contentsOfOverlayAtThisFragmentsPositions = claimedFragment.map{case((x,y), _)=>allClaimedFragments((x,y))}
    /*
     * If we collect the values at this fragments positions we would expect to have no overlaps
     */
    !contentsOfOverlayAtThisFragmentsPositions.toSet.contains("x")
  }}
  /*
   * The value of any position in this fragment's claim will contain its id
   */

  val idOfNonOverlappingCLaim = nonOverlappingClaim.get.values.head.toInt
  assert(idOfNonOverlappingCLaim == 650)
}
