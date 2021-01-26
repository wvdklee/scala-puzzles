package puzzles.adventofcode.year2018.day8

object MemoryManeuver extends App {

  val licence: List[Int] =
    scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day8/input.txt").mkString
      .split(" ")
      .toList
      .map(_.toInt)

  /*
   ********************************************************************************
   *                                                                              *
   * PART 1                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * A tree node has metadata and children
   */
  case class Node(metadata: List[Int], children: List[Node])

  /*
   * Parse the input list to construct the tree. Each node is identified by a header (of length 2) which
   * needs to be skipped past to get to its children. We need to track the index position of the input so that we
   * know where the next node starts
   */
  val HeaderLen = 2
  def buildTree(nodeContents: List[Int], pos: Int): (Node, Int) = {
    /*
     * Header for this node
     */
    val (numChildren, numMetadata) = (nodeContents.head, nodeContents(1))
    if (numChildren == 0) {
      /*
       * Leaf node
       */
      val positionOfNextNode = HeaderLen + numMetadata
      val metadata = nodeContents.slice(HeaderLen, positionOfNextNode)
      val leafNode = Node(metadata, Nil)
      (leafNode, positionOfNextNode)
    }
    else {
      /*
       * Tree node, meaning we need to recursively cycle through each child. The position in the licence
       * is tracked so we known when the next child starts
       */
      val initialChildrenState = (List[Node](), HeaderLen)
      val (nodeChildren, updatedPos) =
        (1 to numChildren).foldLeft(initialChildrenState){case ((children, accPos), _) =>
          val (child, newPos) = buildTree(nodeContents.drop(accPos), accPos)
          (child :: children, newPos + accPos)
        }
      val positionOfNextNode = updatedPos + numMetadata
      val metadata = nodeContents.slice(updatedPos, positionOfNextNode)
      val treeNode = Node(metadata, nodeChildren)
      (treeNode, positionOfNextNode)
    }
  }

  /*
   * Construct the tree
   */
  val tree = buildTree(licence, 0)._1

  /*
   * Tree walk that collects all the nodes into a list
   */
  def treeWalk(root: Node, nodes: List[Node]): List[Node] =
    root ::
      (if (root.children.isEmpty)
        nodes
      else
        root.children.flatMap(treeWalk(_, nodes)))


  /*
   * We are interested in the sum of the metadata items
   */
  val sumOfAllMetadata = treeWalk(tree, Nil).flatMap(_.metadata).sum
  assert(sumOfAllMetadata == 41454)

  /*
   ********************************************************************************
   *                                                                              *
   * PART 2                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * Convert the existing tree to an enhanced tree, which has an additional value property
   * Whereas previously the order of the children didn't matter, this time it does because we are
   * indexing into the list. The list is currently the wrong way round (because of prepending to lists)
   */
  case class EnhancedNode(metadata: List[Int], value: Int, children: List[EnhancedNode])

  def treeConvert(root: Node): EnhancedNode = {
    val metadata = root.metadata
    if (root.children.isEmpty)
      EnhancedNode(metadata, metadata.sum, Nil)
    else {
      val children = root.children.map(treeConvert).reverse
      val calculatedValue = metadata.foldLeft(0) { case (accValue, idx) =>
        val currValue = if (idx > 0 && idx <= children.length) children(idx - 1).value else 0
        accValue + currValue
      }
      EnhancedNode(metadata, calculatedValue, children)
    }
  }

  val enhancedTree = treeConvert(tree)
  assert(enhancedTree.value == 25752)
}
