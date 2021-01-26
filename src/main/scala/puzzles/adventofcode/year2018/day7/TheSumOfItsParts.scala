package puzzles.adventofcode.year2018.day7

import scala.collection.{MapView, Set, View}


object TheSumOfItsParts extends App {

  /*
   * A step has dependencies (ignore  timeToComplete for part 1)
   */
  case class Step(step: Char, dependencies: List[Char], timeToComplete: Int = 0)

  /*
   * Create a dependency tree
   */

  val inputData: MapView[Char, List[Char]] =
    scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day7/input.txt").getLines
      .map(line => (line(36), line(5)))
      .toList
      .groupBy(_._1)
      .view.mapValues(_.map(_._2))


  /*
   * Don't forget to process steps that don't have any dependencies i.e. those that are in dependency lists
   * but not in the main dependency list.
   */

  val stepsWithDependencies: Set[Char] = inputData.keySet
  val allMentionedSteps: Set[Char] =  stepsWithDependencies ++ inputData.values.flatten.toSet
  val stepsWithNoDependencies: Set[Char] = allMentionedSteps.diff(stepsWithDependencies)
  val dependencyTree: View[(Char, List[Char])] = inputData ++ stepsWithNoDependencies.map(step=>(step, Nil))


  /*
   ********************************************************************************
   *                                                                              *
   * PART 1                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * Initialise the dependencies as Step objects
   */
  val allSteps: List[Step] = dependencyTree.map{case(step,deps)=>Step(step, deps)}.toList

  /*
   * Function that works through the steps resolving dependencies according to the rules
   */
  def runStep(steps: List[Step], completed: List[Char]): List[Char] = {
    if (steps.isEmpty)
      completed
    else {
      /*
       * Completed steps are those with no dependencies left
       */
      val completedSteps = steps.collect { case Step(step, deps, _) if deps.diff(completed).isEmpty => step }
      /*
       * Of the steps that are available for completion, only one is completed - that with the lowest letter
       */
      val firstStepCompleted: Char = completedSteps.sorted.head
      /*
       *
       */
      val nonCompletedSteps = steps.filter(_.step != firstStepCompleted)
      /*
       * Repeat with the steps minus the completed dependency
       */
      runStep(nonCompletedSteps, firstStepCompleted :: completed)
    }
  }

  /*
   * Start running the steps
   */
  val stepOrder = runStep(allSteps, List())
  assert(stepOrder.mkString.reverse == "GJFMDHNBCIVTUWEQYALSPXZORK")


  /*
   ********************************************************************************
   *                                                                              *
   * PART 2                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  val MinTimeToComplete = 60
  val NumWorkers = 5
  val StepTime: Map[Char, Int] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".zipWithIndex.map{case(letter, index)=> (letter, index  + MinTimeToComplete)}.toMap

  /*
   * Initialise the dependencies as step objects
   */
  val allStepsPart2: List[Step] = dependencyTree.map{case(step,deps)=>Step(step, deps, StepTime(step))}.toList

  def runSteps(steps: List[Step], done: List[Char], second: Int): Int = {
    if (steps.isEmpty)
      second
    else {
      /*
       * Steps with no dependencies can be assigned to workers straight away for completion (in alphabetical order)
       */
      val workerSteps =
        steps.filter{step => step.dependencies.diff(done).isEmpty}.take(NumWorkers).sortBy(_.step)

      /*
       * Check for worker steps that have already been completed
       */
      val completedWorkerSteps =
        workerSteps.collect{case Step(step, _, timeToComplete) if timeToComplete == 0 => step}

      /*
       * All steps completed so far
       */
      val completedSteps =
        if (completedWorkerSteps.isEmpty) done else completedWorkerSteps.head :: done

      /*
       * The remaining steps (i.e. those not completed) are passed through to the next iteration. Only
       * worker steps have their time-remaining reduced.
       */
      val remainingSteps =
        steps.collect{case s @ Step(step, deps, timeToComplete) if !completedSteps.contains(step) =>
          Step(step, deps, if (workerSteps.contains(s)) timeToComplete - 1 else timeToComplete)}

      runSteps(remainingSteps, completedSteps, second + 1)
    }
  }

  val totalTimeTaken = runSteps(allStepsPart2, Nil, 0)
  assert(totalTimeTaken == 1050)
}
