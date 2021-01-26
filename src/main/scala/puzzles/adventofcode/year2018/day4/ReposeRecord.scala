package puzzles.adventofcode.year2018.day4

import java.text.SimpleDateFormat
import java.util.Date

object ReposeRecord extends App {
  /*
   * Read in the raw observation data
   */
  val observationInputStrings =
    scala.io.Source.fromFile("src/main/resources/puzzles/adventofcode/year2018/day4/input.txt").getLines().toList

  /*
   * The timestamp, state and guard id (where available) need to be extracted (using regex) and the observations sorted into
   * chronological order.
   */

  import Observation._

  val chronologicalObservationData = observationInputStrings.map(obs => {
    val ObservationExtractionPattern(timestamp, text1, guardId, text2) = obs
    (DateFormat.parse(timestamp), guardId, if (guardId.isEmpty) (text1 + text2).trim else text2.trim)
  }).sortBy(_._1)

  /*
   * Now that the observations are in order, the missing guard ids can be completed by working through the
   * observations and substituting the guard id from the last observation, where necessary. The guard id for the first
   * observation will be provided (as this represents the the first guard going on duty), therefore this is the initial
   * reference to start from
   */
  val initialObservation =
    Observation(
      chronologicalObservationData.head._2.toInt, // Guard id  (known)
      chronologicalObservationData.head._1, // Time stamp (in text form)
      chronologicalObservationData.head._3) // State (begins shift, wakes up or falls asleep)

  /*
   * Fold the rest of the observations into dedicated Observation objects
   */
  val observations = chronologicalObservationData.drop(1)
    .foldLeft(List[Observation](initialObservation)) { case (obs, (curDateTime, currGuardId, currGuardState)) =>
      val guardId = if (currGuardId.isEmpty) obs.head.guardId else currGuardId.toInt
      Observation(guardId, curDateTime, currGuardState) :: obs
    }.reverse

  /*
   * Phew - the data is now ready for the puzzle
   */
  /*
   ********************************************************************************
   *                                                                              *
   * PART 1                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * Build the graph shown in the puzzle from which the answers will be obtained. This is a map of (dd-MM, guard id)
   * pairs to a hashDotString of length 60 (each character representing whether the guard is asleep # or awake . at
   * that position in time
   */

  /*
   * Grab the sleep-wake observations for each guard (the alternate sleep, wake observations define the intervals when the guard is asleep
   */
  val awakeSleepObservations: Map[(String, Int), List[Observation]] =
    observations.filter(_.event != "begins shift").groupBy(obs=>(obs.dayMonth, obs.guardId))
  /*
   * Build a hash-dot table for each guard as per the puzzle input example
   */
  val hashDotTable: Map[(String, Int), String] =
    awakeSleepObservations.map{case(key, obs)=>(key, asHashDotArray(obs))}

  /*
   * The answers to the puzzle can be gleaned from the hash-dot graph. It's a bit of a pain to extract the information, which makes
   * me think there might be a better way of doing this.
   */

  val dailySleepTimeByGuardId: List[(Int, Int)] =
    hashDotTable.toList.map{case((_, id), asleepArray)=> (id, asleepArray.filter(_ == '#').length)}
  val totalDailySleepTimeByGuardId: Map[Int, Int] =
    dailySleepTimeByGuardId.groupBy(_._1).map{case(id, stats)=>(id, stats.map(_._2).sum)}

  /*
   * Maximum sleep time and then identify the guard responsible
   */
  val maxSleep = totalDailySleepTimeByGuardId.values.max
  val guardId = totalDailySleepTimeByGuardId.find(_._2 == maxSleep).get._1

  /*
   * Get sleep times for the guard on a minute-by-minute basis
   */
  val sleepTimePerMinute = getSleepOccurrencesPerMinuteForAGuard(guardId)

  /*
   * Find the minute with the maximum sleep time and hence the answer to part 1
   */
  val maxSleepTimePerMinute = sleepTimePerMinute.max
  val minuteWithMaxSleepTime = sleepTimePerMinute.indexOf(maxSleepTimePerMinute)

  println(s"The answer to part 1 (the most sleepy guard id * which minute they are asleep in) is ${minuteWithMaxSleepTime * guardId}")

  /*
   ********************************************************************************
   *                                                                              *
   * PART 2                                                                       *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * Get all the guard ids and the total number of occurrences of them sleeping at each minute
   */
  val allGuards = hashDotTable.map(_._1._2).toSet
  val allSleepOccurrencesForGuardsPerMin = allGuards.map{ id=>(id, getSleepOccurrencesPerMinuteForAGuard(id).toList)}
  /*
   * Find out which guard had the most occurrences and at which minute
   */
  val maxAndIndex: Set[(Int, Int, Int)] = allSleepOccurrencesForGuardsPerMin.map{case (id, times)=>
    val maxOccur = times.max
    (id, maxOccur, times.indexOf(maxOccur))
  }
  /*
   * A quick way (but probably not the most efficient) of arriving at the answer is to sort, order, reverse and take the head of the resulting list
   */
  val sortedByMaxFreq = maxAndIndex.toList.sortBy(_._2).reverse
  val componentPartsForTheAnswer = sortedByMaxFreq.head
  println(s"The answer to part 2 (the guard id that sleeps the most * the minute on which it happens) is ${componentPartsForTheAnswer._1 * componentPartsForTheAnswer._3}")
  /*
   ********************************************************************************
   *                                                                              *
   * PRIVATE HELPER FUNCTIONS                                                     *
   *                                                                              *
   ********************************************************************************
   */

  /*
   * This function returns a 60 character hashDot string if given a guard's list of alternate sleep, wake observations
   */
  def asHashDotArray(sleepWakePairs: List[Observation]): String = {
    /*
     * Pair up the matching sleep and wake observations, which provides the guard's sleep intervals
     */
    val sleepIntervals: List[(Int, Int)] = sleepWakePairs.sliding(2, 2).map{case List(sleep, wake)=> (sleep.mins, wake.mins)}.toList
    /*
     * Create a hashDot string and populate the value at each minute with the guard's sleep status by adding a # whenever they
     * are asleep
     */
    val awakeAtEveryMinute: Array[Char] = Array.fill[Char](60)('.')
    sleepIntervals.foreach{case (sleepingFrom, sleepingTo) => (sleepingFrom until sleepingTo).foreach(awakeAtEveryMinute.update(_, '#'))}
    awakeAtEveryMinute.mkString
  }

  /*
   * Given a hash dot table return the total sleeping time for a particular guard at each minute
   */
  def getSleepOccurrencesPerMinuteForAGuard(guardId: Int): Array[Int] = {
    /*
     * Trim down the hash dot graph to just one guard
     */
    val hashDotGraphForGuard = hashDotTable.filter(_._1._2 == guardId)
    /*
     * Sum up the sleep times for each minute to get the sleep time per minute for the guard
     */
    val sleepTimePerMinute =
      for {
        minute <- 0 until 60
        sumAtPos = hashDotGraphForGuard.values.map(hashDotStr=>if (hashDotStr.charAt(minute) == '#' ) 1 else 0).sum
      } yield sumAtPos

    sleepTimePerMinute.toArray
  }
}



/*
 * Classes that model the observations together with input data extraction and formatting features
 */
case class Observation(guardId: Int, dateTime: Date, event: String) {
  import Observation._
  def mins = MinutesDateFormat.format(dateTime).toInt
  def dayMonth = MonthDayDateFormat.format(dateTime)
}

object Observation {
  val MinutesDateFormat = new SimpleDateFormat("mm")
  val MonthDayDateFormat = new SimpleDateFormat("MM-dd")
  val DateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm")
  val ObservationExtractionPattern = """\[(.+)\]([a-zA-Z ]*)[#]*(\d*)([ a-zA-Z]*)""".r

}

