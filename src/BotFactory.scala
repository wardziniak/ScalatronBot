/**
 * Created by wardziniak on 22.04.15.
 */
object BotFactory {

  def createBot(inputParams: Map[String, String]) = {
    inputParams("generation").toInt match {
      case 0 => MasterBot(inputParams)
      case _ => MiniBot(inputParams)
    }
  }
}

abstract class Bot(input: Map[String, String]) {

  var commands = ""
  var stateParams = Map.empty[String,String]
  var justSpawnBot = false
  val collision = inputAsPointOrElse("collision", Point.Zero)
  val energy = input("energy").toInt
  val generation = input("generation")
  val view = View(input("view"))
  val lastMove = inputAsPointOrElse("lastMove", view.randomDirection)
  var currentTime = input("time").toInt
  //val numberOfMinBots = input.getOrElse("slaves", 0)
  var timeOfLastSpawn = inputAsIntOrElse("timeOfLastSpawn", -100)
  var numberOfSpawns = inputAsIntOrElse("numberOfSpawns", 0)
  var analyzer = ViewAnalyzer(view)
  var numberOfMinibots = 0
  val MAX_NUMBER_OF_MINIBOTS = 20
  val MAX_NUMBER_OF_MINIBOTS_NEAR = 3
  val HOW_MANY_STEPS_BEFORE_END_STOP_SPAWN = 100
//  def createMiniBot(direction: Point): String
  def makeMove(): Bot
  def inputOrElse(key: String, fallback: String) = input.getOrElse(key, fallback)
  def inputAsIntOrElse(key: String, fallback: Int) = input.get(key).map(_.toInt).getOrElse(fallback)
  def inputAsPointOrElse(key: String, fallback: Point) = input.get(key).map(s => Point(s)).getOrElse(fallback)

  def append(s: String) : Bot = { commands += (if(commands.isEmpty) s else "|" + s); this }
  def move(direction: Point) : Bot = { set(Map(("lastMove", direction.toString))); append("Move(direction=" + direction + ")") }

  def spawnMiniBotIfNeeded(suggestion: Point)
  def spawn(direction: Point, params: Map[String,String]) : Bot = append("Spawn(direction="
    + direction + (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) + ")")
  def set( params: Map[String,String]) : Bot = {stateParams ++= params; this}

  def numberOfMiniBots = view.numberOfEntities('S')

}

case class MiniBot(input: Map[String, String]) extends Bot(input) {

  val ENERGY_TO_SEARCH_MASTER = 1000
  val ENERGY_NEEDED_TO_SPAWN = 250
  val MAX_NUMBER_OF_SPAWNS = 3
  val SPAWN_FREQUENCY = 20
  val HOW_MANY_STEPS_BEFORE_END_GO_TO_MASTER = 100
  val MAX_LIFESPAN = 100
  val MIN_DISTANCE_TO_EXPLODE = 4//5
  val MIN_DISTANCE_TO_EXPLODE_MASTER = 2 //5
  val MIN_ENERGY_TO_NOT_EXPLODE = 500
  val MIN_DAMAGE_TO_EXPLODE = 150

  val masterDirection = inputAsPointOrElse("master", Point.Zero)
//  {
//    val segments = input("master").split(":")
//    Point(segments(0).toInt,segments(1).toInt)
//  }

  var born = inputAsIntOrElse("born", 0)

  def explode(size :Int) : Bot = append("Explode(size=" + size + ")")


  def spawnMiniBotIfNeeded(suggestion: Point) = {
    if (analyzer.myMiniBots.size < MAX_NUMBER_OF_MINIBOTS_NEAR //&& currentTime > timeOfLastSpawn + SPAWN_FREQUENCY
      && currentTime + HOW_MANY_STEPS_BEFORE_END_STOP_SPAWN < Apocalypse.value
      && energy > ENERGY_NEEDED_TO_SPAWN ) {
      spawn(suggestion, Map(("miniBotType", "picker"), ("energy", "100")))
      justSpawnBot = true
      timeOfLastSpawn = currentTime
      numberOfSpawns += 1
    }
  }

  def goToMaster = {
    if (currentTime + HOW_MANY_STEPS_BEFORE_END_GO_TO_MASTER > Apocalypse.value || energy > ENERGY_TO_SEARCH_MASTER
    || currentTime - born > MAX_LIFESPAN) {
      move(analyzer.getNearestGoodPointToSuggestion(masterDirection.signum))
      true
    }
   else
      false
  }

  def normalMove(offset: Point) = {
    spawnMiniBotIfNeeded(offset)
    if (goToMaster) this
    else if (justSpawnBot) move(offset.opositePoint)
    else move(offset)
    this
  }

  def moveIfNeededExplodEnemyMaster(offset: Point) = {
    val radius = analyzer.calculateExplosionRadius(this)

    if (radius > 0) explode(radius)
    else normalMove(offset)


//    val enemyMasterBot = analyzer.nearestEnemyMasterBot
//    enemyMasterBot match {
//      case None => normalMove(offset)
//      case Some(offset) =>
//        if (analyzer.myMiniBots.filter(offset.isPointInside(_)).isEmpty) {
//          if (offset.length < MIN_DISTANCE_TO_EXPLODE_MASTER) {
//            explode(7)
//          }
//          else
//            move(offset)
//        }
//        else normalMove(offset)
//    }

  }

  def makeMove()  = {
    if (numberOfSpawns == 0) {
      timeOfLastSpawn = currentTime
      numberOfSpawns = 1
      born = currentTime
    }
    analyzer.analyzeView
    val enemyMiniBot = analyzer.nearestEnemyMinBot
    val suggestion = analyzer.bestChoice(lastMove)


//    val myMaster = analyzer.masterBot
//    if (myMaster == null)
//      moveIfNeededExplodEnemyMaster(suggestion)
//    else {
//      enemyMiniBot match {
//        case None => moveIfNeededExplodEnemyMaster(suggestion)
//        case Some(offset) =>
//          if (analyzer.myMiniBots.filter(offset.isPointInside(_)).isEmpty) {
//            if (offset.length < MIN_DISTANCE_TO_EXPLODE) {
//              explode(7)
//            }
//            else
//              move(offset)
//          }
//          else moveIfNeededExplodEnemyMaster(suggestion)
//      }
//    }



    // Czy puszczamy obronnego bota
    enemyMiniBot match {
      case None => moveIfNeededExplodEnemyMaster(suggestion)
      case Some(offset) =>
          if (analyzer.myMiniBots.filter(offset.isPointInside(_)).isEmpty) {
            if (offset.length < MIN_DISTANCE_TO_EXPLODE) {
              explode(7)
            }
            else
              move(offset)
          }
        else moveIfNeededExplodEnemyMaster(suggestion)
    }
  }

  override def toString = {
    var result = commands
    set(Map(("timeOfLastSpawn", "" + timeOfLastSpawn),("numberOfSpawns", "" + numberOfSpawns), ("born", "" + born)))
    if(!stateParams.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
    }
    result
  }

  def setKeys() = "|Set(timeOfLastSpawn=" + timeOfLastSpawn + ",numberOfSpawns=" + numberOfSpawns + ", born=" + born + ")"

}


case class MasterBot(input: Map[String, String]) extends Bot(input) {

  val MIN_DISTANCE_TO_ENEMY_MIN_BOT = 10.0d
  val SPAWN_FREQUENCY = 20


  val ENERGY_NEEDED_TO_SPAWN = 100

  val MIN_ENERGY_FOR_EXTRA_ENERGY = 1500
  val EXTRA_ENERGY_FOR_SPAWN_BOT = 300

  override def toString = {
    var result = commands
    set(Map(("timeOfLastSpawn", "" + timeOfLastSpawn)))
    if(!stateParams.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
    }
    result
  }

  def spawnMiniBotIfNeeded(suggestion: Point) = {
    val enemyMiniBot = analyzer.nearestEnemyMinBot
    // Czy puszczamy obronnego bota
    enemyMiniBot match {
      case None =>
      case Some(offset) =>
        if (offset.length <= MIN_DISTANCE_TO_ENEMY_MIN_BOT) {
          if (analyzer.myMiniBots.filter(offset.isPointInside(_)).isEmpty) {
            justSpawnBot = true
            spawn(offset.signum, Map(("miniBotType", "defender"), ("energy", "100")))
            //timeOfLastSpawn = currentTime
          }
        }
    }

    if (!justSpawnBot && analyzer.myMiniBots.size < MAX_NUMBER_OF_MINIBOTS_NEAR && currentTime + HOW_MANY_STEPS_BEFORE_END_STOP_SPAWN < Apocalypse.value
      && energy > ENERGY_NEEDED_TO_SPAWN ) { //&&  currentTime-SPAWN_FREQUENCY > timeOfLastSpawn) {
      //spawn(suggestion, Map(("miniBotType", "picker"), ("energy", "100")))
      spawn(suggestion, Map(("miniBotType", "picker"), ("energy", "" + (if (energy > MIN_ENERGY_FOR_EXTRA_ENERGY ) EXTRA_ENERGY_FOR_SPAWN_BOT else 100))))
      timeOfLastSpawn = currentTime
      justSpawnBot = true
    }
  }

  def makeMove() = {
    analyzer.analyzeView

    val suggestion = analyzer.bestChoice(lastMove)
    spawnMiniBotIfNeeded(suggestion)
    if (justSpawnBot) move(suggestion.opositePoint)
    else move(suggestion)
    this
  }
}
