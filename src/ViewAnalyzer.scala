/**
 * Created by wardziniak on 24.04.15.
 */
case class ViewAnalyzer(view: View) {

  val DAMAGE_FACTOR = 200

  var enemyMinBots = List[Point]()
  var enemyMasterBots = List[Point]()
  var fluppets = List[Point]()
  var snorgs = List[Point]()
  var zugars = List[Point]()
  var toxifers = List[Point]()
  var myMiniBots = List[Point]()
  var masterBot: Point = null

  def analyzeView = {
    val cells = view.cells
    val cellCount = cells.length

    for (i <- 0 until cellCount) {
      if (i != (view.size/2 * view.size + view.size/2))
      view.cells(i) match {
        case 'S' => myMiniBots ::= view.relPosFromIndex(i)
        case 's' => enemyMinBots ::= view.relPosFromIndex(i)
        case 'm' => enemyMasterBots ::= view.relPosFromIndex(i)
        case 'B' => fluppets ::= view.relPosFromIndex(i)
        case 'P' => zugars ::= view.relPosFromIndex(i)
        case 'b' => snorgs ::= view.relPosFromIndex(i)
        case 'p' => toxifers ::= view.relPosFromIndex(i)
        case 'M' => masterBot = view.relPosFromIndex(i)
        case _ =>
      }
    }
  }

  def bestChoice(lastMove: Point) = {
    (nearestFluppet, nearestZugar) match {
      case (Some(offsetF), Some(offsetZ)) => getNearestGoodPointToSuggestion(List(offsetF, offsetZ).minBy(_.length).signum)
      case (None, Some(offset)) => getNearestGoodPointToSuggestion(offset.signum)
      case (Some(offset), None) => getNearestGoodPointToSuggestion(offset.signum)
      case _ => getNearestGoodPointToSuggestion(lastMove) // Jesli jest ten ruch to przy spawnnie powinismy utrzymac kierunek, zeby sie nie zapetlac czasami
    }
  }

  def getNearestGoodPointToSuggestion(offset: Point) = {
    val goodPoints = Point.NEIGHBORS.filter(point => Entities.isGood(view.cellAtRelPos(point)))
    if (goodPoints.isEmpty) offset
    else goodPoints.minBy(offset.distanceTo(_))
  }

  def nearestEntity(entities: List[Point]) = if(entities.isEmpty) None else Some(entities.minBy(_.length))

  def nearestEnemyMinBot = nearestEntity(enemyMinBots)

  def nearestFluppet = nearestEntity(fluppets)

  def nearestSnorg = nearestEntity(snorgs)

  def nearestZugar = nearestEntity(zugars)

  def nearestToxifer = nearestEntity(toxifers)

  def nearestEnemyMasterBot = nearestEntity(enemyMasterBots)

  def calculateExplosionRadius(bot: MiniBot) = {

    var dupa = 1
    for (i <- 1 until 10) {
      dupa += i
    }

    if (bot.energy > bot.MIN_ENERGY_TO_NOT_EXPLODE)
      0
    else {
      var maxDemage = 0.0
      var exploadRadius = 0
      for (i <- 1 until 10) {
      var i = 10
        var lista = (enemyMinBots ::: enemyMasterBots)
        var filterList = lista.filter(p => p.length <= i)
        var listMapped = filterList.map(p => damageAtCenter(bot.energy, i) * (1 - (p.length / i)))
        var tmpDamage = listMapped.foldLeft(0.0)(_ + _)
        if (tmpDamage > maxDemage) {
          exploadRadius = i
          maxDemage = tmpDamage
        }
      }
      if (2*maxDemage > bot.energy) exploadRadius
      else 0
    }
  }

  def damageAtCenter(energy: Int, radius: Int) = DAMAGE_FACTOR * energy / (Math.Pi * Math.pow(radius, 2))
}
