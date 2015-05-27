/**
 * Created by wardziniak on 18.04.15.
 */
class ControlFunctionFactory {

  var apocalypse = 100

  def create = new ControlFunction().respond1 _
}

object Apocalypse {
  var value: Int = 1
}

class ControlFunction {

  var lastTime = -50


  def respond(input: String): String = {
    val (opcode, paramMap) = CommandParser(input)

    if (opcode == "React") {
      val viewString = paramMap("view")
      val view = View(viewString)
      val zuggar = view.offsetToNearest('P')
      val fluppet = view.offsetToNearest('B')
      var logComman = "Log(text="
      (fluppet, zuggar) match {
        case (Some(offsetF), Some(offsetZ)) =>
          val unitOffset = List(offsetF, offsetZ).minBy(_.length).signum
          if (paramMap("energy").toInt > 300 && (paramMap("time").toInt > 40 + lastTime)) {
            lastTime = paramMap("time").toInt
            "Move(direction=" + unitOffset + ")|Spawn(direction=" + unitOffset + ",energy=100)|" + logComman + unitOffset + ")"
          }
          else "Move(direction=" + unitOffset + ")|" + logComman + unitOffset + ")"
        /*          "Move(direction=" + (offsetF.length.min(offsetZ.length)) + ")"
          val unitOffset = offset.signum
          "Move(direction=" + unitOffset + ")"*/
        case (None, Some(offset)) =>
          val unitOffset = offset.signum
          "Move(direction=" + unitOffset + ")|" + logComman + unitOffset + ")"
        case (Some(offset), None) =>
          val unitOffset = offset.signum
          "Move(direction=" + unitOffset + ")|" + logComman + unitOffset + ")"
        case _ =>
          val direction = view.randomDirection
          "Move(direction=" + direction + ")|" + logComman + direction + ")"
      }
    } else ""
  }

  def respond1(input: String): String = {
    val (opcode, paramMap) = CommandParser(input)
    opcode match {
      case "React" =>
        val bot = BotFactory.createBot(paramMap)
        bot.makeMove().toString
      case "Welcome" =>
        Apocalypse.value = paramMap("apocalypse").toInt
        ""
      case _ => ""
    }
  }
}







