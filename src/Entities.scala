/**
 * Created by wardziniak on 24.04.15.
 */
object Entities {

  val INVISIBLE = '?'
  val EMPTY = '_'
  val WALL = 'W'
  val MY_BOT = 'M'
  val ENEMY_BOT = 'm'
  val MY_MINI_BOT = 'S'
  val ENEMY_MINI_BOT = 's'
  val GOOD_PLANT = 'P'
  val BAD_PLANT = 'p'
  val GOOD_BEAST = 'B'
  val BAD_BEAST = 'b'

  def isGood(c: Char) = c == GOOD_PLANT || c == GOOD_BEAST || c == MY_BOT || c == EMPTY

  def isNotBad(c: Char) = c == GOOD_PLANT || c == GOOD_BEAST || c == MY_BOT || c == EMPTY

  def isBad(c: Char) = c != GOOD_PLANT && c != GOOD_BEAST

}
