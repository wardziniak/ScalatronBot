import scala.util.Random

/**
 * Created by wardziniak on 18.04.15.
 */
object CommandParser {
  def apply(command: String) = {
    def splitParam(param: String) = {
      val segments = param.split('=')
      if( segments.length != 2 )
        throw new IllegalStateException("invalid key/value pair: " + param)
      (segments(0),segments(1))
    }

    val segments = command.split('(')
    if( segments.length != 2 )
      throw new IllegalStateException("invalid command: " + command)

    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map( splitParam ).toMap
    (segments(0), keyValuePairs)
  }
}

case class Point(x: Int, y: Int) {
  override def toString = x + ":" + y
  def +(other: Point) = Point(x + other.x, y + other.y)
  def -(other: Point) = Point(x - other.x, y - other.y)
  def length : Double = math.sqrt(x*x + y*y)
  def distanceTo(other: Point) : Double = (this-other).length
  def signum = Point(x.signum, y.signum)
  def opositePoint = Point(-x, -y)
  // mierzymy od punktu (0,0)
  def isPointInside(other: Point) = x.signum == other.x.signum && y.signum == other.y.signum && x.abs >= other.x.abs && y.abs >= other.y.abs
}

object Point {
  val Zero = Point(0,0)
  def apply(s: String) : Point = { val a = s.split(':'); Point(a(0).toInt,a(1).toInt) }
  val Right     = Point( 1,  0)
  val RightUp   = Point( 1, -1)
  val Up        = Point( 0, -1)
  val UpLeft    = Point(-1, -1)
  val Left      = Point(-1,  0)
  val LeftDown  = Point(-1,  1)
  val Down      = Point( 0,  1)
  val DownRight = Point( 1,  1)
  val NEIGHBORS = List(Right, RightUp, Up, UpLeft, Left, LeftDown, Down, DownRight)
}

case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = Point(size/2, size/2)
  val random = Random

  def indexFromAbsPos(point: Point) = point.x + point.y * size
  def absPosFromIndex(index: Int) = Point(index % size, index / size)
  def absPosFromRelPos(point: Point) = point + center
  def cellAtAbsPos(point: Point) = cells.charAt(indexFromAbsPos(point))
  def indexFromRelPos(point: Point) = indexFromAbsPos(absPosFromRelPos(point))
  def relPosFromAbsPos(point: Point) = point - center
  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
  def cellAtRelPos(point: Point) = cells.charAt(indexFromRelPos(point))


  def offsetToNearest(c: Char) = {
    val relativePositions = cells.par.view.zipWithIndex.filter(_._1 == c).map(p => relPosFromIndex(p._2))
    if(relativePositions.isEmpty) None
    else Some(relativePositions.minBy(_.length))
  }

  def getEntities(c: Char) = cells.par.view.zipWithIndex.filter(_._1 == c).map(p => p._2)

  def randomDirection = Point(random.nextInt(3)-2, random.nextInt(3) - 1)

  def numberOfEntities(c: Char) = cells.filter(_ == 'c').size

}










