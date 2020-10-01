sealed trait Bearing {
  import Bearing._

  def dx: Int
  def dy: Int

  def right: Bearing = this match {
    case North => East
    case East  => South
    case South => West
    case West  => North
  }

  def left: Bearing = this match {
    case North => West
    case West  => South
    case South => East
    case East  => North
  }
}

object Bearing {
  case object North extends Bearing {
    val dx: Int = 0
    val dy: Int = 1
  }

  case object East extends Bearing {
    val dx: Int = 1
    val dy: Int = 0
  }

  case object West extends Bearing {
    val dx: Int = -1
    val dy: Int = 0
  }

  case object South extends Bearing {
    val dx: Int = 0
    val dy: Int = -1
  }
}

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {
  def turnRight: Robot = copy(bearing = bearing.right)
  def turnLeft: Robot  = copy(bearing = bearing.left)
  def advance: Robot   = copy(coordinates = (coordinates._1 + bearing.dx, coordinates._2 + bearing.dy))
  def simulate(path: String): Robot =
    path.foldLeft(this) {
      case (r, c) =>
        c match {
          case 'L' => r.turnLeft
          case 'R' => r.turnRight
          case 'A' => r.advance
          case _   => r
        }
    }
}
