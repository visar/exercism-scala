object SpiralMatrix {
  def spiralMatrix(size: Int): List[List[Int]] = {
    val world: World = World.create(size)

    world.matrix.map(rows => rows.toList).toList
  }

  case class World private (
    matrix: Array[Array[Int]],
    position: Position = Position(0, 0),
    direction: Direction = Direction.Right
  ) { world =>
    def next: (Position, Direction) = {
      import position.{col, row}
      if (row + world.direction.dx >= 0
          && row + world.direction.dx < world.matrix.length
          && col + world.direction.dy >= 0
          && col + world.direction.dy < world.matrix(0).length
          && world.matrix(row + world.direction.dx)(col + world.direction.dy) == 0)
        (Position(row + world.direction.dx, col + world.direction.dy), world.direction)
      else
        (Position(row + world.direction.next.dx, col + world.direction.next.dy), world.direction.next)
    }
  }

  object World {
    def create(size: Int): World =
      (1 to size * size).foldLeft(World(matrix = Array.ofDim[Int](size, size))) { (world, number) =>
        world.matrix(world.position.row)(world.position.col) = number

        world.next match {
          case (position, direction) =>
            world.copy(position = position, direction = direction)
        }
      }
  }

  sealed trait Direction {
    def dx: Int
    def dy: Int

    def next: Direction = this match {
      case Direction.Right => Direction.Down
      case Direction.Down  => Direction.Left
      case Direction.Left  => Direction.Up
      case Direction.Up    => Direction.Right
    }
  }

  object Direction {
    case object Right extends Direction {
      val dx: Int = 0
      val dy: Int = 1
    }
    case object Down extends Direction {
      val dx: Int = 1
      val dy: Int = 0
    }
    case object Left extends Direction {
      val dx: Int = 0
      val dy: Int = -1
    }
    case object Up extends Direction {
      val dx: Int = -1
      val dy: Int = 0
    }
  }

  case class Position(row: Int, col: Int)
}
