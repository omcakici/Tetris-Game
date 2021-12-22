package tetris.logic


abstract class Tetromino {
  var body : List[Point]
  val tetType : CellType
  val indexOfCenter : Int

  def rotateLeft() : Unit
  def rotateRight() : Unit
}

class TetrominoI(list: List[Point], tetrominoType: CellType) extends Tetromino {
  override var body : List[Point] = list
  override val tetType: CellType = tetrominoType
  val indexOfCenter : Int = 1


  override def rotateLeft(): Unit = body = body.map(point => Point(-point.y + 1, point.x)) // (x,y) -> (-y + 1,x)
  override def rotateRight(): Unit = body = body.map(point => Point(point.y, -point.x + 1)) // (x,y) -> (y ,-x + 1)
}

class TetrominoO(list: List[Point], tetrominoType: CellType) extends Tetromino {
  override var body : List[Point] = list
  override val tetType: CellType = tetrominoType
  val indexOfCenter : Int = 1

  override def rotateLeft(): Unit = body = body // (x,y) -> (x,y)
  override def rotateRight(): Unit = body = body // (x,y) -> (x,y)
}

class TetrominoJLSTZ(list: List[Point], tetrominoType: CellType) extends Tetromino {
  override var body : List[Point] = list
  override val tetType: CellType = tetrominoType
  val indexOfCenter : Int = 2 //rotation for pieces where the anchor center of rotation

  override def rotateLeft(): Unit = body = body.map(point => Point(-point.y, point.x)) // (x,y) -> (-y,x)
  override def rotateRight(): Unit = body = body.map(point => Point(point.y, -point.x)) // (x,y) -> (y,-x)
}