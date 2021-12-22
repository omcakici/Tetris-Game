package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {

  class GameState (var board : Array[Array[CellType]],
                   var nextTetromino : List[Point]) {

    var isOutOfBoundsLeft : Boolean = false
    var isOutOfBoundsRight : Boolean = false
    var collisionDetected : Boolean = false
    var lineCleared : Boolean = false
    var listLinesCleared = List[Int]()
    var isGameOver = false

    def updateBoard() : Array[Array[CellType]] = {
      val updateCellType = getCellType(currentTetromino(0))

      for (i <- 0 until currentTetromino.size) board(currentTetromino(i).y)(currentTetromino(i).x) = updateCellType
      lineCleared = isLineCleared()

      if (lineCleared) {
        for (listIndex <- 0 until listLinesCleared.size) {
          for (row <- listLinesCleared(listIndex) until listLinesCleared.size - 1 by -1) {
            for (col <- 0 until gridDims.width) {
              board(row)(col) = board(row - 1)(col)
            }
          }
        }

        for (row <- 0 until listLinesCleared.size) {
          for (col <- 0 until gridDims.width) {
            board(row)(col) = Empty
          }
        }
      }

      board
    }

    def updateTetromino() : List[Point] = {
      getTetrominoNr = randomGen.randomInt(nrTetrominoes)
      currentTetromino = getNewTetromino()
      currentTetromino
    }
  }

  val nrTetrominoes : Int = 7
  var getTetrominoNr : Int = randomGen.randomInt(nrTetrominoes)
  val anchorX : Int = {
    if (gridDims.width % 2 == 0) (gridDims.width - 1)/2
    else gridDims.width/2
  }
  val anchorY : Int = 1
  val anchorPoint : Point = Point(anchorX, anchorX)

  val tetrominoI = List[Point](Point(anchorX - 1, anchorY), Point(anchorX, anchorY), Point(anchorX + 1, anchorY),  Point(anchorX + 2, anchorY))
  val tetrominoJ = List[Point](Point(anchorX - 1, anchorY - 1), Point(anchorX - 1, anchorY), Point(anchorX, anchorY), Point(anchorX + 1, anchorY))
  val tetrominoL = List[Point](Point(anchorX - 1, anchorY), Point(anchorX, anchorY), Point(anchorX + 1, anchorY), Point(anchorX + 1, anchorY - 1))
  val tetrominoO = List[Point](Point(anchorX, anchorY - 1), Point(anchorX + 1, anchorY - 1), Point(anchorX, 1), Point(anchorX + 1, anchorY))
  val tetrominoS = List[Point](Point(anchorX - 1, anchorY),Point(anchorX, anchorY), Point(anchorX, anchorY - 1), Point(anchorX + 1, anchorY - 1))
  val tetrominoT = List[Point](Point(anchorX - 1, anchorY), Point(anchorX, anchorY), Point(anchorX, anchorY - 1), Point(anchorX + 1, anchorY))
  val tetrominoZ = List[Point](Point(anchorX - 1, anchorY - 1), Point(anchorX, anchorY), Point(anchorX, anchorY - 1), Point(anchorX + 1, anchorY))

  def getNewTetromino(): List[Point] = {
    val getTetrominoNr = randomGen.randomInt(nrTetrominoes)
    var currentTetromino = List[Point]()

    getTetrominoNr match {
      case 0 => currentTetromino = tetrominoI
      case 1 => currentTetromino = tetrominoJ
      case 2 => currentTetromino = tetrominoL
      case 3 => currentTetromino = tetrominoO
      case 4 => currentTetromino = tetrominoS
      case 5 => currentTetromino = tetrominoT
      case 6 => currentTetromino = tetrominoZ
    }
    currentTetromino
  }

  var board = Array.ofDim[CellType](gridDims.height, gridDims.width)
  for (row <- 0 until gridDims.height) {
    for (col <- 0 until gridDims.width) {
      board(row)(col) = initialBoard(row)(col)
    }
  }

  var currentTetromino : List[Point] = getNewTetromino()
  var gameState = new GameState(board, currentTetromino)

  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  def getCellType(p : Point): CellType = {
    if ((getTetrominoNr == 0 && currentTetromino.contains(p)) || board(p.y)(p.x) == ICell) ICell
    else if ((getTetrominoNr == 1 && currentTetromino.contains(p)) || board(p.y)(p.x) == JCell) JCell
    else if ((getTetrominoNr == 2 && currentTetromino.contains(p)) || board(p.y)(p.x) == LCell) LCell
    else if ((getTetrominoNr == 3 && currentTetromino.contains(p)) || board(p.y)(p.x) == OCell) OCell
    else if ((getTetrominoNr == 4 && currentTetromino.contains(p)) || board(p.y)(p.x) == SCell) SCell
    else if ((getTetrominoNr == 5 && currentTetromino.contains(p)) || board(p.y)(p.x) == TCell) TCell
    else if ((getTetrominoNr == 6 && currentTetromino.contains(p)) || board(p.y)(p.x) == ZCell) ZCell
    else Empty
  }

  def calculateRelativePoint(p: Point) : Point = {
    var absolutePoint = Point(anchorPoint.x - p.x, anchorPoint.y - p.y)
    absolutePoint
  }

  def calculateAbsolutePoint(p: Point) : Point = {
    var absolutePoint = Point(anchorPoint.x + p.x, anchorPoint.y + p.y)
    absolutePoint
  }

  def rotateLeft(): Unit = {
    if (!gameState.isGameOver) {
      var canRotate : Boolean = true
      var tempTetromino = currentTetromino

      for (i <- 0 until currentTetromino.size) {
        if (getTetrominoNr == 0) {
          tempTetromino = tempTetromino.updated(i, Point(tempTetromino(i).y, -tempTetromino(i).x + 1))
        }
        else if (getTetrominoNr == 3) return
        else {
          var relativePoint = calculateRelativePoint(tempTetromino(i))
          tempTetromino = tempTetromino.updated(i, Point(-relativePoint.y, relativePoint.x))

          var absolutePoint = calculateAbsolutePoint(tempTetromino(i))
          tempTetromino = tempTetromino.updated(i, absolutePoint)
        }
      }

      for (i <- 0 until tempTetromino.size) {
        if (board(tempTetromino(i).y)(tempTetromino(i).x) != Empty) canRotate = false
      }

      if (canRotate) currentTetromino = tempTetromino
      gameState = new GameState(board, currentTetromino)
    }
  }

  def rotateRight(): Unit = {
    if (!gameState.isGameOver) {
      var canRotate : Boolean = true
      var tempTetromino = currentTetromino

      for (i <- 0 until tempTetromino.size) {
        if (getTetrominoNr == 0) {
          tempTetromino = tempTetromino.updated(i, Point(-tempTetromino(i).y + 3, tempTetromino(i).x))
        }
        else if (getTetrominoNr == 3) return
        else {
          var relativePoint = calculateRelativePoint(tempTetromino(i))
          tempTetromino = tempTetromino.updated(i, Point(relativePoint.y, -relativePoint.x))

          var absolutePoint = calculateAbsolutePoint(tempTetromino(i))
          tempTetromino = tempTetromino.updated(i, absolutePoint)
        }
      }

      for (i <- 0 until tempTetromino.size) {
        if (board(tempTetromino(i).y)(tempTetromino(i).x) != Empty) canRotate = false
      }

      if (canRotate) currentTetromino = tempTetromino
      gameState = new GameState(board, currentTetromino)
    }
  }

  def moveLeft(): Unit = {
    gameState.isOutOfBoundsRight = false
    var canMove : Boolean = true
    var tempTetromino = currentTetromino

    if (!gameState.isGameOver && !gameState.isOutOfBoundsLeft) {
      for (i <- 0 until currentTetromino.size) {
        if (tempTetromino.head.x == 0) gameState.isOutOfBoundsLeft = true

        tempTetromino = tempTetromino.updated(i, Point(tempTetromino(i).x - 1, tempTetromino(i).y))
        if (board(tempTetromino(i).y)(tempTetromino(i).x) != Empty) canMove = false
      }

      if (canMove) currentTetromino = tempTetromino
    }
  }

  def moveRight(): Unit = {
    gameState.isOutOfBoundsLeft = false
    var canMove : Boolean = true
    var tempTetromino = currentTetromino

    if (!gameState.isGameOver && !gameState.isOutOfBoundsRight) {
      for (i <- 0 until tempTetromino.size) {
        if (tempTetromino.last.x == gridDims.width - 2) gameState.isOutOfBoundsRight = true

        tempTetromino = tempTetromino.updated(i, Point(tempTetromino(i).x + 1, tempTetromino(i).y))
        if (board(tempTetromino(i).y)(tempTetromino(i).x) != Empty) canMove = false
      }

      if (canMove) currentTetromino = tempTetromino
    }
  }

  def moveDown(): Unit = {
    var canMove : Boolean = true
    var tempTetromino = currentTetromino

    if (!gameState.isGameOver && !gameState.collisionDetected) {
      for (i <- 0 until currentTetromino.size) {
        tempTetromino = tempTetromino.updated(i, Point(tempTetromino(i).x, tempTetromino(i).y + 1))
        gameState.collisionDetected = detectCollision(tempTetromino(i))

        if (board(tempTetromino(i).y)(tempTetromino(i).x) != Empty) {
          canMove = false
          gameState.isGameOver = true
        }
      }

      if (canMove) currentTetromino = tempTetromino
    }

    else createNewGameState()
  }

  def doHardDrop(): Unit = {
    while (!gameState.collisionDetected) moveDown()
    createNewGameState()
  }

  def detectCollision(p : Point): Boolean = {
    if (p.y == gridDims.height - 1 ||
      (p.y < gridDims.height - 1 && board(p.y + 1)(p.x) != Empty)) gameState.collisionDetected = true

    gameState.collisionDetected
  }

  def createNewGameState() : Unit = {
    board = gameState.updateBoard()
    var tempTetromino = gameState.updateTetromino()
    var numEmptyCells = 0

    for (i <- 0 until tempTetromino.size) {
      if (board(tempTetromino(i).y)(tempTetromino(i).x) == Empty) numEmptyCells += 1
    }

    if (numEmptyCells == tempTetromino.size) {
      currentTetromino = tempTetromino
      gameState = new GameState(board, currentTetromino)
    }
    else gameState.isGameOver = true
  }

  def isLineCleared(): Boolean = {
    var lineCleared = false

    for (row <- 0 until gridDims.height) {
      if (board(row).forall(x => {x != Empty}))
      {
        lineCleared = true
        gameState.listLinesCleared = gameState.listLinesCleared :+ row

        for (col <- 0 until gridDims.width) board(row)(col) = Empty
      }
    }
    lineCleared
  }

  def isGameOver: Boolean = gameState.isGameOver

}

object TetrisLogic {

  val FramesPerSecond: Int = 5
  val DrawSizeFactor = 1.0

  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}
