package example

import scala.collection.immutable.Vector
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.util.control.Breaks.{break, breakable}

// Case class representing a player
case class Player(id: Int, human: Boolean)

// Case class representing the TicTacToe game state
case class TicTacToe(
                      board: Vector[Vector[Int]] = Vector.fill(3)(Vector.fill(3)(0)), // Initialize 3x3 board filled with 0s
                      currentTurn: Int = 1, // Current turn: 1 for player X, 2 for player Y
                      won: Int = 0, // Game won status: 0 = not won, 1 = player X, 2 = player Y
                      player1: Boolean, // Boolean indicating if player 1 is human
                      player2: Boolean // Boolean indicating if player 2 is human
                    ){

  // Define the winning lines for TicTacToe
  val winningLines: Vector[Vector[(Int, Int)]] = Vector(
    // Horizontal lines
    Vector((0, 0), (0, 1), (0, 2)),
    Vector((1, 0), (1, 1), (1, 2)),
    Vector((2, 0), (2, 1), (2, 2)),
    // Vertical lines
    Vector((0, 0), (1, 0), (2, 0)),
    Vector((0, 1), (1, 1), (2, 1)),
    Vector((0, 2), (1, 2), (2, 2)),
    // Diagonal lines
    Vector((0, 0), (1, 1), (2, 2)),
    Vector((0, 2), (1, 1), (2, 0))
  )

  // Method to check if a player has won the game
  private def checkIfWon(localBoard: Vector[Vector[Int]]): Int = {
    var won = 0 // Initialize won status to 0

    breakable {
      winningLines.foreach { line =>
        if (
          (localBoard(line(0)(0))(line(0)(1)) != 0) &&
            (localBoard(line(0)(0))(line(0)(1)) == localBoard(line(1)(0))(line(1)(1))) &&
            (localBoard(line(0)(0))(line(0)(1)) == localBoard(line(2)(0))(line(2)(1)))
        ) {
          won = localBoard(line(0)(0))(line(0)(1))
          println(s"GAME OVER!!, ${stringifyPlayer(won)} has won!!")
          break // Exit the loop if a winning line is found
        }
      }
    }

    won // Return the won status
  }

  // Check if the current player is human
  def isCurrentPlayerHuman: Boolean = {
    if(currentTurn == 1) player1 else player2
  }

  // Method to determine the computer's move
  def computerMove: List[Int] = {
    var returnMove: List[Int] = List() // Initialize return move
    var filledLines: List[(List[List[Int]], List[List[Int]], List[List[Int]])] = List()

    winningLines.foreach { line =>
      var samePlayerCells: List[List[Int]] = List()
      var otherPlayerCells: List[List[Int]] = List()
      var emptyCells: List[List[Int]] = List()

      for (cell <- line) {
        if (board(cell._1)(cell._2) == currentTurn) {
          samePlayerCells = samePlayerCells :+ List(cell._1, cell._2)
        } else if (board(cell._1)(cell._2) == 0) {
          emptyCells = emptyCells :+ List(cell._1, cell._2)
        } else {
          otherPlayerCells = otherPlayerCells :+ List(cell._1, cell._2)
        }
      }
      filledLines = filledLines :+ (samePlayerCells, otherPlayerCells, emptyCells)
    }

    //TODO Improve AI logic - https://stackoverflow.com/questions/125557/what-algorithm-for-a-tic-tac-toe-game-can-i-use-to-determine-the-best-move-for

    // Define the middle cell
    val middleCell = List(1, 1)

    // Check if a winning move can be made
    filledLines.find(line => line._1.length == 2 && line._3.nonEmpty)
      .orElse(
        // Check if can block a winning move
        filledLines.find(line => line._2.length == 2 && line._3.nonEmpty)
      )
      .orElse(
        // Check if can make a strategic move
        filledLines.find(line => line._1.length == 1 && line._3.nonEmpty)
      )
      .orElse(
        // Check if the middle cell is empty
        if (board(middleCell(0))(middleCell(1)) == 0) Some((List(), List(), List(middleCell))) else None
      )
      .orElse(
        // Find an empty cell
        filledLines.find(line => line._3.nonEmpty)
      )
      // Get the first empty cell from the line found by previous logic
      .map(line => line._3.head)
      .getOrElse(List())
  }

  // Method to make a move on the board
  def makeMove(x: Int, y: Int): TicTacToe = {
    if (board(x)(y) == 0) {
      val updatedBoard = board.updated(x, board(x).updated(y, currentTurn)) // Update board

      val won = checkIfWon(updatedBoard) // Check if the move results in a win

      copy(board = updatedBoard, currentTurn = if (currentTurn == 1) 2 else 1, won = won) // Return updated game state
    } else {
      println("Invalid move! Try again.")
      this // Return the same instance if the move is invalid
    }
  }

  // Convert player number to string representation
  def stringifyPlayer(player: Int = currentTurn): String = {
    if (player == 1) "X" else "Y"
  }
}

// Setup player information
def setUpPlayers(): Seq[Boolean] = {
  var player1: Option[Boolean] = None
  var player2: Option[Boolean] = None

  // Define if Player 1 is human
  while (player1.isEmpty) {
    println("Is player X human? Y or N")
    val answer = readLine().toUpperCase
    answer match {
      case "Y" => player1 = Some(true)
      case "N" => player1 = Some(false)
      case _ => println("Please answer with 'Y' or 'N'.")
    }
  }

  // Define if Player 2 is human
  while (player2.isEmpty) {
    println("Is player Y human? Y or N")
    val answer = readLine().toUpperCase
    answer match {
      case "Y" => player2 = Some(true)
      case "N" => player2 = Some(false)
      case _ => println("Please answer with 'Y' or 'N'.")
    }
  }

  Seq(player1.get, player2.get)
}

// Main entry point for the game
@main def run() = {
  var won = 0 // Initialize the winning status

  val Seq(x, y) = setUpPlayers() // Setup players and determine if they are human

  println(s"Player X is human: $x")
  println(s"Player Y is human: $y")

  
  var game = TicTacToe(Vector.fill(3)(Vector.fill(3)(0)), 1, 0, x, y) // Initialize the game state

  // Main game loop
  while (won == 0) {
    if (game.isCurrentPlayerHuman) {
      // Get move from human player
      println(s"Enter X Coordinates for player ${game.stringifyPlayer()}")
      val x = readLine().toInt
      println(s"Enter Y Coordinates for player ${game.stringifyPlayer()}")
      val y = readLine().toInt
      game = game.makeMove(x, y) // Make the move
    } else {
      // Get move from computer player
      val List(x, y) = game.computerMove
      game = game.makeMove(x, y) // Make the move
    }

    // Print the current state of the board
    game.board.foreach { row =>
      println(row.map(cell => if (cell == 1 || cell == 2) game.stringifyPlayer(cell) else ".").mkString(" "))
    }

    if (game.won != 0) {
      won = 1 // Update the winning status if someone has won
    }
  }
}