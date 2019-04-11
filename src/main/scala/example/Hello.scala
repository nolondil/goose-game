package example

import scala.util.Random
import scala.io.StdIn.readLine

object Game extends Greeting with App {
  println(greeting)
  GameHandler.run()
}

object GameHandler {
  val add = """^add player (\w+)$""".r
  val move = """^move (\w+) ([1-6]), ([1-6])$""".r
  val moveRandom = """^move (\w+)$""".r
  val stop = """^stop$""".r

  val generator = Random

  def step(input: String, board: Board): (Boolean, Board) = input match {
    case add(player) if board.players(player) =>
      println(s"$player: already existing player")
      (false, board)
    case add(player) =>
      val newPlayers = board.players + player
      val newPositions = board.positions + (player -> 1)
      println(s"Players: ${newPlayers.mkString(", ")}")
      (false, board.copy(players = newPlayers, positions = newPositions))
    case move(player, die1, die2) if board.players(player) =>
      move(board, player, die1.toInt, die2.toInt)
    case move(player, _, _) =>
      println(s"$player: Not a player in the game")
      (false, board)
    case moveRandom(player) if board.players(player) =>
      val die1 = generator.nextInt(6) + 1
      val die2 = generator.nextInt(6) + 1
      move(board, player, die1, die2)
    case moveRandom(player) =>
      println(s"$player: Not a player in the game")
      (false, board)
    case stop() =>
      (true, board)
    case _ =>
      println("Unown input")
      (false, board)
  }

  val lastTile = 63
  val bridge = 6
  val endBridge = 12
  val gooses = Set(5, 9, 14, 18, 23, 27)

  def move(board: Board, player: String, die1: Int, die2: Int): (Boolean, Board) = {
    val newPosition = board.positions(player) + die1 + die2
    print(s"$player rolls $die1, $die2. ")
    if (newPosition == lastTile) { // Terminating condition
      print(s"$player moves from ${board.positions(player)} to $newPosition. ")
      print(s"$player Wins!!!\n")
      (true, board.copy(positions = board.positions.updated(player, lastTile)))
    } else if (newPosition > lastTile) { // Go beyond the last tile, boncing back
      val endingPosition = lastTile - (newPosition - lastTile)
      print(s"$player moves from ${board.positions(player)} to $lastTile. $player bounces! $player returns to $endingPosition.\n")
      (false, board.copy(positions = board.positions.updated(player, endingPosition)))
    } else if (bridge == newPosition) { // Moving to the bridge
      print(s"$player moves from 4 to The Bridge. Pippo jumps to $endBridge.\n")
      (false, board.copy(positions = board.positions.updated(player, endBridge)))
    } else if (gooses(newPosition)) { // Moving on a goose
      print(s"$player moves from ${board.positions(player)} to $newPosition, The Goose. ")
      var nextPosition = newPosition + die1 + die2
      // While it's not pure, the result is always the same for the two dice roll
      while (gooses(nextPosition)) {
        print(s"$player moves again and goes to $nextPosition, The Goose. ")
        nextPosition = nextPosition + die1 + die2
      }
      print(s"$player moves again and goes to $nextPosition.\n")
      (false, board.copy(positions = board.positions.updated(player, nextPosition)))
    } else { // Otherwise just move up
      print(s"$player moves from ${board.positions(player)} to $newPosition.\n")
      (false, board.copy(positions = board.positions.updated(player, newPosition)))
    }
  }

  def run(board: Board = Board(Set(), Map())): Unit = {
    // geting the input
    val input = readLine()
    step(input, board) match {
      // Game has ended
      case (true, _) =>
        println("Game has ended.")
      // Otherwise wait for the next input
      case (false, newBoard) =>
        run(newBoard)
    }
  }
}

case class Board(players: Set[String], positions: Map[String, Int])

trait Greeting {
  lazy val greeting: String = "Welcome to The Goose Game!"
}
