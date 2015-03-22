package my

import my.Game._

import scala.collection.mutable
import scala.io.Source

object PlayerApp extends App {
  def printMove(decision: Move): Unit = {
    decision.fleetOrders.foreach(c => println(s"F ${c.fromPlanetId} ${c.toPlanetId} ${c.fleetSize}"))
    decision.planetsToUpgrade.foreach(p => println(s"B $p"))
    println('.')
  }

  def parsePlanetOwnerId(value: String): Option[Int] = value.toInt match {
    case 0 => None
    case id => Some(id)
  }

  def parsePlanetLine(line: String): Planet = {
    val tokens = line.split(' ')
    require(tokens.length == 7, s"unexpected planet line: $line")
    Planet(
      id = tokens(1).toInt,
      coordinates = Point(tokens(2).toFloat, tokens(3).toFloat),
      shipProduction = tokens(4).toInt,
      ownerId = parsePlanetOwnerId(tokens(5)),
      fleetSize = tokens(6).toInt)
  }

  def parseSelfPlayerIdLine(line: String): Int = {
    val tokens = line.split(' ')
    require(tokens.length == 2, s"unexpected self player ID line: $line")
    tokens(1).toInt
  }

  val playerFactory = Bot
  var playerId: Option[Int] = None
  var player: Option[Player] = None
  val planets = mutable.ArrayBuffer.empty[Planet]

  for {
    rawLine <- Source.stdin.getLines()
    line = rawLine.trim
    if !line.isEmpty
  } {
    line.charAt(0) match {
      case 'P' =>
        planets += parsePlanetLine(line)
      case 'Y' =>
        playerId = Some(parseSelfPlayerIdLine(line))
      case '.' =>
        val updatedPlayerInstance = player match {
          case Some(oldPlayerInstance) =>
            oldPlayerInstance.nextTurn(getPlanetsState(planets))
            oldPlayerInstance
          case None =>
            val newPlayerInstance = playerFactory(playerId.get, GameMap(planets), getPlanetsState(planets))
            player = Some(newPlayerInstance)
            newPlayerInstance
        }
        printMove(updatedPlayerInstance.makeMove)
        planets.clear()
    }
  }
}
