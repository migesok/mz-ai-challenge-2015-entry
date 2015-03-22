package my

import my.Game.GameMap.PlanetInfo

import scala.concurrent.duration._
import scala.math._

/**
 * Planet Wars game logic
 */
object Game {
  val TurnsMax = 200
  val PlanetsMax = 100
  val TeamSizeMax = 10
  val BotStartTimeMax = 10.seconds
  val BotMakeMoveTimeMax = 1.second

  type PlanetId = Int
  type PlayerId = Int

  case class Point(x: Float, y: Float)

  def turnsToTravel(a: Point, b: Point): Int = {
    ceil(sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2))).toInt
  }

  def turnsToTravel(a: Planet, b: Planet): Int = turnsToTravel(a.coordinates, b.coordinates)

  def turnsToTravel(a: PlanetInfo, b: PlanetInfo): Int = turnsToTravel(a.coordinates, b.coordinates)

  case class Planet(
    id: PlanetId,
    coordinates: Point,
    shipProduction: Int,
    ownerId: Option[Int],
    fleetSize: Int)

  case class PlanetState(ownerId: Option[PlayerId], fleetSize: Int, production: Int) {
    require(fleetSize >= 0, "size of fleet to send must be a non-negative number")
  }

  def getPlanetsState(planets: Iterable[Planet]): Map[PlanetId, PlanetState] = planets
    .map(p => (p.id, p))
    .toMap
    .mapValues(p => PlanetState(p.ownerId, p.fleetSize, p.shipProduction))

  case class SendFleet(fromPlanetId: Int, toPlanetId: Int, fleetSize: Int) {
    require(fromPlanetId != toPlanetId, "starting and destination planets must not be the same")
    require(fleetSize > 0, "size of fleet to send must be a positive number")
  }

  case class Move(fleetOrders: List[SendFleet] = Nil, planetsToUpgrade: Set[PlanetId] = Set.empty)

  /**
   * All the things about planets that do not change during the game:
   * - coordinates
   * - distances
   * - ship production rates
   */
  case class GameMap private(planetsInfo: Map[PlanetId, PlanetInfo])

  object GameMap {
    def apply(planets: Iterable[Planet]): GameMap = GameMap(
      planets
        .map(p => (p.id, p))
        .toMap
        .mapValues(p => PlanetInfo(p.coordinates, computeDistances(p, planets))))

    private[this] def computeDistances(from: Planet, planets: Iterable[Planet]): List[(PlanetId, Int)] =
      planets.collect { case to if to.id != from.id => (to.id, turnsToTravel(from, to))}
        .toList
        .sortBy { case (planetId, distance) => distance}

    /**
     *
     * @param coordinates planet's coordinates
     * @param distances   distances between this planet and others sorted in ascending order
     */
    case class PlanetInfo(coordinates: Point, distances: List[(PlanetId, Int)])

  }

  trait Player {
    def id: PlayerId

    def makeMove: Move

    def nextTurn(updatedPlanetsState: Map[PlanetId, PlanetState]): Unit
  }

  trait PlayerFactory[P <: Player] {
    def apply(id: PlayerId, map: GameMap, initialPlanetsState: Map[PlanetId, PlanetState]): P
  }

}
