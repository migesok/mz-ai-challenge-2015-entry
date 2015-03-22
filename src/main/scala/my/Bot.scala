package my

import my.Bot.{OngoingPlanetAttack, ScheduledFleetOrder}
import my.Game._

import scala.collection.mutable
import scala.math._

class Bot(
  val id: PlayerId,
  val map: GameMap,
  val initialPlanetsState: Map[PlanetId, PlanetState]) extends Player {

  private[this] var turn: Int = 0

  private[this] var currentPlanetsState = initialPlanetsState
  private[this] val reservedFleets = mutable.Map.empty[PlanetId, Int].withDefaultValue(0)
  private[this] var scheduledFleetOrders: Set[ScheduledFleetOrder] = Set.empty
  private[this] var ongoingPlanetAttacks = Map.empty[PlanetId, OngoingPlanetAttack]
  private[this] var currentPlanetsToUpgrade = Set.empty[PlanetId]

  updateAttackPlans()
  currentPlanetsToUpgrade = planUpgrades()

  override def makeMove = Move(currentTurnOrders, currentPlanetsToUpgrade)

  override def nextTurn(updatedPlanetsState: Map[PlanetId, PlanetState]): Unit = {
    incrementTurn()
    currentPlanetsState = updatedPlanetsState
    updateAttackPlans()
    currentPlanetsToUpgrade = planUpgrades()
  }

  private[this] def planUpgrades(): Set[PlanetId] =
    (for {
      (planet, PlanetState(Some(`id`), fleet, production)) <- currentPlanetsState
      availableFleet = fleet - reservedFleets(planet)
      upgradeCost = ceil(pow(2, production)).toInt
      if availableFleet > upgradeCost
      remainingTurns = Game.TurnsMax - turn
      futureShipsProduced = remainingTurns * production
      futureShipsProducedWithUpgrade = remainingTurns * (production + 1) - upgradeCost
      if futureShipsProducedWithUpgrade >= futureShipsProduced * 2
    } yield planet).toSet

  private[this] def updateAttackPlans(): Unit = {
    val enemyPlanetsState = getEnemyPlanetsState
    val neutralPlanetsState = getNeutralPlanetsState

    //removing scheduled orders targeting planets which changed owners so now it is unreasonable to attack
    unscheduleOrders(scheduledFleetOrders.filter(order =>
      !enemyPlanetsState.contains(order.to) && !neutralPlanetsState.contains(order.to)))
    ongoingPlanetAttacks = ongoingPlanetAttacks.filterKeys(planetId =>
      enemyPlanetsState.contains(planetId) || neutralPlanetsState.contains(planetId))
    unscheduleImpossibleOrders()

    val alreadyTargetedPlanets = ongoingPlanetAttacks.keySet
    val newTargets: Set[PlanetId] = (enemyPlanetsState.keySet ++ neutralPlanetsState.keySet -- alreadyTargetedPlanets)
      .filter(p => currentPlanetsState(p).production >= 0)
    val prioritizedTargets = newTargets.toList.sortBy(getTargetPlanetPriority)

    var remainingTargets = prioritizedTargets
    while (remainingTargets.nonEmpty) {
      val target = remainingTargets.head
      val attackOrders = planAttack(target)
      if (attackOrders.nonEmpty) {
        scheduleOrders(attackOrders)
      }
      remainingTargets = remainingTargets.tail
    }
  }

  private[this] def scheduleOrders(orders: Iterable[ScheduledFleetOrder]): Unit = {
    scheduledFleetOrders = scheduledFleetOrders ++ orders
    orders.foreach(reserveFleetForOrder)
    val newAttacks = for {
      (target, targetOrders) <- orders.groupBy(_.to)
      attackFinishInTurns = targetOrders.map(getScheduledOrderTurnsToExecute).max
    } yield (target, OngoingPlanetAttack(target, attackFinishInTurns))
    ongoingPlanetAttacks = ongoingPlanetAttacks ++ newAttacks
  }

  private[this] def unscheduleImpossibleOrders(): Unit = {
    val currentTurnOrders = impossibleCurrentTurnOrders
    unscheduleOrders(currentTurnOrders)
    val targetsToCancel = currentTurnOrders.map(_.to)
    unscheduleOrders(scheduledFleetOrders.filter(o => targetsToCancel(o.to)))
    ongoingPlanetAttacks = ongoingPlanetAttacks.filterKeys(p => !targetsToCancel(p))
  }

  private[this] def impossibleCurrentTurnOrders = for {
    o@ScheduledFleetOrder(from, to, fleetSize, 0) <- scheduledFleetOrders
    fromPlanetState = currentPlanetsState(from)
    if fromPlanetState.fleetSize < fleetSize || fromPlanetState.ownerId != Some(id)
  } yield o

  private[this] def unscheduleOrders(orders: Iterable[ScheduledFleetOrder]): Unit = {
    scheduledFleetOrders = scheduledFleetOrders -- orders
    orders.foreach(dropFleetReservationForOrder)
  }

  private[this] def reserveFleetForOrder(order: ScheduledFleetOrder): Unit = {
    val planetReservedFleet = reservedFleets(order.from)
    reservedFleets.update(order.from, planetReservedFleet + order.fleetSize)
  }

  private[this] def dropFleetReservationForOrder(order: ScheduledFleetOrder): Unit = {
    val planetReservedFleet = reservedFleets(order.from)
    val newReservedFleet = planetReservedFleet - order.fleetSize
    require(newReservedFleet >= 0, "reserved fleet size must be non-negative")
    reservedFleets.update(order.from, newReservedFleet)
  }

  private[this] def incrementTurn(): Unit = {
    turn += 1

    val (executedOrders, pendingOrders) = scheduledFleetOrders.partition(_.afterTurns == 0)
    executedOrders.foreach(dropFleetReservationForOrder)
    scheduledFleetOrders = pendingOrders.map(o => o.copy(afterTurns = o.afterTurns - 1))

    ongoingPlanetAttacks = ongoingPlanetAttacks
      .filter { case (planetId, attack) => attack.turnsRemaining > 1}
      .mapValues(a => a.copy(turnsRemaining = a.turnsRemaining - 1))
  }

  private[this] def currentTurnOrders: List[SendFleet] = (for {
    ScheduledFleetOrder(from, to, fleetSize, afterTurns) <- scheduledFleetOrders
    if afterTurns == 0
  } yield SendFleet(from, to, fleetSize))
    .toList

  private[this] def planAttack(target: PlanetId) = {
    val targetInfo = map.planetsInfo(target)
    val targetCurrentState = currentPlanetsState(target)

    def planAttackFromPlanets(
      observedPlanetsDistances: List[(PlanetId, Int)],
      remainingPlanetsDistances: List[(PlanetId, Int)]): List[ScheduledFleetOrder] = remainingPlanetsDistances match {
      case Nil => Nil
      case planetDistance :: newRemainingPlanetsDistances =>
        val planetsDistances = planetDistance :: observedPlanetsDistances
        val turnsForLastFleetArrival = planetDistance._2
        if (turnsForLastFleetArrival + turn > Game.TurnsMax) //unreasonable to plan attacks after the end of a game
          Nil
        else {
          val enemyFleetOnArrival = forecastPlanetFleet(targetCurrentState, turnsForLastFleetArrival)
          val desiredAttackFleet: Int = enemyFleetOnArrival + ceil(enemyFleetOnArrival * 0.2).toInt
          val scheduledOrdersFullAttack = for {
            (planetId, distance) <- planetsDistances
            waitBeforeSendTurns = turnsForLastFleetArrival - distance
            currentState = currentPlanetsState(planetId)
            info = map.planetsInfo(planetId)
            shipsReserved = reservedFleets(planetId)
            availableFleet = getAvailablePlanetFleet(currentState, shipsReserved, waitBeforeSendTurns)
            if availableFleet > 0
          } yield ScheduledFleetOrder(
              from = planetId,
              to = target,
              fleetSize = availableFleet,
              afterTurns = waitBeforeSendTurns)
          val fullAttackFleet = scheduledOrdersFullAttack.map(_.fleetSize).sum

          if (fullAttackFleet < desiredAttackFleet)
            planAttackFromPlanets(planetsDistances, newRemainingPlanetsDistances)
          else
            scheduledOrdersFullAttack
        }
    }

    val myPlanetsDistances = targetInfo.distances.filter {
      case (planetId, _) => getMyPlanetsState.contains(planetId)
    }

    planAttackFromPlanets(Nil, myPlanetsDistances)
  }

  private[this] def forecastPlanetFleet(currentState: PlanetState, turnsFromNow: Int): Int =
    if (currentState.ownerId.isDefined) currentState.fleetSize + currentState.production * turnsFromNow
    else currentState.fleetSize

  //TODO: defend fleet size?
  private[this] def getAvailablePlanetFleet(
    currentState: PlanetState,
    shipsReserved: Int,
    turnsFromNow: Int): Int =
    forecastPlanetFleet(currentState, turnsFromNow) - shipsReserved

  private[this] def getTargetPlanetPriority(planetId: PlanetId): Double = {
    val state = currentPlanetsState(planetId)
    val info = map.planetsInfo(planetId)
    val myPlanets = getMyPlanetsState.keySet
    val Some(minDistanceToMyPlanet) = info.distances.collectFirst {
      case (anotherPlanet, distance) if myPlanets(anotherPlanet) => distance
    }
    state.production.toDouble / state.fleetSize / minDistanceToMyPlanet
  }

  private[this] def getMyPlanetsState: Map[PlanetId, PlanetState] = currentPlanetsState.filter {
    case (_, state) => state.ownerId == Some(id)
  }

  private[this] def getEnemyPlanetsState: Map[PlanetId, PlanetState] = currentPlanetsState.filter {
    case (_, state) => state.ownerId.exists(_ != id)
  }

  private[this] def getNeutralPlanetsState: Map[PlanetId, PlanetState] = currentPlanetsState.filter {
    case (_, state) => state.ownerId == None
  }

  private[this] def getScheduledOrderTurnsToExecute(order: ScheduledFleetOrder): Int =
    order.afterTurns + turnsToTravel(map.planetsInfo(order.from), map.planetsInfo(order.to))
}

object Bot extends PlayerFactory[Bot] {

  override def apply(id: PlayerId, map: GameMap, initialPlanetsState: Map[PlanetId, PlanetState]) =
    new Bot(id, map, initialPlanetsState)

  private case class ScheduledFleetOrder(from: PlanetId, to: PlanetId, fleetSize: Int, afterTurns: Int) {
    require(fleetSize > 0, "fleet size must be positive")
    require(afterTurns >= 0, "remaining turns count must be non-negative")
  }

  private case class OngoingPlanetAttack(target: PlanetId, turnsRemaining: Int) {
    require(turnsRemaining > 0, "remaining turns count must be positive")
  }

}
