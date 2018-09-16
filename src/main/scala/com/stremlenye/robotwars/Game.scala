package com.stremlenye.robotwars

import java.util.UUID

import cats.data._
import cats.implicits._
import com.stremlenye.robotwars.physics._

case class Frame(index : Long, world : World, players : Set[Player])

case class PlayerStats(hp : Int) {
  val isDead : Boolean = hp <= 0
}

object PlayerStats {
  implicit val stats : Stats[PlayerStats] = _.hp
}

case class Player(id : UUID, robot : Robot, stats : PlayerStats)

case class GameSettings(defaultPlayerStats : PlayerStats, defaultSpeed : Int, fov : Int)

sealed trait Intention

final case class RemoveEntity(point : Coordinate, entity : Entity) extends Intention

final case class PutEntity(point : Coordinate, entity : Entity) extends Intention

case class Game(length : Long, world : World, gameSettings : GameSettings, robots : Seq[Robot])

object Game {

  def run(game : Game) : Stream[Frame] =
    Stream
      .iterate(Frame(0, game.world, players(game.robots, game.gameSettings.defaultPlayerStats))) { prev =>
        nextFrame(prev)
      }.take(game.length.toInt)

  private def players(robots : Seq[Robot], playerStats : PlayerStats) : Set[Player] =
    robots
      .map(Player(UUID.randomUUID(), _, playerStats))
      .toSet

  private def applyTransition(world : World, movementTransition : MovementTransition) : Option[World] =
    for {
      w1 <- world.removeEntity(movementTransition.from, movementTransition.entityTransition.from)
      w2 <- w1.putEntity(movementTransition.to, movementTransition.entityTransition.to)
    } yield w2

  def nextFrame(frame : Frame) : Frame = {
    println(s"${Console.YELLOW}>>>> frame = ${frame.index}${Console.RESET}")
    val newWorld = frame.world.surface.toVector.foldl(frame.world) {
      (w : World, p : (Coordinate, NonEmptyVector[Entity])) =>
        val (coordinate, entities) = p
//        println(s"${Console.YELLOW}>>>> coordinate = ${coordinate}${Console.RESET}")
        entities.map(e => Engine.compute(e, coordinate, frame.world.surface.get))
          .collect {
            case Some(l) => l.toVector
          }.flatten.foldl(Option(w)) { (optW, transition) =>
          optW.flatMap(applyTransition(_, transition))
        }.getOrElse(w)
    }
    Frame(frame.index + 1, newWorld, frame.players)
  }
}

object MovementIntention {
  private def movementVector(direction : Direction, speed : Int) : Coordinate =
    direction match {
      case Up => Coordinate(0, speed)
      case Down => Coordinate(0, -1 * speed)
      case Right => Coordinate(speed, 0)
      case Left => Coordinate(-1 * speed, 0)
    }

  def move(entity : Entity, direction : Direction, speed : Int, point : Coordinate) : Seq[Intention] =
    Seq(RemoveEntity(point, entity), PutEntity(point |+| movementVector(direction, speed), entity))
}
