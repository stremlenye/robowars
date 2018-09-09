package com.stremlenye.robotwars

import java.util.UUID

import cats.data._
import cats.implicits._

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

final case class RemoveEntity(point : Point, entity : Entity) extends Intention

final case class PutEntity(point : Point, entity : Entity) extends Intention

case class Game(length : Long, world : World, gameSettings : GameSettings, robots : Seq[Robot])

object Game {

  def run(game : Game) : Stream[Frame] =
    Stream
      .iterate(Frame(0, game.world, players(game.robots, game.gameSettings.defaultPlayerStats)))
      { prev => nextFrame(prev, game.gameSettings) }

  private def players(robots : Seq[Robot], playerStats : PlayerStats) : Set[Player] =
    robots
      .map(Player(UUID.randomUUID(), _, playerStats))
      .toSet

  def getFieldOfView(world : World, point : Point, fov : Int) : World =
    world.pickRect(point, fov)

  def collectReactionsForActor(actor : Actor, point : Point, frame : Frame, gameSettings : GameSettings) : (Point, Actor, Reaction) =
    (point,
      actor,
      frame.players
        .find(_.id === actor.id)
        .map { player => player.robot.react(getFieldOfView(frame.world, point, gameSettings.fov), player.stats)
        }
        .getOrElse(NoReaction))

  def collectReactions(frame : Frame, gameSettings : GameSettings) : Chain[(Point, Actor, Reaction)] =
    Chain
      .fromSeq(frame.world.surface.map {
        case (p, c) =>
          c.collect {
            case a : Actor => a
          }
            .map(collectReactionsForActor(_, p, frame, gameSettings))
      }.toSeq)
      .flatten

  def getEntityIntentions(point : Point, entity : Entity, reaction : Reaction, gameSettings : GameSettings) : Seq[Intention] =
    reaction match {
      case NoReaction => Seq.empty
      case Move(direction) => MovementIntention.move(entity, direction, gameSettings.defaultSpeed, point)
    }

  def applyToWorld(world : World, intention : Intention) : World =
    intention match {
      case RemoveEntity(point, _) =>
        World(world.surface.collect {
          case a @ (p, _) if p =!= point => a
        })
      case PutEntity(point, entity) => world.putEntity(point, entity)
    }

  def nextFrame(frame : Frame, gameSettings : GameSettings) : Frame = {
    val reactions : Chain[(Point, Actor, Reaction)] = collectReactions(frame, gameSettings)
    val intentions : Chain[Seq[Intention]] = reactions.map {
      case (point, actor, reaction) => getEntityIntentions(point, actor, reaction, gameSettings)
    }
    val newWorld = intentions
      .map(
        x =>
          Chain
            .fromSeq(x)
            .map(applyToWorld(frame.world, _))
            .foldl(Option(frame.world)) { (optW, candidate) => optW.flatMap(_ combine candidate)
            })
      .collect {
        case Some(world) => world
      }
      .foldl(frame.world) { (init, candidate) => init.combine(candidate).getOrElse(init)
      }

    Frame(frame.index + 1, newWorld, frame.players)
  }
}

object MovementIntention {
  private def movementVector(direction : Direction, speed : Int) : Point =
    direction match {
      case Up => Point(0, speed)
      case Down => Point(0, -1 * speed)
      case Right => Point(speed, 0)
      case Left => Point(-1 * speed, 0)
    }

  def move(entity : Entity, direction : Direction, speed : Int, point : Point) : Seq[Intention] =
    Seq(RemoveEntity(point, entity), PutEntity(point |+| movementVector(direction, speed), entity))
}
