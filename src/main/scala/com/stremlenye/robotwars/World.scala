package com.stremlenye.robotwars

import cats._
import cats.data._
import cats.implicits._
import com.stremlenye.robotwars.physics._

case class Size(width : Int, height : Int)

object Size {
  val zero = Size(0, 0)
}

case class Coordinate(x : Int, y : Int)

object Coordinate {
  implicit val ordering : Ordering[Coordinate] =
    (l : Coordinate, r : Coordinate) => {
      l.x * l.y compare r.x * r.y
    }

  implicit val show : Show[Coordinate] = Show.show(c => s"${c.x}x${c.y}")

  implicit val monoid : Monoid[Coordinate] = new Monoid[Coordinate] {
    override def empty : Coordinate = Coordinate(0, 0)

    override def combine(l : Coordinate, r : Coordinate) : Coordinate =
      Coordinate(
        x = l.x + r.x,
        y = l.y + r.y
      )
  }

  implicit val eq : Eq[Coordinate] = Eq.by(p => p.x -> p.y)
}


trait View[A] {
  def size(a : A) : Size

  def pick(a : A)(point : Coordinate) : Option[NonEmptyVector[Entity]]
}

case class World(surface : Map[Coordinate, NonEmptyVector[Entity]]) {

  def topLeftCorner : Option[Coordinate] =
    Either.catchNonFatal(surface.keys.min).toOption

  def bottomRightCorner : Option[Coordinate] =
    Either.catchNonFatal(surface.keys.max).toOption

  val size : Size = (for {
    tlCorner <- topLeftCorner
    brCorner <- bottomRightCorner
  } yield Size(brCorner.x - tlCorner.x, brCorner.y - tlCorner.y)).getOrElse(Size.zero)

  def pickRect(center : Coordinate, radius : Int) : World = {
    val points = (for {
      x <- (radius * -1) to radius
      y <- (radius * -1) to radius
    } yield
      surface
        .get(Coordinate(center.x + x, center.y + y))
        .map(Coordinate(x + radius, y + radius) -> _))
      .collect { case Some(a) => a }
      .toMap

    World(points)
  }

  private def stackEntities(entity: Entity, entities : NonEmptyVector[Entity]) : Option[NonEmptyVector[Entity]] = {
    Option(entities).map(_.append(entity))
  }

  def putEntity(point: Coordinate, entity: Entity) : Option[World] =
    for {
      entities <- surface.get(point)
      stacked <- stackEntities(entity, entities)
    } yield World(surface.updated(point, stacked))

  def removeEntity(point: Coordinate, entity: Entity) : Option[World] =
    for {
      entities <- surface.get(point)
      updated <- Option(entities.filterNot(_.id === entity.id))
        .flatMap(NonEmptyVector.fromVector)
    } yield World(surface.updated(point, updated))

}

object World {
  implicit val view : View[World] = new View[World] {
    override def size(a : World) : Size = a.size

    override def pick(a : World)(point : Coordinate) : Option[NonEmptyVector[Entity]] = a.surface.get(point)
  }
}

trait Stats[A] {
  def hp(a : A) : Int
}

sealed trait Direction

case object Up extends Direction

case object Down extends Direction

case object Left extends Direction

case object Right extends Direction

sealed trait Reaction

case object NoReaction extends Reaction

case class Move(direction : Direction) extends Reaction

trait Robot {
  def name : String

  def react[A : View, B : Stats](a : A, b : B) : Reaction
}

object Robot {
  def dummy : Robot = new Robot {
    override def name : String = "Dummy"

    override def react[A : View, B : Stats](a : A, b : B) : Reaction = NoReaction
  }
}
