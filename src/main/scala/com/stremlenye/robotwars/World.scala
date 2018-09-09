package com.stremlenye.robotwars

import java.util.UUID

import cats._
import cats.data._
import cats.implicits._

case class Size(width : Int, height : Int)

object Size {
  val zero = Size(0, 0)
}

case class Point(x : Int, y : Int)

object Point {
  implicit val ordering : Ordering[Point] = (l : Point, r : Point) => l.x * l.y compare r.x * r.y

  implicit val monoid : Monoid[Point] = new Monoid[Point] {
    override def empty : Point = Point(0, 0)

    override def combine(l : Point, r : Point) : Point =
      Point(
        x = l.x + r.x,
        y = l.y + r.y
      )
  }

  implicit val eq : Eq[Point] = Eq.by(p => p.x -> p.y)
}

trait Entity {
  def id : UUID

  def transparent : Boolean

  def passable : Boolean
}

object Entity {
  implicit val eq : Eq[Entity] = Eq.by(_.id)
}

case class Floor(id : UUID) extends Entity {
  override def transparent : Boolean = true

  override def passable : Boolean = true
}

case class Water(id : UUID) extends Entity {
  override def transparent : Boolean = true

  override def passable : Boolean = false
}

case class Wall(id : UUID) extends Entity {
  override def transparent : Boolean = false

  override def passable : Boolean = false
}

case class Actor(id : UUID) extends Entity {
  override def transparent : Boolean = false

  override def passable : Boolean = true
}

trait View[A] {
  def size(a : A) : Size

  def pick(a : A)(point : Point) : Option[Chain[Entity]]
}

case class World(surface : Map[Point, Chain[Entity]]) {

  def topLeftCorner : Option[Point] =
    Either.catchNonFatal(surface.keys.min).toOption

  def bottomRightCorner : Option[Point] =
    Either.catchNonFatal(surface.keys.max).toOption

  def size : Size = (for {
    tlCorner <- topLeftCorner
    brCorner <- bottomRightCorner
  } yield Size(brCorner.x - tlCorner.x, brCorner.y - tlCorner.y)).getOrElse(Size.zero)

  def pickRect(center : Point, radius : Int) : World = {
    val points = (for {
      x <- (radius * -1) to radius
      y <- (radius * -1) to radius
    } yield
      surface
        .get(Point(center.x + x, center.y + y))
        .map(Point(x + radius, y + radius) -> _))
      .collect { case Some(a) => a }
      .toMap

    World(points)
  }

  def putEntity(point: Point, entity: Entity) : World =
    World(surface.updated(point, surface.getOrElse(point, Chain.empty[Entity]).append(entity)))

  def combine(world : World) : Option[World] = World.combine(this, world)
}

object World {
  implicit val view : View[World] = new View[World] {
    override def size(a : World) : Size = a.size

    override def pick(a : World)(point : Point) : Option[Chain[Entity]] = a.surface.get(point)
  }

  private case class NonPassableObjectsCounter(i : Int) {
    def valid : Boolean = i <= 0

    def combine(e : Entity) : NonPassableObjectsCounter =
      if (e.passable) this else NonPassableObjectsCounter(i + 1)
  }

  def combine(l : World, r : World) : Option[World] = {
    Option(World(l.surface.combine(r.surface)))
      .filterNot(_.surface.exists {
        case (_, entities) => entities
          .foldl(NonPassableObjectsCounter(0))(_ combine _).valid == false
      })
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
