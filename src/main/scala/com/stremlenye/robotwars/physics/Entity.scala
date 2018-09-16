package com.stremlenye.robotwars.physics

import java.util.UUID

import cats._
import cats.implicits._
import com.sksamuel.scrimage.Color
import com.stremlenye.robotwars.{Robot, Size}
import com.stremlenye.robotwars.rendering.RenderData

final case class Velocity(x : Double, y : Double)

object Velocity {
  def zero : Velocity = Velocity(0D, 0D)

  implicit val monoid : Monoid[Velocity] = new Monoid[Velocity] {
    override def empty : Velocity = zero

    override def combine(l : Velocity, r : Velocity) : Velocity =
      Velocity(l.x + r.x, l.y + r.y)
  }
}

sealed trait Entity {
  def id : UUID

  def transparent : Boolean

  /**
    * The factor by witch the speed of movement should be reduced passing each point of space
    */
  def inertiaFactor : Double

  /**
    * The factor which determines how the speed of the colliding entity will change on impact with current one
    */
  def absorptionFactor : Double

  def velocity : Velocity = Velocity.zero
}

object Entity {
  implicit val eq : Eq[Entity] = Eq.by(_.id)

  implicit val renderData : RenderData[Entity] = new RenderData[Entity] {
    override def color(a : Entity) : Color = a match {
      case _ : Floor => Color(0,0,0,0)
      case _ : Actor => Color.awt2color(java.awt.Color.GREEN)
    }

    override def size(a : Entity) : Size = Size(1, 1)
  }
}

trait Passable { self : Entity =>
  val absorptionFactor : Double = 0D
}

trait Immobile { self : Entity =>
  val vector : Velocity = Velocity.zero

  val inertiaFactor : Double = 0D
}

case class Floor(id : UUID) extends Entity with Passable with Immobile {
  override def transparent : Boolean = true
}

case class Actor(id : UUID, robot : Robot) extends Entity  {
  override def transparent : Boolean = false

  val inertiaFactor : Double = 1D

  val absorptionFactor : Double = 1D

  override def velocity : Velocity = Velocity(10,10)
}
