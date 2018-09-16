package com.stremlenye.robotwars.physics

import java.util.UUID

import cats._
import cats.implicits._
import com.github.ghik.silencer.silent
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

  implicit val show : Show[Velocity] = Show.show(a => s"${a.x}x${a.y}")
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

  def velocity : Velocity

  def updateVelocity(velocity: Velocity) : Entity
}

object Entity {

  implicit val show : Show[Entity] = Show.show({
    case Floor(id) => s"Floor[$id]"
    case Actor(id, _, _) => s"Actor[$id]"
  })

  implicit val eq : Eq[Entity] = Eq.by(_.id)

  implicit val renderData : RenderData[Entity] = new RenderData[Entity] {
    def color(a : Entity) : Color = a match {
      case _ : Floor => Color.awt2color(java.awt.Color.black)
      case _ : Actor => Color.awt2color(java.awt.Color.white)
    }

    def size(a : Entity) : Size = Size(1, 1)

    def renderPriority(a : Entity) : Int = a match {
      case _ : Floor => 0
      case _ : Actor => 100
    }
  }
}

trait Passable { self : Entity =>
  val absorptionFactor : Double = 0D
}

trait Immobile { self : Entity =>
  val velocity : Velocity = Velocity.zero

  val inertiaFactor : Double = 0D

  @silent
  def updateVelocity(v : Velocity) : Entity = self
}

case class Floor(id : UUID) extends Entity with Passable with Immobile {
  override def transparent : Boolean = true
}

case class Actor(id : UUID, robot : Robot, velocity: Velocity) extends Entity  {
  override def transparent : Boolean = false

  val inertiaFactor : Double = 0D

  val absorptionFactor : Double = 1D

  override def updateVelocity(v : Velocity) : Entity = this.copy(velocity = v)
}
