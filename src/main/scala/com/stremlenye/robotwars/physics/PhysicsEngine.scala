package com.stremlenye.robotwars.physics

import cats._
import cats.data._
import cats.implicits._
import cats.tagless._
import mouse.option._
import com.stremlenye.robotwars.Coordinate
import com.stremlenye.robotwars.utils.algebras.LoggingAlgebra

final case class EntityTransition(from : Entity, to : Entity)

object EntityTransition {
  def noop(entity : Entity) : EntityTransition = EntityTransition(from = entity, to = entity)
}

final case class MovementTransition(entityTransition : EntityTransition, from : Coordinate, to : Coordinate)

@finalAlg
@autoFunctorK
trait PhysicsEngineAlgebra[F[_]] {
  def compute(entity : Entity, coordinate : Coordinate,
              checkCoordinate : Coordinate => Option[NonEmptyVector[Entity]]) : F[NonEmptyVector[MovementTransition]]
}

object PhysicsEngineAlgebra {

  final case class EngineError(message : String) extends Exception(message)

  private[physics] def nextCoordinate(coordinate : Coordinate, velocity : Velocity) : Coordinate = {
    Coordinate(
      (coordinate.x + velocity.x).ceil.toInt,
      (coordinate.y + velocity.y).ceil.toInt
    )
  }

  private[physics] def getDirectionFactor(a : Double) : Int = Either.catchNonFatal((a / a).ceil.toInt).getOrElse(1)

  private[physics] def xDirectionFactor(v : Velocity) : Int = getDirectionFactor(v.x)

  private[physics] def yDirectionFactor(v : Velocity) : Int = getDirectionFactor(v.y)

  def nextVelocity(velocity : Velocity, inertiaFactor : Double) : Velocity =
    velocity |+| Velocity(
      -1 * inertiaFactor * xDirectionFactor(velocity),
      -1 * inertiaFactor * yDirectionFactor(velocity)
    )

  def apply[F[_]](logger : LoggingAlgebra[F])(implicit F : MonadError[F, Throwable]) : PhysicsEngineAlgebra[F] =
    new PhysicsEngineAlgebra[F] {
      def compute(entity : Entity,
                  coordinate : Coordinate,
                  checkCoordinate : Coordinate => Option[NonEmptyVector[Entity]]) : F[NonEmptyVector[MovementTransition]] = {
        for {
          _ <- logger.trace(s"Computing movement transitions for ${entity.show} with velocity ${entity.velocity.show} from ${coordinate.show}")
          newCoordinate <- F.pure(nextCoordinate(coordinate, entity.velocity))
          _ <- logger.trace(s"Checking coordinate ${newCoordinate.show} for ${entity.show}")
          _ <- checkCoordinate(newCoordinate).cata(F.pure, F.raiseError(EngineError("No reactors were found")))
          newVelocity <- F.pure(nextVelocity(entity.velocity, entity.inertiaFactor))
          _ <- logger.trace(s"Velocity changes for ${entity.show} from ${entity.velocity.show} to ${newVelocity.show} ")
        } yield NonEmptyVector.one(
          MovementTransition(
            entityTransition = EntityTransition(entity, entity.updateVelocity(newVelocity)),
            from = coordinate,
            to = newCoordinate
          )
        )
      }
    }
}
