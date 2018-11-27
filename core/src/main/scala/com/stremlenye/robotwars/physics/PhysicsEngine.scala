package com.stremlenye.robotwars.physics

import cats._
import cats.data._
import cats.implicits._
import cats.tagless._
import mouse.option._
import com.stremlenye.robotwars.Coordinate
import com.stremlenye.robotwars.mtl.FunctorKDerivation
import com.stremlenye.robotwars.utils.algebras.LoggingAlgebra

final case class EntityTransition(from : Entity, to : Entity)

object EntityTransition {
  def noop(entity : Entity) : EntityTransition = EntityTransition(from = entity, to = entity)
}

final case class MovementTransition(entityTransition : EntityTransition, from : Coordinate, to : Coordinate)

trait PhysicsEngineAlgebra[F[_]] {
  def compute(entity : Entity, coordinate : Coordinate,
              checkCoordinate : Coordinate => Option[NonEmptyVector[Entity]]) : F[NonEmptyVector[MovementTransition]]
}

object PhysicsEngineAlgebra {
  implicit def functorK : FunctorK[PhysicsEngineAlgebra] = FunctorKDerivation.apply[PhysicsEngineAlgebra]

  final case class EngineError(message : String) extends Exception(message)

  private[physics] def nextCoordinate(coordinate : Coordinate, velocity : Velocity) : Coordinate = {
    Coordinate(
      (coordinate.x + velocity.x).ceil.toInt,
      (coordinate.y + velocity.y).ceil.toInt
    )
  }

  private[physics] def path(start : Coordinate, end : Coordinate) : NonEmptyVector[Coordinate] = {
    NonEmptyVector.fromVectorUnsafe((
      for {
        x <- start.x to end.x
        y <- start.y to end.y
      } yield Coordinate(x, y)
      ).toVector)
  }

  private[physics] def pathCollisionReverseLookup(path : NonEmptyVector[Coordinate],
                                                  check : Coordinate => Option[NonEmptyVector[Entity]]) : Option[(Coordinate, NonEmptyVector[Entity])] = {
    path.reverse.foldl(Option.empty[(Coordinate, NonEmptyVector[Entity])]) { (r, coordinate) =>
      r.orElse(check(coordinate).map(coordinate -> _))
    }
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
          pathEndCoordinate <- F.pure(nextCoordinate(coordinate, entity.velocity))
          _ <- logger.trace(s"Building path for ${entity.show} between ${coordinate.show} and ${pathEndCoordinate.show}")
          path <- F.pure(path(coordinate, pathEndCoordinate))
          _ <- logger.trace(s"Checking path ${path.mkString_("[",",","]")} collisions")
          collisions <- pathCollisionReverseLookup(path, checkCoordinate).cata(F.pure, F.raiseError(EngineError("No valid landing coordinate was found")))
          _ <- logger.trace(s"Found landing coordinate for ${entity.show} at ${collisions._1.show}")
          newVelocity <- F.pure(nextVelocity(entity.velocity, entity.inertiaFactor))
          _ <- logger.trace(s"Velocity changes for ${entity.show} from ${entity.velocity.show} to ${newVelocity.show} ")
        } yield NonEmptyVector.one(
          MovementTransition(
            entityTransition = EntityTransition(entity, entity.updateVelocity(newVelocity)),
            from = coordinate,
            to = collisions._1
          )
        )
      }
    }
}
