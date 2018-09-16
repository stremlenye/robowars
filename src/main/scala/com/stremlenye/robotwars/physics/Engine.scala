package com.stremlenye.robotwars.physics

import cats.data._
import com.stremlenye.robotwars.Coordinate

final case class EntityTransition(from: Entity, to : Entity)

object EntityTransition {
  def noop(entity: Entity) : EntityTransition = EntityTransition(from = entity, to = entity)
}

final case class MovementTransition(entityTransition: EntityTransition, from : Coordinate, to : Coordinate)

object Engine {
  def compute(entity : Entity, coordinate : Coordinate,
              checkCoordinate : Coordinate => Option[NonEmptyVector[Entity]]) : Option[NonEmptyVector[MovementTransition]] = {
    val newCoordinate = nextCoordinate(coordinate, entity.velocity)
    val reactors = checkCoordinate(newCoordinate)
    reactors.map(_ => NonEmptyVector.one(
      MovementTransition(
        entityTransition = EntityTransition.noop(entity),
        from = coordinate,
        to = newCoordinate
      )
    ))
  }

  private def nextCoordinate(coordinate: Coordinate, velocity: Velocity) : Coordinate = {
    Coordinate(
      (coordinate.x + velocity.x).ceil.toInt,
      (coordinate.y + velocity.y).ceil.toInt
    )
  }
}
