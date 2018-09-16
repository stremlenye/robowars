package com.stremlenye.robotwars

import java.nio.file.Paths
import java.util.UUID

import cats.implicits._
import cats.data.NonEmptyVector
import cats.tagless.implicits._
import com.stremlenye.robotwars.mtl.Transformations._
import com.stremlenye.robotwars.io.ImageIOAlgebra
import com.stremlenye.robotwars.physics.{Actor, Floor, PhysicsEngineAlgebra, Velocity}
import com.stremlenye.robotwars.rendering.RenderEngine
import com.stremlenye.robotwars.utils.algebras.{ExternalProcessAlgebra, SimpleLogging}

object App {
  def main(args : Array[String]) : Unit = {
    val outputPath = Paths.get("./out")

    type ErrorContext[A] = Either[Throwable, A]

    val loggingAlgebra = SimpleLogging[ErrorContext]("App")

    val imageIO = ImageIOAlgebra[ErrorContext](
      outputPath,
      SimpleLogging[ErrorContext]("ImageIO")).mapK(
      trilogging(loggingAlgebra)("Saving frame image", "Saved framed image")
    )

    val peLogger = SimpleLogging[ErrorContext]("PhysicsEngine")
    val physicsEngine = PhysicsEngineAlgebra[ErrorContext](peLogger).mapK(errorLogging(peLogger))

    val defaultSettings = GameSettings(
      PlayerStats(0),
      0,
      0
    )

    val length = 24L * 3
    val mapSize = 300

    val initialWorld = World(
      (for {
        x <- 1 to mapSize
        y <- 1 to (mapSize / 2)
      } yield (Coordinate(x, y), NonEmptyVector.one(Floor(UUID.randomUUID())))).toMap
    )

    val out = (for {
      w1 <- initialWorld.putEntity(Coordinate(1, 25), Actor(UUID.randomUUID(), Robot.dummy, Velocity(10, 0)))
      w2 <- w1.putEntity(Coordinate(1, 50), Actor(UUID.randomUUID(), Robot.dummy, Velocity(20, 0)))
      w3 <- w2.putEntity(Coordinate(1, 75), Actor(UUID.randomUUID(), Robot.dummy, Velocity(30, 0)))
    } yield w3).map { world =>
      GameSetup(length, world, defaultSettings, Seq.empty)
    }.map(Game.run(_, physicsEngine)).map(_.map { frame =>
      val image = RenderEngine.renderFrame(frame)
      imageIO.sink(frame.index, image)
    }.toVector)

    ExternalProcessAlgebra[ErrorContext](
      "ffmpeg -y -framerate 12 -i ./out/%05d.png ./out/output.mp4",
      loggingAlgebra
    ).mapK(errorLogging(loggingAlgebra)).run

    println(out)
  }
}
