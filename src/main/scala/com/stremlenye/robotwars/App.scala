package com.stremlenye.robotwars

import java.nio.file.Files
import java.util.UUID

import cats.Monad
import cats.implicits._
import cats.data._
import cats.effect.internals.IOContextShift
import cats.tagless.implicits._
import mouse.option._
import cats.effect._
import com.stremlenye.robotwars.mtl.Transformations._
import com.stremlenye.robotwars.io.ImageIOAlgebra
import com.stremlenye.robotwars.physics.{Actor, Floor, PhysicsEngineAlgebra, Velocity}
import com.stremlenye.robotwars.rendering.{RenderAlgebra, ScaleFactor}
import com.stremlenye.robotwars.utils.{Benchmark, Ffmpeg}
import com.stremlenye.robotwars.utils.algebras.{ExternalProcessAlgebra, SimpleLogging}
import com.stremlenye.robotwars.utils.executors._

object App {
  def main(args : Array[String]) : Unit = {
    implicit val cs = IOContextShift(ioContext)
    val imagesOutputPath = Files.createTempDirectory("images").toAbsolutePath
    val videoOutputPath = Files.createTempFile("output", ".mp4").toAbsolutePath

    type ErrorContext[A] = Either[Throwable, A]

    val F = Monad[ErrorContext]

    val loggingAlgebra = SimpleLogging[ErrorContext]("App")

    val renderer = RenderAlgebra[ErrorContext](
      ScaleFactor(20),
      logger = SimpleLogging[ErrorContext]("algebra.RenderAlgebra")
    )
    val imageIO = ImageIOAlgebra[ErrorContext](
      imagesOutputPath,
      SimpleLogging[ErrorContext]("algebra.ImageIO")).mapK(
      trilogging(loggingAlgebra)("Saving frame image", "Saved framed image")
    )

    def frameSink(frame : Frame) : ErrorContext[Unit] =
      for {
        image <- renderer.render(frame)
        _ <- imageIO.sink(frame.index, image)
      } yield ()

    val physicsEngine : PhysicsEngineAlgebra[ErrorContext] = {
      val peLogger = SimpleLogging[ErrorContext]("algebra.PhysicsEngine")
      PhysicsEngineAlgebra[ErrorContext](peLogger).mapK(errorLogging(peLogger))
    }

    val videoGenerator = ExternalProcessAlgebra[ErrorContext](
      Ffmpeg.composeCommand(24, imagesOutputPath, videoOutputPath),
      loggingAlgebra
    ).mapK(errorLogging(loggingAlgebra))

    val defaultSettings = GameSettings(
      PlayerStats(0),
      0,
      0
    )

    val length = 24L * 5
    val mapSize = 150

    Benchmark.withTimer("app_async") {
      for {
        world <- generateWorld(mapSize, mapSize / 2)
        gameSetup <- F.pure(GameSetup(length, world, defaultSettings, Seq.empty))
        _ <- Either.catchNonFatal {
          Game.run(gameSetup, physicsEngine)
            .mapAsyncUnordered(parallelismFactor) { frame =>
              IO.delay(frameSink(frame)).flatMap(IO.fromEither)
            }.compile.drain.unsafeRunSync()
        }
        _ <- videoGenerator.run
        _ <- loggingAlgebra.info(videoOutputPath.toString)
      } yield ()
    }

    ()
  }

  private def generateWorld(width : Int, height : Int) : Either[Throwable, World] = {
    val initialWorld = World(
      (for {
        x <- 0 to width
        y <- 0 to height
      } yield (Coordinate(x, y), NonEmptyVector.one(Floor(UUID.randomUUID())))).toMap
    )
    (for {
      w1 <- initialWorld.putEntity(Coordinate(1, 20), Actor(UUID.randomUUID(), Robot.dummy, Velocity(1, 1)))
      w2 <- w1.putEntity(Coordinate(1, 45), Actor(UUID.randomUUID(), Robot.dummy, Velocity(2, 0)))
      w3 <- w2.putEntity(Coordinate(1, 65), Actor(UUID.randomUUID(), Robot.dummy, Velocity(3, 0)))
    } yield w3).right(new Exception("Failed to build the world"))
  }
}
