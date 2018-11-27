package com.stremlenye.robotwars

import java.nio.file.Files
import java.util.UUID

import cats.Monad
import cats.implicits._
import cats.data._
import cats.effect.internals.IOContextShift
import cats.tagless.implicits._
import mouse.option._
import com.sksamuel.scrimage.Image
import com.stremlenye.robotwars.config.Config
import com.stremlenye.robotwars.mtl.Transformations._
import com.stremlenye.robotwars.io.{FrameFilesOutput, ImageIOAlgebra, TerminalOutput}
import com.stremlenye.robotwars.physics.{Actor, Floor, PhysicsEngineAlgebra, Velocity}
import com.stremlenye.robotwars.rendering.{RenderAlgebra, ScaleFactor}
import com.stremlenye.robotwars.utils.Benchmark
import com.stremlenye.robotwars.utils.algebras.SimpleLogging
import com.stremlenye.robotwars.utils.executors._

object App {
  def main(args : Array[String]) : Unit = {
    implicit val cs = IOContextShift(ioContext)
    val imagesOutputPath = Files.createTempDirectory("images").toAbsolutePath

    type ErrorContext[A] = Either[Throwable, A]

    val F = Monad[ErrorContext]

    val loggingAlgebra = SimpleLogging[ErrorContext]("App")

    val renderer = RenderAlgebra[ErrorContext](
      ScaleFactor(1),
      logger = SimpleLogging[ErrorContext]("algebra.RenderAlgebra")
    )

    val terminalIO = new TerminalOutput[ErrorContext](Size(158, 42), Config.xtermColors, SimpleLogging[ErrorContext]("algebra.TerminalOutput"))
    val imageFileIO = new FrameFilesOutput[ErrorContext](imagesOutputPath, SimpleLogging[ErrorContext]("algebra.FrameFilesOutput"))

    val combinedIO = new ImageIOAlgebra[ErrorContext] {
      override def sink(frameNumber : Long, image : Image) : ErrorContext[Unit] =
        for {
          _ <- terminalIO.sink(frameNumber, image)
          _ <- imageFileIO.sink(frameNumber, image.scaleTo(158, 42))
        } yield ()
    }

    def frameSink(frame : Frame) : ErrorContext[Unit] =
      for {
        image <- renderer.render(frame)
        _ <- combinedIO.sink(frame.index, image)
      } yield ()

    val physicsEngine : PhysicsEngineAlgebra[ErrorContext] = {
      val peLogger = SimpleLogging[ErrorContext]("algebra.PhysicsEngine")
      PhysicsEngineAlgebra[ErrorContext](peLogger).mapK(errorLogging(peLogger))
    }

    //    val videoGenerator = ExternalProcessAlgebra[ErrorContext](
    //      Ffmpeg.composeCommand(24, imagesOutputPath, videoOutputPath),
    //      loggingAlgebra
    //    )

    val defaultSettings = GameSettings(
      PlayerStats(0),
      0,
      0
    )

    val length = 24L

    Benchmark.withTimer("app_async") {
      for {
        world <- generateWorld(158, 42)
        gameSetup <- F.pure(GameSetup(length, world, defaultSettings, Seq.empty))
        _ <- Either.catchNonFatal {
          Game.run(gameSetup, physicsEngine)
            .map { frame => frameSink(frame)
            }.compile.drain.unsafeRunSync()
        }
        _ <- loggingAlgebra.info(imagesOutputPath.toString)
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
      w1 <- initialWorld.putEntity(Coordinate(10, 10), Actor(UUID.randomUUID(), Robot.dummy, Velocity(1, 1)))
      w2 <- w1.putEntity(Coordinate(20, 20), Actor(UUID.randomUUID(), Robot.dummy, Velocity(1, 0)))
      w3 <- w2.putEntity(Coordinate(30, 30), Actor(UUID.randomUUID(), Robot.dummy, Velocity(0, 1)))
    } yield w3).right(new Exception("Failed to build the world"))
  }
}
