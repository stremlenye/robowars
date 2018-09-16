package com.stremlenye.robotwars

import java.nio.file.Paths
import java.util.UUID

import cats.data.NonEmptyVector
import com.stremlenye.robotwars.io.ImageIO
import com.stremlenye.robotwars.physics.Floor
import com.stremlenye.robotwars.physics.Actor
import com.stremlenye.robotwars.rendering.RenderEngine

object App {
  def main(args : Array[String]) : Unit = {
    val outputPath = Paths.get("./out")
    println(s"${Console.YELLOW}>>>> outputPath = ${outputPath}${Console.RESET}")
    val defaultSettings = GameSettings(
      PlayerStats(0),
      0,
      0
    )

    val length = 10L
    val mapSize = 100

    val out = World(
      (for {
        x <- 1 to mapSize
        y <- 1 to mapSize
      } yield (Coordinate(x, y), NonEmptyVector.one(Floor(UUID.randomUUID())))).toMap
    ).putEntity(Coordinate(1, 50), Actor(UUID.randomUUID(), Robot.dummy))
      .map { world =>
        Game(length, world, defaultSettings, Seq.empty)
      }.map(Game.run).map(_.map { frame =>
      val image = RenderEngine.renderFrame(frame)
      ImageIO.out(frame.index, image)(outputPath)
    }.toVector)

    println(out)
  }
}
