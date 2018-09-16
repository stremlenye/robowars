package com.stremlenye.robotwars.rendering

import cats._
import cats.implicits._
import com.sksamuel.scrimage._
import com.sksamuel.scrimage.canvas._
import com.stremlenye.robotwars.physics.Entity
import com.stremlenye.robotwars.{Coordinate, Frame, Size}
import simulacrum._

@typeclass trait RenderData[A] {
  @op("color") def color(a : A) : Color
  @op("size") def size(a : A) : Size
  @op("renderPriority") def renderPriority(a : A) : Int

  def drawable(a : A)(coordinate: Coordinate, scaleFactor : Int) : Drawable = {
    FilledRect(coordinate.x * scaleFactor,
      coordinate.y * scaleFactor,
      size(a).height * scaleFactor,
      size(a).width * scaleFactor,
      Context.Default.copy(color = color(a))
    )
  }
}

object RenderEngine {

  import RenderData.ops._

  val scaleFactor = 10

  def renderFrame(frame : Frame) : Image = {
    implicit val renderOrder = Order.by((a : Entity) => a.inertiaFactor)
    val drawables = frame.world.surface.map {
      case (coordinate, entities) => entities.maximum.drawable(coordinate, scaleFactor)
    }
    val imageSize = scale(frame.world.size, scaleFactor)
    val canvas = Canvas(Image(imageSize.width, imageSize.height))
    canvas.draw(drawables.toSeq).image
  }

  private def scale(size : Size, factor : Int) : Size =
    Size(size.width * factor, size.height * factor)
}
