package com.stremlenye.robotwars.rendering

import com.sksamuel.scrimage._
import com.sksamuel.scrimage.canvas._
import com.stremlenye.robotwars.{Coordinate, Frame, Size}
import simulacrum._

@typeclass trait RenderData[A] {
  @op("color") def color(a : A) : Color
  @op("size") def size(a : A) : Size

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
    val drawables = frame.world.surface.map {
      case (coordinate, entities) => entities.head.drawable(coordinate, scaleFactor)
    }

    val imageSize = scale(frame.world.size, scaleFactor)

    println(s"${Console.YELLOW}>>>> imageSize = ${imageSize}${Console.RESET}")

    val canvas = Canvas(Image(imageSize.width, imageSize.height))

    canvas.draw(drawables.toSeq).image
  }

  private def scale(size : Size, factor : Int) : Size =
    Size(size.width * factor, size.height * factor)
}
