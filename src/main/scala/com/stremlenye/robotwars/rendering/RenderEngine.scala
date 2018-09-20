package com.stremlenye.robotwars.rendering

import cats._
import cats.implicits._
import cats.tagless.{autoFunctorK, finalAlg}
import com.sksamuel.scrimage._
import com.sksamuel.scrimage.canvas._
import com.stremlenye.robotwars.physics.Entity
import com.stremlenye.robotwars.utils.Benchmark
import com.stremlenye.robotwars.utils.algebras.LoggingAlgebra
import com.stremlenye.robotwars.{Coordinate, Frame, Size}
import simulacrum._

@typeclass trait RenderData[A] {
  @op("color") def color(a : A) : Color
  @op("size") def size(a : A) : Size
  @op("renderPriority") def renderPriority(a : A) : Int
}

@finalAlg
@autoFunctorK
trait RenderAlgebra[F[_]] {
  def render(frame : Frame) : F[Image]
}

object RenderAlgebra {
  private[rendering] def scale(size : Size, factor : Int, offset : Int) : Size =
    Size(size.width * factor + offset, size.height * factor + offset )

  private[rendering] def toDrawable[A : RenderData](a : A, coordinate: Coordinate, scaleFactor : Int) : Drawable = {
    import RenderData.ops._
    FilledRect(coordinate.x * scaleFactor,
      coordinate.y * scaleFactor,
      a.size.height * scaleFactor,
      a.size.width * scaleFactor,
      Context.Default.copy(color = a.color)
    )
  }

  private[rendering] def applyOffset(coordinate: Coordinate, offset : Int) : Coordinate =
    coordinate |+| Coordinate(offset / 2, offset / 2)

  def apply[F[_]](scaleFactor : Int,
                  logger : LoggingAlgebra[F],
                  offset : Int = 0)(implicit F : Monad[F]) : RenderAlgebra[F] = new RenderAlgebra[F] {
    override def render(frame : Frame) : F[Image] = {
      Benchmark.withTimer("RenderAlgebra.render") {
        implicit val renderOrder = Order.by((a : Entity) => a.inertiaFactor)
        val drawables = frame.world.surface.map {
          case (coordinate, entities) => toDrawable(entities.maximum, applyOffset(coordinate, offset), scaleFactor)
        }
        val imageSize = scale(frame.world.size, scaleFactor, offset)
        val canvas = Canvas(Image.filled(imageSize.width, imageSize.height, color = Color.awt2color(java.awt.Color.CYAN)))
        for {
          _ <- logger.trace(s"Rendering frame ${frame.index}")
        } yield canvas.draw(drawables.toSeq).image
      }
    }
  }
}
