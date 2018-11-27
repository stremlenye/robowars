package com.stremlenye.robotwars.rendering

import cats._
import cats.implicits._
import cats.tagless.FunctorK
import com.sksamuel.scrimage._
import com.sksamuel.scrimage.canvas._
import com.stremlenye.robotwars.mtl.FunctorKDerivation
import com.stremlenye.robotwars.physics.Entity
import com.stremlenye.robotwars.utils.Benchmark
import com.stremlenye.robotwars.utils.algebras.LoggingAlgebra
import com.stremlenye.robotwars.{Coordinate, Frame, Size, World}
import simulacrum._

@typeclass trait RenderData[A] {
  @op("color") def color(a : A) : Color
  @op("size") def size(a : A) : Size
  @op("renderPriority") def renderPriority(a : A) : Int
}

trait RenderAlgebra[F[_]] {
  def render(frame : Frame) : F[Image]
}

case class ScaleFactor(toInt : Int)
case class Offset(toInt : Int)

object Offset {
  val zero = Offset(0)
}

object RenderAlgebra {

  implicit def functorK : FunctorK[RenderAlgebra] = FunctorKDerivation.apply[RenderAlgebra]

  def apply[F[_]](scaleFactor : ScaleFactor,
                  logger : LoggingAlgebra[F])(implicit F : Monad[F]) : RenderAlgebra[F] = new RenderAlgebra[F] {
    override def render(frame : Frame) : F[Image] = {
      import Rendering._
      Benchmark.withTimer(s"RenderAlgebra.render_${frame.index}") {
        val drawables = getDrawables(frame.world, scaleFactor)
        val imageSize = scale(frame.world.size, scaleFactor)
        val canvas = Canvas(Image.filled(imageSize.width, imageSize.height, color = Color.awt2color(java.awt.Color.CYAN)))
        for {
          _ <- logger.trace(s"Rendering frame ${frame.index}")
        } yield canvas.draw(drawables).image
      }
    }
  }
}

object Rendering {
  implicit val renderOrder = Order.by((a : Entity) => a.inertiaFactor)

  def scale(size : Size, factor : ScaleFactor) : Size =
    Size(size.width * factor.toInt, size.height * factor.toInt)

  def scale(coordinate: Coordinate, factor : ScaleFactor) : Coordinate =
    Coordinate(coordinate.x * factor.toInt, coordinate.y * factor.toInt)

  def addOffset(coordinate: Coordinate, offset: Offset) : Coordinate =
    coordinate |+| Coordinate(offset.toInt, offset.toInt)

  def toDrawable[A : RenderData](a : A, coordinate: Coordinate, scaleFactor : ScaleFactor) : Drawable = {
    import RenderData.ops._
    val actualCoordinate = scale(coordinate, scaleFactor)
    val actualSize = scale(a.size, scaleFactor)
    FilledRect(actualCoordinate.x,
      actualCoordinate.y,
      actualSize.height,
      actualSize.width,
      Context.Default.copy(color = a.color)
    )
  }

  def getDrawables(world : World, scaleFactor : ScaleFactor) : Vector[Drawable] = {
    world.surface.map {
      case (coordinate, entities) =>
        toDrawable(entities.maximum, coordinate, scaleFactor)
    }.toVector
  }
}
