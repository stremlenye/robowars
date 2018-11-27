package com.stremlenye.robotwars.io

import cats.Monad
import cats.syntax.all._
import com.sksamuel.scrimage.{Image, RGBColor}
import com.stremlenye.robotwars.{Coordinate, Size}
import com.stremlenye.robotwars.config.XTermColor
import com.stremlenye.robotwars.utils.algebras.LoggingAlgebra
import com.stremlenye.robotwars.utils.images

class TerminalOutput[F[_]](terminalSurfaceSize : Size, xtermColorsConfig : Vector[XTermColor], logger : LoggingAlgebra[F])
                          (implicit F : Monad[F]) extends ImageIOAlgebra[F] {

  override def sink(frameNumber : Long, image : Image) : F[Unit] = for {
    _ <- logger.info(s"Output the $frameNumber frame to terminal")
    _ <- F.point(clear())
    _ <- F.point {
      images.iterable(image.scaleTo(terminalSurfaceSize.width, terminalSurfaceSize.height)).foreach {
        case (x, y, p) => renderPixel(Coordinate(y, x), findClosestColorID(xtermColorsConfig)(p.toColor), "█")
      }
    }
    _ <- F.point(Console.flush())
  } yield ()

  def clear() : Unit = {
    print("\u001b[2J")
  }

  private def renderPixel(point : Coordinate, colorId : Int, text : String) : Unit = {
    setCursor(point)
    put(colorId, text)
  }

  private def setCursor(point : Coordinate) : Unit =
    print(s"\u001b[${point.x};${point.y}H")

  private def put(colorId : Int, text : String) : Unit =
    print(s"\u001b[38;5;${colorId}m${text}\u001b[0m")

  private def findClosestColorID(xtermColorsConfig : Vector[XTermColor])(color : RGBColor) : Int =
    xtermColorsConfig.minBy {
      case XTermColor(_, rgb) => cartesianDistance(rgb, color)
    }.colorId

  private def cartesianDistance(l : RGBColor, r : RGBColor) : Int =
    math.abs(l.red - r.red) +
      math.abs(l.green - r.green) +
      math.abs(l.blue - r.blue)
}

