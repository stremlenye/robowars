package com.stremlenye.robotwars.utils

import com.stremlenye.robotwars.aliases._
import java.io.{FileNotFoundException, IOException, InputStream}

import io.circe.Decoder
import io.circe.parser.decode

import scala.io.Source

object files {
  private def resourceStream(path: String): FileNotFoundException \/ InputStream =
    Option(getClass.getResourceAsStream(path)).toRight(new FileNotFoundException(path))

  private def tryIo[A](x: => A, f: => Unit = ()): IOException \/ A =
    try Right(x) catch { case ioe: IOException => Left(ioe) } finally f

  def readResourceFile(path: String): IOException \/ String = for {
    stream <- resourceStream(path)
    source <- tryIo(Source.fromInputStream(stream))
    contents <- tryIo(source.mkString, source.close())
  } yield contents

  def readResource[A: Decoder](path: String): Exception \/ A = for {
    json <- files.readResourceFile(path)
    a <- decode[A](json)
  } yield a
}
