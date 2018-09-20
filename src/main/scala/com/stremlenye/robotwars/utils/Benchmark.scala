package com.stremlenye.robotwars.utils

import java.util.concurrent.TimeUnit

import com.typesafe.scalalogging.Logger

import scala.concurrent.duration.Duration

object Benchmark {
  def withTimer[A](name: String)(f : => A) : A = {
    val logger = Logger(s"benchmark.timer.$name")
    val t0 = System.nanoTime()
    val result = f
    val t1 = System.nanoTime()
    val duration = Duration(t1 - t0, TimeUnit.NANOSECONDS).toMillis
    logger.info(s": $duration")
    result
  }
}
