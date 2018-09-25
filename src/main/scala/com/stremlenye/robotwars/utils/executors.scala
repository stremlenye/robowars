package com.stremlenye.robotwars.utils

import java.util.concurrent.ForkJoinPool

import scala.concurrent.ExecutionContext

object executors {

  val parallelismFactor = 100
  val ioContext = ExecutionContext.fromExecutor(new ForkJoinPool(parallelismFactor))
}
