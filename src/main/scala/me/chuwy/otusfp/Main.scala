package me.chuwy.otusfp

import cats.effect.{IOApp, IO, ExitCode}

import me.chuwy.otusfp.algebra.Console

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    Command.parse(args) match {
      case Right(command) => Command.process(command)
      case Left(error) => Console.putStrLn(error).as(ExitCode.Error)
    }
}
