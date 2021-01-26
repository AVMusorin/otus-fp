package me.chuwy.otusfp.algebra

import cats.effect.IO

trait Console[F[_]] {
  def putStrLn(str: String): F[Unit]
}

object Console {

  def apply[F[_]](implicit ev: Console[F]): Console[F] = ev


  /** Ссылочно-прозрачный аналог println */
  def putStrLn(str: String): IO[Unit] =
    IO.delay(println(str))  // FFI

  val ioInterpeteter: Console[IO] =
    new Console[IO] {
      def putStrLn(str: String): IO[Unit] =
        Console.putStrLn(str)
    }
}
