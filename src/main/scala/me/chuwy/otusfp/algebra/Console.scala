package me.chuwy.otusfp.algebra

import cats.effect.IO

object Console {
  /** Ссылочно-прозрачный аналог println */
  def putStrLn(str: String): IO[Unit] =
    IO.delay(println(str))  // FFI
}
