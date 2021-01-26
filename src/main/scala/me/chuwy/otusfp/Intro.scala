package me.chuwy.otusfp

import cats.data.State

import cats.effect.IO

object Intro {
  def add(a: Int, b: Int): IO[Int] = {
    println("Creating IO")  // Так делать не надо!
    IO(println(s"Adding ${a} and ${b}")).map(_ => a + b)
  }

  def add0(a: Int, b: Int): Int = {
    println("Adding")
    a + b
  }

  case class Request(host: String, port: Int, uri: String)
  // IO как HTTP запрос - ты не ждешь что он исполнится после создания
  Request("google.com", 80, "/")

  val result = add(1, 2)
  val finalResult = for {
    _ <- result
    _ <- result
  } yield ()

  def chaining(a: IO[Int]) =
    a.flatMap { aa => IO(println(aa)) }

  object RealWorld
  type HaskellIO[A] = RealWorld => (RealWorld, A)

  case class Counter(i: Int)

  val increment: State[Counter, Unit] =
    State { (counter: Counter) =>
      (counter.copy(i = counter.i + 1), ())
    }

  val incrementThree: State[Counter, Int] = for {
    _ <- increment
    counter <- State.get[Counter]
    _ <- increment
    _ <- increment
  } yield counter.i

  val result2 = incrementThree.run(Counter(0)).value



  case class RealWorld(log: List[String])

  type Test[A] = State[RealWorld, A]

}
