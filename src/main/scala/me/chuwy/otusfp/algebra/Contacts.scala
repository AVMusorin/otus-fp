package me.chuwy.otusfp.algebra

import java.nio.file.{Path, Paths}

import io.circe.syntax._

import cats.effect.{IO, Blocker, Resource}

import fs2.Stream
import fs2.io.file.{writeAll, createDirectory, readAll, directoryStream, delete => deleteFile}

import me.chuwy.otusfp.data.Contact
import me.chuwy.otusfp.data.Contact.Id

trait Contacts[F[_]] {
  def list: F[List[(Contact.Id, Contact)]]
  def add(person: Contact): F[Unit]
  def delete(id: Id): F[Unit]
}

object Contacts {

  def apply[F[_]](implicit ev: Contacts[F]): Contacts[F] = ev

  // (игнорировать)
  private implicit val CS = IO.contextShift(concurrent.ExecutionContext.global)

  /** Убедиться что директория существует, вернуть полный путь и блокирующий тредпул (игнорировать) */
  private val blockerInDir: Resource[IO, (Path, Blocker)] =
    for {
      blocker <- Blocker[IO]
      curDir  <- Resource.liftF(IO(Paths.get("").toAbsolutePath))
      dbPath   = Path.of(curDir.toString, "fakedb")
      path    <- Resource.liftF(createDirectory[IO](blocker, dbPath).handleErrorWith(_ => IO.pure(dbPath)))
    } yield (path, blocker)

  // Реализации (детали неважны)

  val ioInterprter = new Contacts[IO] {
    def list: IO[List[(Contact.Id, Contact)]] =
      Contacts.list

    def add(person: Contact): IO[Unit] =
      Contacts.add(person)

    def delete(id: Id): IO[Unit] =
      Contacts.delete(id)
  }

  def list: IO[List[(Contact.Id, Contact)]] =
    blockerInDir.use { case (fileDb, b) =>
      directoryStream[IO](b, fileDb, (p: Path) => p.toString.endsWith(".json"))
        .evalMap { path => readAll[IO](path, b, 4096).compile.to(Array).map(arr => (path, new String(arr))) }
        .map { case (path, content) => (path, Contact.fromJsonString(content).getOrElse(throw new RuntimeException(s"Invalid file content $content"))) }
        .compile
        .toList
    }

  def add(person: Contact): IO[Unit] =
    blockerInDir.use { case (fileDb, b) =>
      for {
        rnd <- IO(scala.util.Random.between(10, 100))
        path = Path.of(fileDb.toString, s"${person.email}-$rnd.json")
        _ <- Stream.emits(person.asJson.noSpaces.getBytes).through(writeAll[IO](path, b)).compile.drain
      } yield ()
    }

  def delete(id: Id): IO[Unit] =
    blockerInDir.use { case (_, b) => deleteFile[IO](b, id) }
}