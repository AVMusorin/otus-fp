package me.chuwy.otusfp

import java.nio.file.Paths

import cats.{Functor, Monad, Applicative}
import cats.implicits._

import cats.effect.{IO, ExitCode}

import me.chuwy.otusfp.data.Contact
import me.chuwy.otusfp.algebra.{Contacts, Console}

/** Все возможные команды, собранные в ADT */
sealed trait Command

object Command {

  case object Help extends Command
  case object List extends Command
  case class Save(args: String) extends Command
  case class Delete(id: Contact.Id) extends Command

  /** Распарсить команду из argv */
  def parse(args: List[String]): Either[String, Command] =
    args match {
      case command :: args =>
        def getId: Contact.Id =
          Paths.get(if (args.isEmpty) "unknown" else args.mkString(" "))

        command.toUpperCase match {
          case "LIST" if args.isEmpty => Right(List)
          case "SAVE" => Right(Save(args.mkString(" ")))
          case "HELP" => Right(Help)
          case "DELETE" => Right(Delete(getId))
          case other => Left(s"$other is unknown command")
        }
      case Nil => Left("No command specified, try HELP")
    }

  def process(command: Command): IO[ExitCode] = {
    implicit val interpetorConsole = Console.ioInterpeteter
    implicit val interpetorContacts = Contacts.ioInterprter

    command match {
      case Help       => help[IO].as(ExitCode.Success)
      case List       => list[IO].as(ExitCode.Success)
      case Save(args) => save(args)
      case Delete(id) => delete(id)
    }
  }

  // Имплементации

  def help[F[_]: Console]: F[Unit] =
    Console[F].putStrLn("Use ADD or LIST")

  def list[F[_]: Console: Contacts: Monad]: F[Unit] =
    Contacts[F].list.flatMap { list =>
      list.traverse_ { case (id, _) =>
        Console[F].putStrLn(id.toString)
      }
    }

  def save[F[_]: Contacts: Applicative](args: String): F[ExitCode] =
    Contact.fromString(args) match {
      case Right(contact) => Contacts[F].add(contact).map(_ => ExitCode.Success)
      case Left(_) => Applicative[F].pure(ExitCode.Error)
    }

  def delete[F[_]: Contacts: Functor](id: Contact.Id): F[ExitCode] =
    Contacts[F].delete(id).as(ExitCode.Success)


  def get(id: Contact.Id): IO[ExitCode] =
    ???
}
