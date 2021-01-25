package me.chuwy.otusfp

import java.nio.file.Paths

import cats.effect.{IO, ExitCode}

import me.chuwy.otusfp.data.Contact

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
    command match {
      case Help       => help.as(ExitCode.Success)
      case List       => list.as(ExitCode.Success)
      case Save(args) => save(args)
      case Delete(id) => delete(id)
    }
  }

  // Имплементации

  def help: IO[Unit] =
    ???

  def list: IO[Unit] =
    ???

  def save(args: String): IO[ExitCode] =
    ???

  def delete(id: Contact.Id): IO[ExitCode] =
    ???

  def get(id: Contact.Id): IO[ExitCode] =
    ???
}
