package me.chuwy.otusfp.data

import cats.syntax.either._
import cats.syntax.show._

import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._
import io.circe.parser.parse

/** Основной тип данных приложения */
case class Contact(email: String, yearOfBirth: Int, comment: Option[String]) {
  /** Реверс [[Contact.fromString]] */
  def asString: String = s"$email $yearOfBirth" ++ comment.getOrElse("")
}

/** Вспомогательные функции (можно игнорировать) */
object Contact {

  type Id = java.nio.file.Path

  def id(str: String): Contact.Id =
    java.nio.file.Path.of(str)

  /** Распарсить JSON строку в контакт (используется в БД) */
  def fromJsonString(s: String): Either[String, Contact] =
    parse(s).flatMap(_.as[Contact]).leftMap(_.show)

  /** Распарсить простую строку в контакт (используется в stdin, реверс [[Contact.asString]] ) */
  def fromString(s: String): Either[String, Contact] =
    s.split(" ").toList match {
      case name :: IntString(age) :: comment if comment.nonEmpty =>
        Contact(name, age, Some(comment.mkString(" "))).asRight
      case List(name, IntString(age)) =>
        Contact(name, age, None).asRight
      case _ =>
        s"$s is invalid string for Contact".asLeft
    }

  implicit val personDecoder: Decoder[Contact] =
    deriveDecoder[Contact]
  implicit val personEncoder: Encoder[Contact] =
    deriveEncoder[Contact]

  private object IntString {
    def unapply(s: String): Option[Int] =
      Either.catchOnly[NumberFormatException](s.toInt).toOption
  }
}