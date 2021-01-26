package me.chuwy.otusfp

import cats.data.State

import me.chuwy.otusfp.algebra.{Contacts, Console}
import me.chuwy.otusfp.data.Contact
import me.chuwy.otusfp.data.Contact.Id

object Interpreter {

  case class RealWorld(log: List[String], contacts: List[(Contact.Id, Contact)])

  object RealWorld {
    val init: RealWorld = RealWorld(Nil, Nil)
  }

  type Test[A] = State[RealWorld, A]

  implicit val interpreter: Console[Test] =
    new Console[Test] {
      def putStrLn(str: String): Test[Unit] =
        State { (rw: RealWorld) =>
          (RealWorld(str :: rw.log, Nil), ())
        }
    }

  implicit val interprterContacts: Contacts[Test] =
    new Contacts[Test] {
      def list: Test[List[(Id, Contact)]] =
        State.get[RealWorld].map(_.contacts)

      def add(person: Contact): Test[Unit] =
        State.modify[RealWorld] { rw =>
          val newPerson = (Contact.id("foo"), person)
          val updatedContacts = newPerson :: rw.contacts
          val q = rw.copy(contacts = updatedContacts)
          println(q)
          q
        }

      def delete(id: Id): Test[Unit] =
        State.modify[RealWorld] { rw =>
          rw.copy(contacts = rw.contacts.filter(_._1 != id))
        }
    }
}
