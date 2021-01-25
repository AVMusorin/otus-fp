package me.chuwy.otusfp

import cats.data.State

import me.chuwy.otusfp.data.Contact

object Interpreter {

  case class RealWorld(log: List[String], db: List[(Contact.Id, Contact)], counter: Int)

  type Test[A] = State[RealWorld, A]

}
