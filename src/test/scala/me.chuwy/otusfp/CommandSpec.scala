/*
 * Copyright (c) 2012-2020 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package me.chuwy.otusfp

import me.chuwy.otusfp.Interpreter._
import me.chuwy.otusfp.algebra.Contacts
import org.specs2.mutable.Specification

class CommandSpec extends Specification {

  "help" should {
    "print a help message" in {
      val result = Command.help[Test].runS(RealWorld.init).value
      result must beEqualTo(RealWorld(List("Use ADD or LIST"), Nil))
    }
  }

  "list and save" should {
    "work together" in {
      val resultS = for {
        first <- Contacts[Test].list
        exitCode <- Command.save[Test]("joe@gmail.com 1988")
        _ <- Command.list[Test]
        second <- Contacts[Test].list
      } yield (first, second)

      val (endState, (first, second)) = resultS.run(RealWorld.init).value
      println(endState)

      first must beEmpty
      second must haveSize(1)

    }
  }
}
