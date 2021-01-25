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
package me.chuwy.otusfp.data

import org.specs2.mutable.Specification

class ContactSpec extends Specification {
  "Contact.fromString" should {
    "be isomorphic to Contact.asString" in {
      val input = Contact("joe@gmail.com", 1983, None)
      val output = Contact.fromString(input.asString)
      output must beRight(input)
    }

    "be able to parse string with multi-word comment" in {
      val output = Contact.fromString("jack@twitter.com 1981 A founder of twitter dot com")
      output must beRight
    }
  }
}
