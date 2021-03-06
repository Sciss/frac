/*
 * Copyright (C) 2012 Julien Letrouit
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package frac

import org.specs2.mutable._

class FractalDefinitionSpec extends Specification {
  val renderer = new StringRenderer

  "a rules and seed definition" should {
    "output seed when given no rules" in {
      val sut = FracDef(
        List(RuleReference('A'), RuleReference('B'), RuleReference('C'))
      )

      renderer.render(sut, 2) must beEqualTo("ABC")
    }
    "apply rules" in {
      val sut = FracDef(
        List(RuleReference('A'), RuleReference('B')),
        rules = List(
          Rule('A', List(RuleReference('C'))),
          Rule('B', List(RuleReference('D')))
        )
      )

      renderer.render(sut, 2) must beEqualTo("CD")
    }
    "recurse once when depth is one" in {
      val sut = FracDef(
        List(RuleReference('A'), RuleReference('B')),
        rules = List(
          Rule('A', List(RuleReference('A'), RuleReference('A')))
        )
      )

      renderer.render(sut, 1) must beEqualTo("AAB")
    }
    "don't recurse when depth 0" in {
      val sut = FracDef(
        List(RuleReference('A'), RuleReference('B')),
        rules = List(
          Rule('A', List(RuleReference('A'), RuleReference('A')))
        )
      )

      renderer.render(sut, 0) must beEqualTo("AB")
    }
    "recurse 'depth' time" in {
      val sut = FracDef(
        List(RuleReference('A'), RuleReference('B')),
        rules = List(
          Rule('A', List(RuleReference('A'), RuleReference('A')))
        )
      )

      renderer.render(sut, 3) must beEqualTo("AAAAAAAAB")
    }
  }
}