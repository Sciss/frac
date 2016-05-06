// modified by Hanns Holger Rutz in May 2016

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

import scala.math._

class FracDefParserSpec extends Specification {
  val sut = new FracDefParser

  "The rule parser" should {
    "parse empty string rule" in {
      val src = "seed = "

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          Nil,
          src) )
    }
    "parse angle assignment" in {
      val src = "angle = 60\nseed = "

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          Nil,
          src,
          turnAngle = Pi / 3 ) )
    }
    "parse scale ratio assignment" in {
      val src = "ratio = 0.2\nseed = "

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          Nil,
          src,
          scaleRatio = 0.2 ) )
    }
    "parse start point assignment" in {
      val src = "start = bottom\nseed = "

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          Nil,
          src,
          startingPoint = StartingPoint.Bottom ) )
    }
    "parse title assignment" in {
      val src = "title = Koch Flake\nseed = "

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          Nil,
          src,
          title = "Koch Flake" ) )
    }
    "parse single primitive rule" in {
      val src = "seed = F"

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          List(RuleReference('F')),
          src) )
    }
    "parse repeated primitive rule" in {
      val src = "seed = 24F"

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          List(RuleReference('F', 24)),
          src) )
    }
    "parse multiple primitives rule" in {
      val src = "seed = F+A"

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          List(RuleReference('F'), RuleReference('+'), RuleReference('A')),
          src) )
    }
    "parse a color statement" in {
      val src = "seed = {12,250,0}"

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          List(ConstantColorOperation(12, 250, 0)),
          src) )
    }
    "parse a predeined color statement" in {
      val src = "seed = {red}"

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          List(ConstantColorOperation("red")),
          src) )
    }
    "parse a color increment statement" in {
      val src = "seed = {+10,+0,-10}"

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          List(IncrementColorOperation(10, 0, -10)),
          src) )
    }
    "parse multiple rules" in {
      val src = "seed = F\nF = A\nA = +"

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          List(RuleReference('F')),
          src,
          rules = List(
            Rule('F', List(RuleReference('A'))),
            Rule('A', List(RuleReference('+')))
          )) )
    }
    "parse a complex rule made of all possible elements" in {
      val src = "seed = F{1,2,3}+"

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          List(RuleReference('F'), ConstantColorOperation(1, 2, 3), RuleReference('+')),
          src) )
    }
    "ignore spaces and empty lines" in {
      val src = "angle = \t60\n \t\nseed\t = \n\n"

      sut.parseFracDef(src).result.get must beEqualTo(
        FracDef(
          Nil,
          sourceText = src,
          turnAngle = Pi / 3 ) )
    }
    "parse multiple definitions" in {
      val src = "angle = 60\nseed = \nangle = 30\nseed = "

      sut.parseFracDefList(src).result.get must beEqualTo( List(
        FracDef(
          Nil,
          sourceText = "angle = 60\nseed = \n",
          turnAngle = Pi / 3 ),
        FracDef(
          Nil,
          sourceText = "angle = 30\nseed = ",
          turnAngle = Pi / 6 )
      ) )
    }
  }
}
