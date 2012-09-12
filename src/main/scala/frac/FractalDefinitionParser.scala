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

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import frac._
import org.parboiled.Context

class FractalDefinitionParser extends Parser {
  /** Small methods that can be passed around and that can build part of a definition */
  private type DefinitionTansformer = FractalDefinition => FractalDefinition
  private def setAngle(angle: Int)(fd: FractalDefinition) = fd.copy(turnAngle = angle.toRad)
  private def setRatio(ratio: Double)(fd: FractalDefinition) = fd.copy(scaleRatio = ratio)
  private def setTitle(title: String)(fd: FractalDefinition) = fd.copy(title = title)
  private def setStart(startingPoint: StartingPoint.Value)(fd: FractalDefinition) = fd.copy(startingPoint = startingPoint)

  /** Builds the fractal definition out of the given parsed elements */
  private def buildDefinition(transformers: List[DefinitionTansformer],
                              seed: List[Symbol],
                              rules: List[frac.Rule],
                              context: Context) = {
    var definition = frac.FractalDefinition(seed, context.getInputBuffer.extract(context.getMatchRange), rules = rules)
    // Apply transformers
    transformers.foreach(transform => definition = transform(definition))
    definition
  }

  def FractalDefinitionList = rule { zeroOrMore( FractalDefinition ) }
  def FractalDefinition = rule { zeroOrMore(ConstantAssignment) ~ SeedAssignment ~ zeroOrMore(Rule) ~~> (withContext(buildDefinition(_, _, _, _))) }
  def ConstantAssignment = rule { AngleAssignment | RatioAssignment | StartAssignment }
  def TitleAssignment: Rule1[DefinitionTansformer] = rule { "title" ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Title ~ Ignored ~~> (setTitle(_)) }
  def AngleAssignment: Rule1[DefinitionTansformer] = rule { "angle" ~ WhiteSpace ~ "=" ~ WhiteSpace ~ PositiveInteger ~ Ignored ~~> (setAngle(_)) }
  def RatioAssignment: Rule1[DefinitionTansformer] = rule { "ratio" ~ WhiteSpace ~ "=" ~ WhiteSpace ~ RealNumber ~ Ignored ~~> (setRatio(_)) }
  def StartAssignment: Rule1[DefinitionTansformer] = rule { "start" ~ WhiteSpace ~ "=" ~ WhiteSpace ~ ( LeftStartingPoint | BottomStartingPoint ) ~ Ignored ~~> (setStart(_)) }
  def SeedAssignment = rule { "seed" ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Symbols ~ Ignored }
  def Rule = rule { RuleName ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Symbols ~ Ignored ~~> (frac.Rule(_, _)) }
  def RuleName = rule { noneOf("{") ~:> (name => name) }

  def Symbols = rule { zeroOrMore(Symbol) }
  def Symbol = rule { ColorOperation | RuleReference }
  def RuleReference: Rule1[Symbol] = rule { noneOf("{") ~:> (frac.RuleReference(_)) }
  def ColorOperation = rule { "{" ~ ( ConstantColorOperation | PredefinedColorOperation | IncrementColorOperation )  ~ "}" }
  def ConstantColorOperation: Rule1[Symbol] = rule { PositiveInteger ~ "," ~ PositiveInteger ~ "," ~ PositiveInteger ~~> (frac.ConstantColorOperation(_, _, _)) }
  def PredefinedColorOperation: Rule1[Symbol] = rule { Letters ~> (frac.ConstantColorOperation(_)) }
  def IncrementColorOperation: Rule1[Symbol] = rule { Increment ~ "," ~ Increment ~ "," ~ Increment ~~> (frac.IncrementColorOperation(_, _, _)) }

  def LeftStartingPoint = rule { "left" ~ push(StartingPoint.Left) }
  def BottomStartingPoint = rule { "bottom" ~ push(StartingPoint.Bottom) }
  def Title = rule { oneOrMore( noneOf( WhiteSpace ) ) ~> (_ => _) }
  def RealNumber = rule { oneOrMore(Digit) ~ optional( "." ~ oneOrMore(Digit) ) ~> (_.toDouble) }
  def PositiveInteger = rule { oneOrMore(Digit) ~> (_.toInt) }
  def Increment = rule { Sign ~ PositiveInteger ~~> ( (_: Int) * (_: Int) ) }
  def Sign = rule { ( "+" ~ push(1) ) | ( "-" ~ push(-1) ) }
  def Digit = rule { "0" - "9" }
  def Letters = rule { oneOrMore(Letter) }
  def Letter = rule { "a" - "z" | "A" - "Z" }
  def EOL = rule { "\r\n" | "\n" | "\n" | EOI }
  def WhiteSpace = rule { zeroOrMore(anyOf(" \t\f")) }
  def BlankLine = rule { WhiteSpace ~ EOL }
  def Ignored = rule { oneOrMore(BlankLine) }

  def parseFractalDefinition(input: String) = ReportingParseRunner(FractalDefinition).run(input)
  def parseFractalDefinitionList(input: String) = ReportingParseRunner(FractalDefinitionList).run(input)
}
