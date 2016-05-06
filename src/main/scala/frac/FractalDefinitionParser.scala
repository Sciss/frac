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

import org.parboiled.Context
import org.parboiled.scala._

class FractalDefinitionParser extends Parser {
  /** Small methods that can be passed around and that can build part of a definition */
  private[this] type DefTrans = FractalDefinition => FractalDefinition
  private[this] def setAngle(angle: Int   )(fd: FractalDefinition) = fd.copy(turnAngle  = angle.toRad)
  private[this] def setRatio(ratio: Double)(fd: FractalDefinition) = fd.copy(scaleRatio = ratio)
  private[this] def setTitle(title: String)(fd: FractalDefinition) = fd.copy(title      = title)
  private[this] def setStart(startingPoint: StartingPoint.Value)(fd: FractalDefinition) = fd.copy(startingPoint = startingPoint)

  /** Builds the fractal definition out of the given parsed elements */
  private[this] def buildDefinition(transformers: List[DefTrans],
                              seed: List[Symbol],
                              rules: List[frac.Rule],
                              context: Context[_]): FractalDefinition = {
    val source = context.getInputBuffer.extract(context.getMatchRange)
    var definition = frac.FractalDefinition(seed, source, rules = rules)
    // Apply transformers
    transformers.foreach(transform => definition = transform(definition))
    definition
  }

  private[this] val build =
    withContext(buildDefinition(_: List[DefTrans], _: List[Symbol], _: List[frac.Rule], _: Context[_]))

  def FractalDefinitionList   = rule { zeroOrMore( FractalDefinition ) }
  def SingleFractalDefinition = rule { FractalDefinition ~ EOI }
  def FractalDefinition       = rule { FractalDefinitionRule ~~> build }
  def FractalDefinitionRule   = rule { zeroOrMore(ConstantAssignment) ~ SeedAssignment ~ zeroOrMore(Rule) }
  def ConstantAssignment      = rule { TitleAssignment | AngleAssignment | RatioAssignment | StartAssignment }

  def TitleAssignment: Rule1[DefTrans] = rule { "title" ~ optional(WhiteSpaces) ~ "=" ~
                                                          optional(WhiteSpaces) ~ Title ~ Ignored ~~> setTitle _ }
  def AngleAssignment: Rule1[DefTrans] = rule { "angle" ~ optional(WhiteSpaces) ~ "=" ~
                                                          optional(WhiteSpaces) ~ PositiveInteger ~ Ignored ~~> setAngle _ }
  def RatioAssignment: Rule1[DefTrans] = rule { "ratio" ~ optional(WhiteSpaces) ~ "=" ~
                                                          optional(WhiteSpaces) ~ RealNumber ~ Ignored ~~> setRatio _ }
  def StartAssignment: Rule1[DefTrans] = rule { "start" ~ optional(WhiteSpaces) ~ "=" ~
                                                          optional(WhiteSpaces) ~ ( LeftStartingPoint | BottomStartingPoint ) ~
                                                          Ignored ~~> setStart _ }
  def SeedAssignment = rule { "seed" ~ optional(WhiteSpaces) ~ "=" ~ optional(WhiteSpaces) ~ Symbols ~ Ignored }
  def Rule = rule { RuleName ~ optional(WhiteSpaces) ~ "=" ~ optional(WhiteSpaces) ~ Symbols ~ Ignored ~~> frac.Rule }

  def Symbols = rule { zeroOrMore(Symbol) }
  def Symbol  = rule { ColorOperation | RuleReference }
  def RuleReference: Rule1[Symbol] = rule { optional(RepeatCount) ~ RuleName ~~> (frac.RuleReference(_, _)) }
  def RuleName = rule { noneOf("{ \t\r\n\f") ~:> (c => c) }
  def ColorOperation = rule { "{" ~ ( ConstantColorOperation | PredefinedColorOperation | IncrementColorOperation )  ~ "}" }

  def ConstantColorOperation  : Rule1[Symbol] = rule { PositiveInteger ~ "," ~ PositiveInteger ~ "," ~
                                                       PositiveInteger ~~> (frac.ConstantColorOperation(_, _, _)) }
  def PredefinedColorOperation: Rule1[Symbol] = rule { Letters ~> frac.ConstantColorOperation.apply }
  def IncrementColorOperation : Rule1[Symbol] = rule { Increment ~ "," ~ Increment ~ "," ~ Increment ~~> frac.IncrementColorOperation }

  def LeftStartingPoint   = rule { "left"   ~ push(StartingPoint.Left  ) }
  def BottomStartingPoint = rule { "bottom" ~ push(StartingPoint.Bottom) }

  def Title           = rule { oneOrMore( noneOf( "\r\n" ) ) ~> (_.toString) }
  def RealNumber      = rule { oneOrMore(Digit) ~ optional( "." ~ oneOrMore(Digit) ) ~> (_.toDouble) }
  def PositiveInteger = rule { oneOrMore(Digit) ~> (_.toInt) }
  def Increment       = rule { Sign ~ PositiveInteger ~~> ( (_: Int) * (_: Int) ) }
  def Sign            = rule { ( "+" ~ push(1) ) | ( "-" ~ push(-1) ) }
  def RepeatCount     = rule { oneOrMore(Digit) ~> ( s => if (s.isEmpty) 1 else s.toInt ) }
  def Digit           = rule { "0" - "9" }
  def Letters         = rule { oneOrMore(Letter) }
  def Letter          = rule { "a" - "z" | "A" - "Z" }
  def EOL             = rule { "\r\n" | "\n" | "\n" }
  def WhiteSpaces     = rule { oneOrMore(anyOf(" \t\f")) }
  def BlankLine       = rule { optional(WhiteSpaces) ~ EOL }
  def Ignored         = rule { zeroOrMore(BlankLine) }

  def parseFractalDefinition    (input: String) = ReportingParseRunner(SingleFractalDefinition).run(input)
  def parseFractalDefinitionList(input: String) = ReportingParseRunner(FractalDefinitionList  ).run(input)
}
