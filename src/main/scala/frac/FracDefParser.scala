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

class FracDefParser extends Parser {
  /** Small methods that can be passed around and that can build part of a definition */
  private[this] type DefTrans = FracDef => FracDef
  private[this] def setAngle(angle: Int   )(fd: FracDef) = fd.copy(turnAngle  = angle.toRad)
  private[this] def setRatio(ratio: Double)(fd: FracDef) = fd.copy(scaleRatio = ratio)
  private[this] def setTitle(title: String)(fd: FracDef) = fd.copy(title      = title)
  private[this] def setStart(startingPoint: StartingPoint.Value)(fd: FracDef) = fd.copy(startingPoint = startingPoint)

  /** Builds the fractal definition out of the given parsed elements */
  private[this] def buildDefinition(transformers: List[DefTrans],
                              seed: List[Symbol],
                              rules: List[frac.Rule],
                              context: Context[_]): FracDef = {
    val source = context.getInputBuffer.extract(context.getMatchRange)
    var definition = frac.FracDef(seed, source, rules = rules)
    // Apply transformers
    transformers.foreach(transform => definition = transform(definition))
    definition
  }

  private[this] val build =
    withContext(buildDefinition(_: List[DefTrans], _: List[Symbol], _: List[frac.Rule], _: Context[_]))

  def FracDefList         = rule { zeroOrMore( FracDef ) }
  def SingleFracDef       = rule { FracDef ~ EOI }
  def FracDef             = rule { FracDefRule ~~> build }
  def FracDefRule         = rule { zeroOrMore(ConstantAssignment) ~ SeedAssignment ~ zeroOrMore(Rule) }
  def ConstantAssignment  = rule { TitleAssignment | AngleAssignment | RatioAssignment | StartAssignment }

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
  def Symbol  = rule { ColorOperation | StrokeOperation | RuleReference }
  def RuleReference: Rule1[Symbol] = rule { optional(RepeatCount) ~ RuleName ~~> (frac.RuleReference(_, _)) }
  def RuleName = rule { noneOf("{ \t\r\n\f") ~:> (c => c) }

  // ---- colors ----

  def ColorOperation = rule { "{" ~ ( ConstantColorOperation | PredefinedColorOperation | IncrementColorOperation )  ~ "}" }

  def ConstantColorOperation  : Rule1[Symbol] = rule { PositiveInteger ~ "," ~ PositiveInteger ~ "," ~
    PositiveInteger ~~> (frac.ConstantColorOperation(_, _, _)) }
  def PredefinedColorOperation: Rule1[Symbol] = rule { Letters ~> frac.ConstantColorOperation.apply }
  def IncrementColorOperation : Rule1[Symbol] = rule { Increment ~ "," ~ Increment ~ "," ~ Increment ~~> frac.IncrementColorOperation }

  // ---- strokes ----
  
  def StrokeOperation = rule { "(" ~ ( ConstantStrokeOperation | PredefinedStrokeOperation | IncrementStrokeOperation )  ~ ")" }

  def ConstantStrokeOperation  : Rule1[Symbol] = rule { RealNumber ~~> (frac.ConstantStrokeOperation(_)) }
  def PredefinedStrokeOperation: Rule1[Symbol] = rule { Letters ~> frac.ConstantStrokeOperation.apply }
  def IncrementStrokeOperation : Rule1[Symbol] = rule { Factor ~~> frac.IncrementStrokeOperation }

  // ----
  
  def LeftStartingPoint   = rule { "left"   ~ push(StartingPoint.Left  ) }
  def BottomStartingPoint = rule { "bottom" ~ push(StartingPoint.Bottom) }

  def Title           = rule { oneOrMore( noneOf( "\r\n" ) ) ~> identity[String] }

  def RealNumber = rule {
    Integer ~ optional[Int]("." ~ PositiveInteger) ~~> { (a: Int, bOpt: Option[Int]) =>
      bOpt.fold(a.toDouble)(b => s"$a.$b".toDouble) // I don't think this is the way to do it...
    }}
//  def RealNumber      = rule { Digits ~ optional( "." ~ Digits ) ~> { s =>
//    println(s"GOT '$s'")
//    s.toDouble
//  }}
  def PositiveInteger = rule { Digits ~> (_.toInt) }
  def Integer         = rule {
    optional[Unit]("-" ~> (_ => ())) ~ PositiveInteger ~~> { (neg: Option[Unit], num: Int) =>
      if (neg.isDefined) -num else num
    }
  }
  def Increment       = rule { Sign ~ PositiveInteger ~~> ( (_: Int) * (_: Int) ) }
  def Factor          = rule { "*" ~ RealNumber }
  def Sign            = rule { ( "+" ~ push(1) ) | ( "-" ~ push(-1) ) }
  def RepeatCount     = rule { Digits ~> ( s => if (s.isEmpty) 1 else s.toInt ) }
  def Digit           = rule { "0" - "9" }
  def Digits          = rule { oneOrMore(Digit) }
  def Letters         = rule { oneOrMore(Letter) }
  def Letter          = rule { "a" - "z" | "A" - "Z" }
  def EOL             = rule { "\r\n" | "\n" | "\n" }
  def WhiteSpaces     = rule { oneOrMore(anyOf(" \t\f")) }
  def BlankLine       = rule { optional(WhiteSpaces) ~ EOL }
  def Ignored         = rule { zeroOrMore(BlankLine) }

  def parseFracDef    (input: String): ParsingResult[FracDef]       = ReportingParseRunner(SingleFracDef).run(input)
  def parseFracDefList(input: String): ParsingResult[List[FracDef]] = ReportingParseRunner(FracDefList  ).run(input)
}
