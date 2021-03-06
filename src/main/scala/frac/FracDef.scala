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

import scala.annotation.tailrec

case class FracDef(seed         : List[Symbol],
                   sourceText   : String              = "",
                   turnAngle    : Double              = 90.toRad,
                   scaleRatio   : Double              = 0.5,
                   title        : String              = "",
                   startingPoint: StartingPoint.Value = StartingPoint.Left,
                   rules        : List[Rule]          = Nil
                  ) {

  private[this] lazy val ruleIndex: Map[Char, List[Symbol]] =
    rules.map(rule => rule.name -> rule.expression).toMap

  @tailrec
  private[this] def executeRecurse(run: RunState, callback: Symbol => Unit, nextTokens: List[(Int, Symbol)]): Unit =
    if (run.isRunning) nextTokens match {
      case Nil => ()
      case (level, symbol) :: xs =>
        if (level == 0 || !symbol.isInstanceOf[RuleReference] || ruleFor(symbol).isEmpty) {
          callback(symbol)
          executeRecurse(run, callback, xs)
        }
        else {
          executeRecurse(run, callback, ruleFor(symbol).map((level - 1, _)) ::: xs)
        }
    }

  private[this] def ruleFor(symbol: Symbol): List[Symbol] = symbol match {
    case RuleReference(ruleName, _) => ruleIndex.getOrElse(ruleName, Nil)
    case _ => Nil
  }

  def execute(run: RunState, depth: Int, callback: Symbol => Unit): Unit =
    executeRecurse(run, callback, seed.map((depth, _)))
}

object StartingPoint extends Enumeration {
  val Left, Bottom = Value
}

case class Rule(name: Char, expression: List[Symbol])

/** Represents one symbol in a rule */
trait Symbol

object RuleReference {
  def apply(repetition: Option[Int], ruleName: Char): RuleReference = RuleReference(ruleName, repetition.getOrElse(1))
}

/** A primitive token. Primitives are token that have a rule to be executed */
case class RuleReference(ruleName: Char, repetition: Int = 1) extends Symbol {
  override lazy val toString = new String(Array(ruleName))
}