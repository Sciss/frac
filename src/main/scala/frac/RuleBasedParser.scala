/*
 * Copyright (C) 2011 Julien Letrouit
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

class RuleBasedParser extends DefinitionParser
{
    def parse(text: String) =
    {
        var angle = 90
        var ratio = 0.5
        var seed = "F"
        var rules = Map.empty[Char,String]

        text.lines.foreach { line =>
            if (line.trim.length > 0) {
                val Array(varName, value) = line.split('=').map(_.trim)
                varName match {
                    case "seed" => seed = value
                    case "angle" => angle = value.toInt
                    case "ratio" => ratio = value.toDouble
                    case c => rules = rules.updated(c(0), value)
                }
            }
        }

        new RuleBasedDefinition(seed, rules, angle, ratio)
    }
}