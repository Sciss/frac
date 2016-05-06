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

/** Renders the given definition in a simple string */
class StringRenderer extends RunState {
  def isRunning: Boolean = true

  def render(definition: FracDef, depth: Int): String = {
    val res = new StringBuffer()

    def callback(symbol: Symbol): Unit =
      res.append(symbol.toString)

    definition.execute(this, depth, callback)
    res.toString
  }
}