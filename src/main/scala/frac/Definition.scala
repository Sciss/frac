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

import java.awt.Color

/** Represents the definition of a fractal curve */
trait Definition {
  def turnAngle: Double
  def scaleRatio: Double
  def startingPoint: StartingPoint.Value
  def run(depth: Int, callback: Symbol => Unit)
}

object StartingPoint extends Enumeration {
  val Left, Bottom = Value
}