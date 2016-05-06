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

import java.io.{ByteArrayOutputStream, Closeable, InputStream}

import scala.language.implicitConversions

package object frac {
  implicit def toRichInt        (enriched: Int        ): RichInt          = new RichInt(enriched)
  implicit def toRichInputStream(enriched: InputStream): RichInputStream  = new RichInputStream(enriched)

  def using[A <: Closeable](closeable: A)(block: A => Unit): Unit =
    try {
      block(closeable)
    }
    finally {
      if (closeable != null) closeable.close()
    }
}

class RichInt(enriched: Int) {
  def toRad: Double = math.Pi * 2 * enriched / 360
}

class RichInputStream(enriched: InputStream) {
  def toByteArray: Array[Byte] = {
    val output = new ByteArrayOutputStream
    val readBuffer = new Array[Byte](16384)
    var read = enriched.read(readBuffer, 0, readBuffer.length)

    while (read != -1) {
      output.write(readBuffer, 0, read)
      read = enriched.read(readBuffer, 0, readBuffer.length)
    }

    output.flush()
    output.toByteArray
  }
}