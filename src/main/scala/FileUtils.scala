import java.nio.file.{Files, Paths}

import scala.language.reflectiveCalls
import scala.io.Source

object FileUtils {
  def readFile(filename: String) =
    using(Source.fromFile(filename)) { _.mkString }

  def writeFile(filename: String, bytes: Array[Byte]) =
    Files.write(Paths.get(filename), bytes)

  private def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }
}
