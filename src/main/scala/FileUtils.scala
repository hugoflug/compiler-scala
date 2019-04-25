import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.io.Source

object FileUtils {
  def readFile(filename: String) =
    using(Source.fromFile(filename)) { _.mkString }

  def writeFile(filename: String, s: String) =
    Files.write(Paths.get(filename), s.getBytes(StandardCharsets.UTF_8))

  private def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }
}
