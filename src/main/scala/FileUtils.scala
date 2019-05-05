import java.nio.file.{Files, Paths}

import cats.effect.internals.IOAppPlatform
import cats.effect.{ContextShift, IO, Timer}
import fs2.{Pure, Stream, io}
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.language.reflectiveCalls
import scala.io.Source

object FileUtils {
  /**
    * Provides an implicit [[Timer]] for the app.
    *
    * Users can override this value in order to customize the
    * underlying scheduler being used.
    *
    * The default on top of the JVM uses an internal scheduler built with Java's
    * [[https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/Executors.html#newScheduledThreadPool-int- Executors.newScheduledThreadPool]]
    * (configured with one or two threads) and that defers the execution of the
    * scheduled ticks (the bind continuations get shifted) to Scala's `global`.
    *
    * On top of JavaScript the default timer will simply use the standard
    * [[https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/setTimeout setTimeout]].
    */

  case class FileOutput(filename: String, content: Stream[Pure, Byte])

  def readFile(filename: String) =
    using(Source.fromFile(filename)) { _.mkString }

  def writeFile(filename: String, bytes: Array[Byte]) =
    Files.write(Paths.get(filename), bytes)

  protected implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  def writeFile(filename: String, bytes: Stream[Pure, Byte]): Unit =
    bytes.through(io.file.writeAll[IO](Paths.get(filename), ExecutionContext.global)).compile.drain.unsafeRunSync()

  private def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }
}
