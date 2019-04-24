import java.io.File
import java.nio.file.{Files, Path, Paths}

import sys.process._
import org.scalatest.{AppendedClues, Matchers}

import scala.io.Source

class IntegrationTest extends org.scalatest.FunSuite with Matchers with AppendedClues {
  private val extensions = Array("JVM", "IWE", "CLE", "CGT", "CGE", "CEQ", "CNE", "BDJ")

  private def readFile(filename: String) =
    Source.fromFile(filename).mkString

  private def run(command: String, directory: String): (Int, String, String) = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val logger = ProcessLogger(stdout.append(_), stderr.append(_))
    val status = Process(command, new File(directory)) ! logger
    (status, stdout.toString, stderr.toString)
  }

  private def shouldSkip(program: String, extensions: Seq[String]): Boolean = {
    val ext = """^/ *EXT:(.*)$""".r
    val noExt = """^/ *EXT:!(.*)$""".r

    val unsupportedExt = ext.findAllMatchIn(program).map(_.group(1)).exists(!extensions.contains(_))
    val incompatibleExt = noExt.findAllMatchIn(program).map(_.group(1)).exists(extensions.contains(_))

    unsupportedExt || incompatibleExt
  }

  private def listFiles(dir: String) =
    new File(dir).listFiles

  private def executeTest(program: String, mainClass: String, testFn: (Int, String, String) => Unit): Unit = {
    val outDir = Files.createTempDirectory("compiler-scala").toString
    val result = Compiler.compileToFiles(program, mainClass + ".java", outDir)
    result should matchPattern { case Right(_) => }

    val (_, _, stdErr) = run(s"java -jar jasmin.jar $outDir/$mainClass.jasmin -d $outDir", ".")
    stdErr shouldBe empty

    val (errCode, stdOut, stdErr2) = run(s"java $mainClass", outDir)
    testFn(errCode, stdOut, stdErr2)
  }

  private def forAllFiles(itSubDir: String, testFn: (String, File) => Unit): Unit = {
    val subDirs = listFiles("./src/test/resources/integration-test/" + itSubDir)

    subDirs.filter(_.isDirectory).foreach(subDir => subDir.listFiles((_, f) => f.endsWith(".java"))
      .foreach(sourceFile => {
        val program = readFile(sourceFile.getAbsolutePath)
        if (!shouldSkip(program, extensions)) {
          val testName = itSubDir + "/" + subDir.getName + "/" + sourceFile.getName.stripSuffix(".java")
          test(testName) {
            testFn(program, sourceFile)
          }
        }
    }))
  }

  private def executeAllFiles(itSubDir: String, testFn: (Int, String, String, String) => Unit): Unit = {
    forAllFiles(itSubDir, (program, file) => {
      val base = file.getPath.stripSuffix(".java")
      val mainClass = readFile(base + ".main")

      val outPath = base + ".out"
      val out = if (Files.exists(Paths.get(outPath))) readFile(outPath) else ""
      executeTest(program, mainClass, (errCode, stdErr, stdOut) => testFn(errCode, stdErr, stdOut, out))
    })
  }

  forAllFiles("compile", (program, file) =>
    Compiler.compile(program, file.getName) should matchPattern { case Right(_) => })

  forAllFiles("noncompile", (program, file) =>
    Compiler.compile(program, file.getName) should matchPattern { case Left(_) => })

  executeAllFiles("execute",
    (errCode, stdErr, stdOut, expectedOut) => {
      errCode shouldBe 0 withClue stdErr
      stdOut shouldBe expectedOut
    })

  executeAllFiles("nonexecute",
    (errCode, _, stdOut, _) => {
      errCode shouldBe 1 withClue stdOut
    })
}
