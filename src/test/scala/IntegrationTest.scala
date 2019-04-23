import java.io.File
import java.nio.file.Files

import sys.process._
import org.scalatest.{AppendedClues, Matchers}

class IntegrationTest extends org.scalatest.FunSuite with Matchers with AppendedClues {

  private def run(command: String, directory: String): (Int, String, String) = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val logger = ProcessLogger(stdout.append(_), stderr.append(_))
    val status = Process(command, new File(directory)) ! logger
    (status, stdout.toString, stderr.toString)
  }

  private def assertOutput(program: String, mainClass: String, output: String) = {
    val outDir = Files.createTempDirectory("compiler-scala").toString
    val result = Compiler.compileToFiles(program, mainClass + ".mj", outDir)
    result shouldBe Right()

    val (_, _, stdErr) = run(s"java -jar jasmin.jar $outDir/$mainClass.jasmin -d $outDir", ".")
    stdErr shouldBe empty

    val (errCode, stdOut, stdErr2) = run(s"java While", outDir)
    errCode shouldBe 0 withClue stdErr2
    stdOut shouldBe output
  }

  test("While") {
    val program = """
      class While {
        public static void main(String[] args) {
          int a;
          a = 5;
          while (a > 0) {
            System.out.println(a);
            a = a-1;
          }
        }
      }"""

    assertOutput(program, "While", "5\n4\n3\n2\n1")
  }
}
