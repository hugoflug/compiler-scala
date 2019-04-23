import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import CodeGenerator.JasminAssembly

import scala.io.Source

object Compiler {
  def main(args: Array[String]): Unit =
    if (args.isEmpty) println("Usage: mjc <file> [<file> ...]")
    else args.foreach(compileFromFile(_, "."))

  def compileFromFile(filename: String, outDir: String): Unit =
    compileWithErrorMsgs(Source.fromFile(filename).mkString, filename, outDir)

  def compileWithErrorMsgs(s: String, sourceFile: String, outDir: String): Unit =
    compileToFiles(s, sourceFile, outDir) match {
      case Left(error) => println(ErrorFormatter.format(error))
      case Right(_) =>
    }

  private def writeToFile(filename: String, s: String) =
    Files.write(Paths.get(filename), s.getBytes(StandardCharsets.UTF_8))

  def compileToFiles(s: String, sourceFile: String, outDir: String): Either[CompilationError, Unit] =
    compile(s, sourceFile) match {
      case Left(error) => Left(error)
      case Right(assemblies) =>
        assemblies.foreach(a => writeToFile(outDir + "/" + a.filename, a.program))
        Right()
    }

  def compile(s: String, sourceFile: String): Either[CompilationError, Seq[JasminAssembly]] =
    for {
      syntaxTree <- Parser.parse(s)
      symTable <- SymbolTableCreator.create(syntaxTree)
      _ <- TypeChecker.typeCheck(syntaxTree, symTable)
      jasminAssembly = CodeGenerator.generate(syntaxTree, symTable, sourceFile)
    } yield jasminAssembly
}
