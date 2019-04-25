import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import CodeGenerator.JasminAssembly

import scala.io.Source

object Compiler {
  abstract class CompilationError(val index: Int)

  def main(args: Array[String]): Unit =
    if (args.isEmpty) println("Usage: mjc <file> [<file> ...]")
    else args.foreach(compileFromFile(_, "."))

  def compileFromFile(filename: String, outDir: String): Unit =
    compileWithErrorMsgs(Source.fromFile(filename).mkString, filename, outDir)

  def compileWithErrorMsgs(program: String, sourceFile: String, outDir: String): Unit =
    compileToFiles(program, sourceFile, outDir) match {
      case Left(error) => println(ErrorFormatter.format(error, program, sourceFile))
      case Right(_) =>
    }

  private def writeToFile(filename: String, s: String) =
    Files.write(Paths.get(filename), s.getBytes(StandardCharsets.UTF_8))

  def compileToFiles(program: String, sourceFile: String, outDir: String): Either[CompilationError, Seq[JasminAssembly]] =
    compile(program, sourceFile) match {
      case Left(error) => Left(error)
      case Right(assemblies) =>
        assemblies.foreach(a => writeToFile(outDir + "/" + a.className + ".jasmin", a.assembly))
        Right(assemblies)
    }

  def compile(program: String, sourceFile: String): Either[CompilationError, Seq[JasminAssembly]] =
    for {
      syntaxTree <- Parser.parse(program)
      symTable <- SymbolTableCreator.create(syntaxTree)
      _ <- TypeChecker.typeCheck(syntaxTree, symTable)
      jasminAssembly = CodeGenerator.generate(syntaxTree, symTable, sourceFile)
    } yield jasminAssembly
}
