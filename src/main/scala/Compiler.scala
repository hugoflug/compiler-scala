import Assembler.ClassFile
import FileUtils.FileOutput

object Compiler {
  trait CompilationError {
    def index: Int
  }

  def main(args: Array[String]): Unit =
    if (args.isEmpty) println("Usage: mjc <file> [<file> ...]")
    else args.foreach(compileFromFile(_, "."))

  def compileFromFile(filename: String, outDir: String): Unit =
    compileWithErrorMsgs(FileUtils.readFile(filename), filename, outDir)

  def compileWithErrorMsgs(program: String, sourceFile: String, outDir: String): Unit =
    compileToFiles(program, sourceFile, outDir) match {
      case Left(error) => println(ErrorFormatter.format(error, program, sourceFile))
      case Right(_) =>
    }

  def compileToFiles(program: String, sourceFile: String, outDir: String): Either[CompilationError, Seq[FileOutput]] =
    compile(program, sourceFile) match {
      case Left(error) => Left(error)
      case Right(assemblies) =>
        assemblies.foreach(a => FileUtils.writeFile(outDir + "/" + a.filename + ".class", a.content))
        Right(assemblies)
    }

  def compile(program: String, sourceFile: String): Either[CompilationError, Seq[FileOutput]] =
    for {
      syntaxTree <- Parser.parse(program)
      symTable <- SymbolTableCreator.create(syntaxTree)
      _ <- TypeChecker.typeCheck(syntaxTree, symTable)
      jvmClasses = CodeGenerator.generate(syntaxTree, symTable)
      classFiles = jvmClasses.map(Assembler.assemble)
    } yield classFiles
}
