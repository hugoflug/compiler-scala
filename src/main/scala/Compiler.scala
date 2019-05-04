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
    compileToFiles(program, outDir) match {
      case Left(error) => println(ErrorFormatter.format(error, program, sourceFile))
      case Right(_) =>
    }

  def compileToFiles(program: String, outDir: String): Either[CompilationError, Seq[FileOutput]] =
    compile(program) match {
      case Left(error) => Left(error)
      case Right(classes) =>
        classes.foreach(a => FileUtils.writeFile(outDir + "/" + a.filename, a.content))
        Right(classes)
    }

  def compile(program: String): Either[CompilationError, Seq[FileOutput]] =
    for {
      syntaxTree <- Parser.parse(program)
      symTable <- SymbolTableCreator.create(syntaxTree)
      _ <- TypeChecker.typeCheck(syntaxTree, symTable)
      jvmClasses = CodeGenerator.generate(syntaxTree, symTable)
      classFiles = jvmClasses.map(Assembler.assemble)
    } yield classFiles
}
