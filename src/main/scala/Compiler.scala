import CodeGenerator.JasminAssembly

object Compiler {
  def compile(s: String, sourceFile: String): Either[CompilerError, Seq[JasminAssembly]] =
    for {
      syntaxTree <- Parser.parse(s)
      symTable <- SymbolTableCreator.create(syntaxTree)
      _ <- TypeChecker.typeCheck(syntaxTree, symTable)
      jasminAssembly = CodeGenerator.generate(syntaxTree, symTable, sourceFile)
    } yield jasminAssembly
}
