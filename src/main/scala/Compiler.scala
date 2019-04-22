object Compiler {
  def compile(s: String): Either[CompilerError, String] =
    for {
      syntaxTree <- Parser.parse(s)
      symTable <- SymbolTableCreator.create(syntaxTree)
      _ <- TypeChecker.typeCheck(syntaxTree, symTable)
      jasminAssembly = CodeGenerator.generate(syntaxTree, symTable)
    } yield jasminAssembly
}
