object Interprete {

  def ejecutarLinea(
                     linea: String,
                     tags: Map[String, Lambda]
                   ): (Map[String, Lambda], List[String]) = {
    Lexer.tokenize(linea) match {
      case Left(err) =>
        (tags, List(err))
      case Right(tokens) =>
        ejecutarTokens(tokens, tags)
    }
  }
  private def ejecutarTokens(
                      tokens: List[Token],
                      tags: Map[String, Lambda]
                    ): (Map[String, Lambda], List[String]) = {

    tokens match {
      case Reduce :: resto =>
        Metodos.reducirTokens(resto, tags)
      case CalcVars :: resto =>
        Metodos.calcularVariablesTokens(resto, tags)
      case TVar(id) :: Equal :: resto =>
        Metodos.definirEtiquetaTokens(id, resto, tags)
      case _ =>
        (tags, List("Error interpretando tokens"))
    }
  }
}