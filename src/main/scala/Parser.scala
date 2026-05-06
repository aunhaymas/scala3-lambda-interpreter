object Parser {

  def parsear(texto: String): Either[String, Lambda] =
    Lexer.tokenize(texto) match {
      case Left(err) => Left(err)
      case Right(tokens) =>
        parsearExpresion(tokens.filterNot(_ == Space)) match {
          case Right((exp, Nil)) => Right(exp)
          case Right((_, resto)) => Left(s"Error: tokens inesperados $resto")
          case Left(err) => Left(err)
        }
    }

  def parsearExpresion(tokens: List[Token]): Either[String, (Lambda, List[Token])] =
    parsearAtomo(tokens) match {
      case Left(err) => Left(err)
      case Right((exp, tokens_restantes)) => parsearAplicacion(exp, tokens_restantes)
    }

  private def parsearAplicacion(acumulado: Lambda, tokens: List[Token]): Either[String, (Lambda, List[Token])] =
    tokens match {
      case Nil | (RPar :: _) => Right((acumulado, tokens))
      case _ =>
        parsearAtomo(tokens) match {
          case Left(err) => Left(err)
          case Right((siguiente, restantes)) => parsearAplicacion(App(acumulado, siguiente), restantes)
        }
    }

  private def parsearAtomo(tokens: List[Token]): Either[String, (Lambda, List[Token])] =
    tokens match {
      case Nil =>
        Left("Error: expresión vacía")

      case Lambda :: TVar(nombre) :: Dot :: LPar :: resto =>
        parsearExpresion(resto) match {
          case Left(err) => Left(err)
          case Right((cuerpo, RPar :: r)) => Right((Abs(nombre, cuerpo), r))
          case Right(_) => Left("Error: falta ')' cerrando cuerpo de abstracción")
        }

      case Lambda :: TVar(_) :: Dot :: _ =>
        Left("Error: el cuerpo de la abstracción debe ir entre paréntesis")

      case Lambda :: _ =>
        Left("Error: abstracción mal formada")

      case LPar :: t =>
        parsearExpresion(t) match {
          case Left(err) => Left(err)
          case Right((exp, RPar :: resto)) => Right((exp, resto))
          case Right((_, _)) => Left("Error: falta ')'")
        }

      case RPar :: _ =>
        Left("Error: ')' inesperado")

      case TVar(nombre) :: t =>
        Right((Var(nombre), t))

      case tok :: _ =>
        Left(s"Error: token inesperado $tok")
    }
}