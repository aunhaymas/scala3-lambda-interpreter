import scala.annotation.tailrec

object Parser {
  // Convierte el AST de vuelta a una cadena de texto legible
  def imprimir(ast: Lambda): String = ast match {
    //case Abs(param, Var(idCuerpo)) => s"λ$param.$idCuerpo"
    case Var(nombre) => nombre
    case Abs(param, cuerpo) => s"λ$param.(${imprimir(cuerpo)})"
    case App(funcion, argumento) =>
      val strF = funcion match {
        case Abs(_, _) => s"(${imprimir(funcion)})"
        case _ => imprimir(funcion)
      }
      val strA = argumento match {
        case App(_, _) => s"(${imprimir(argumento)})"
        case Abs(_, _) => s"(${imprimir(argumento)})"
        case _ => imprimir(argumento)
      }
      s"$strF $strA"
  }

  def parsear(texto: String, tags: Map[String, Lambda] = Map.empty): Either[String, Lambda] =
    Lexer.tokenize(texto) match {
      case Left(err) => Left(err)
      case Right(tokens) => parsearTokens(tokens, tags)
    }
  // polk: solo recibe la lista de tokens (no conoce al lexer)
  def parsearTokens(tokens: List[Token], tags: Map[String, Lambda]): Either[String, Lambda] =
    parsearExpresion(tokens.filterNot(_ == Space), tags) match {
      case Right((exp, Nil)) => Right(exp)
      case Right((_, resto)) => Left(s"Error: tokens inesperados $resto")
      case Left(err) => Left(err)
    }

  def parsearExpresion(tokens: List[Token], tags: Map[String, Lambda]): Either[String, (Lambda, List[Token])] =
    parsearAtomo(tokens, tags) match {
      case Left(err) => Left(err)
      case Right((exp, tokens_restantes)) => parsearAplicacion(exp, tokens_restantes, tags)
    }

  @tailrec
  private def parsearAplicacion(acumulado: Lambda, tokens: List[Token], tags: Map[String, Lambda]): Either[String, (Lambda, List[Token])] =
    tokens match {
      case Nil | (RPar :: _) => Right((acumulado, tokens))
      case _ =>
        parsearAtomo(tokens, tags) match {
          case Left(err) => Left(err)
          case Right((siguiente, restantes)) => parsearAplicacion(App(acumulado, siguiente), restantes, tags)
        }
    }

  private def parsearAtomo(tokens: List[Token], tags: Map[String, Lambda]): Either[String, (Lambda, List[Token])] =
    tokens match {
      case Nil =>
        Left("Error: expresión vacía")

      case Lambda :: TVar(nombre) :: Dot :: LPar :: resto =>
        parsearExpresion(resto, tags) match {
          case Left(err) => Left(err)
          case Right((cuerpo, RPar :: r)) => Right((Abs(nombre, cuerpo), r))
          case Right(_) => Left("Error: falta ')' cerrando cuerpo de abstracción")
        }

      case Lambda :: TVar(_) :: Dot :: _ =>
        Left("Error: el cuerpo de la abstracción debe ir entre paréntesis")

      case Lambda :: _ =>
        Left("Error: abstracción mal formada")

      case LPar :: t =>
        parsearExpresion(t, tags) match {
          case Left(err) => Left(err)
          case Right((exp, RPar :: resto)) => Right((exp, resto))
          case Right((_, _)) => Left("Error: falta ')'")
        }

      case RPar :: _ =>
        Left("Error: ')' inesperado")

      case TVar(nombre) :: t =>
        Right((Var(nombre), t))

      case TTag(nombre) :: t =>
        tags.get(nombre) match {
          case Some(astGuardado) => Right((astGuardado, t)) // Reemplaza por el AST
          case None => Left(s"Etiqueta no definida '!$nombre'")
        }

      case tok :: _ =>
        Left(s"Error: token inválido $tok")
    }
}