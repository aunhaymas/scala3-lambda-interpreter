object Parser {
  def parsear(linea: String): Lambda =
    parsearRec(Lexer.tokenize(linea).filterNot(_ == Space))._1

  def parsearRec(tokens: List[Token]): (Lambda, List[Token]) = {
    val (h, t) = parsearLambda(tokens)
    def rec(acumulado: Lambda, tokens_restantes: List[Token]): (Lambda, List[Token]) =
      tokens_restantes match {
        case Nil | (RPar :: _) | (Dot :: _) => (acumulado, tokens_restantes)
        case _ =>
          val (siguiente, restantes) = parsearLambda(tokens_restantes)
          rec(App(acumulado, siguiente), restantes)
    }
    rec(h, t)
  }
  
  def parsearLambda(tokens: List[Token]): (Lambda, List[Token]) =
    tokens match {
      case Lambda :: TVar(nombre) :: Dot :: LPar :: t =>
        val (cuerpo, resto) = parsearRec(t)
        (Abs(nombre, cuerpo), resto)

      case LPar :: t =>
        val(exp, resto) = parsearRec(t)
        (exp, resto)

      case TVar(nombre) :: t =>
        (Var(nombre), t)
    }
}
