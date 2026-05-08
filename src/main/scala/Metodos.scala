  import scala.annotation.tailrec

  object Metodos {
    def reducirTokens(tokens: List[Token], tags: Map[String, Lambda]): (Map[String, Lambda], List[String]) = {
      tokens match {
        // CBN DEBUG
        case CBN :: Debug :: resto =>
          parsear(resto, tags) match {
            case Right(ast) =>
              (tags, ejecutarDebug(true, ast))
            case Left(err) =>
              (tags, List(s"Error: $err"))
          }
        // CBV DEBUG
        case CBV :: Debug :: resto =>
          parsear(resto, tags) match {

            case Right(ast) =>
              (tags, ejecutarDebug(false, ast))
            case Left(err) =>
              (tags, List(s"Error: $err"))
          }
        // CBN
        case CBN :: resto =>
          reducirNormal(
            true,
            resto,
            tags
          )
        // CBV
        case CBV::resto =>
          reducirNormal(
            false,
            resto,
            tags
          )
      }
    }
    def calcularVariablesTokens(
                                 tokens: List[Token],
                                 tags: Map[String, Lambda]
                               ): (Map[String, Lambda], List[String]) = {

      parsear(tokens, tags) match {
        case Right(ast) =>
          val libres =
            Variables.libres(ast).mkString(",")
          val ligadas =
            Variables.ligadas(ast).mkString(",")
          (tags, List(s"Variables libres: {$libres}", s"Variables ligadas: {$ligadas}"))
        case Left(err) =>
          (tags, List(s"Error: $err"))
      }
    }
    def definirEtiquetaTokens(
                               nombre: String,
                               tokens: List[Token],
                               tags: Map[String, Lambda]
                             ): (Map[String, Lambda], List[String]) = {

      nombre match {
        case n if tags.contains(n) =>
          (tags, List(s"Error: '$n' ya está definido"))
        case n =>
          parsear(tokens, tags) match {
            case Left(err) =>
              (tags, List(s"Error '$n': $err"))
            case Right(ast) =>
              (tags + (n -> ast), List())
          }
      }
    }
    private def parsear(
                         tokens: List[Token],
                         tags: Map[String, Lambda]
                       ): Either[String, Lambda] = {
      Parser.parsearTokens(tokens, tags)
    }
    private def reducirNormal(estrategiaCBN: Boolean, tokens: List[Token], tags: Map[String, Lambda]): (Map[String, Lambda], List[String]) = {
      val estrategias = Map(true  -> Reductor.reducirCBN _, false -> Reductor.reducirCBV _)
      parsear(tokens, tags).fold(
        error => (tags, List(s"Error: $error")),
        ast   => (tags, List(Parser.imprimir(estrategias(estrategiaCBN)(ast))))
      )
    }

    private def ejecutarDebug(isCBN: Boolean, ast: Lambda): List[String] = {
      @tailrec
      def rec(exp: Lambda, acc: List[String]): List[String] = {
        Reductor.unPaso(isCBN, exp) match {
          case Some(paso) =>
            val (nuevaExp, mensaje) = paso match {
              case PasoCONV(v, n, nExp) =>
                (nExp, s"CONV[$v -> $n] = ${Parser.imprimir(nExp)}")
              case PasoSUST(param, arg, nExp) =>
                (nExp, s"SUST[$param:=${Parser.imprimir(arg)}] = ${Parser.imprimir(nExp)}")
            }
            rec(nuevaExp, mensaje :: acc)
          case None =>
            // ultima expresión!
            (Parser.imprimir(exp) :: acc).reverse
        }
      }

      // meto el header primero para que el reverse lo deje en su lugar
      val header: String = s"EXPR = ${Parser.imprimir(ast)}"
      rec(ast, List(header))
    }
  }