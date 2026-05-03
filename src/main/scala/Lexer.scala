import scala.annotation.tailrec

object Lexer {
  // tokenize, recibeInput:String
  // retorna una lista de tokens
  // Ejemplo: tokenize("reduce cbn λx.(λy.(y) x)")
  // input  = "reduce cbn λx.y"
  // tokenize(input)->
  // Reduce,Space,CBN,Space,Lambda,TVar("x"),TVar("y")
  def tokenize(input:String): List[Token] ={
    val input_list = input.toList
    @tailrec
    def create_list(remaining: List[Char], acc: List[Token]): List[Token] = {
      remaining match {
        case Nil => acc.reverse // damos vuelta la lista
        // armamos una lista, agregando cada token en una lista
        case 'λ' :: rest => create_list(rest, Lambda :: acc)
        case '.' :: rest => create_list(rest, Dot :: acc)
        case '(' :: rest => create_list(rest, LPar :: acc)
        case ')' :: rest => create_list(rest, RPar :: acc)
        case ' ' :: rest => create_list(rest, Space :: acc)
        case '\n' :: rest => create_list(rest, Space :: acc)
        case '='::rest=> create_list(rest, Equal :: acc)// Etiquetas
        case c::_ if c.isLetter =>
          val (word,more)=getWord(remaining)
          create_list(more, getOp(word) ::acc)
        case '!'::rest=> // Etiquetas
          val (name, more) = getWord(rest)
          create_list(more,TTag(name)::acc)
        case _ :: rest => create_list(rest, acc) // Para carácteres no válidps
      }
    }
    val res :List[Token] =clean_spaces(create_list(input_list, Nil),Nil)
    clean_first_spaces(res.reverse)
  }

  private def getOp(word: String): (Token) = word match {
    case "calc_vars" => CalcVars
    case "reduce" => Reduce
    case "cbv" => CBV
    case "cbn" => CBN
    case "debug" => Debug
    case _ => TVar(word) //asumo que es una variable
  }

  private def getWord(chars: List[Char]): (String, List[Char]) = {
    val (word, rest) = chars.span(c => c.isDigit || c.isLetter || c=='_')

    (word.mkString, rest)
  }
    @tailrec
    private def clean_spaces(list:List[Token],accum:List[Token]):List[Token] = list match{
      case Nil => accum
      // Espacios dobles
      case Space::next::rest if next==Space =>
        clean_spaces(next::rest, accum)
      // Space finales
      case h::Space::Nil => clean_spaces(Nil,h::accum)
      // Space entre dos elementos que no son parentesis o TVars
      case h::Space::rest if h==LPar || h==Lambda || h==Dot||h==Equal
      => clean_spaces(h::rest,accum)
      // Space antes de ')':  (λx.x   )-> (λx.x)
      case Space::mid::rest if mid==RPar || mid==Dot
      => clean_spaces(mid::rest,accum)

      // Space válido! (λx.x) y-> (λx.x) y
      case Space::rest => clean_spaces(rest,Space::accum)
      // SigaSiga
      case h::rest => clean_spaces(rest,h::accum)
    }
    @tailrec
    private def clean_first_spaces(list: List[Token]):List[Token] = list match {
    case Nil=>Nil
    case Space::rest => clean_first_spaces(rest)
    case h::rest => h::rest

  }
  }
