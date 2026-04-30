import scala.annotation.tailrec

object Lexer {
  private def getWord(chars: List[Char]):(String,List[Char])={
    val (word, rest)= chars.span(c=>c.isDigit||c.isLetter)
    return (word.mkString,rest)
  }
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
        case '!'::rest=> // Etiquetas
          val (name, more) = getWord(rest)
          create_list(more,TTag(name)::acc)
        case c::_ if c.isLetter =>
          val (word,more)=getWord(remaining)
          create_list(more, TVar(word)::acc)

        case _ :: rest => create_list(rest, acc) // Para carácteres no válidps
      }
    }

    create_list(input_list, Nil)
  }
  }
