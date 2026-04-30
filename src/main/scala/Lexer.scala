import scala.annotation.tailrec

object Lexer {
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
        case _ :: rest => create_list(rest, acc) // Ignorar caracteres no válidos
      }
    }

    create_list(input_list, Nil)
  }
  }
