// IGNORAR ESTE ARCHIVO

//sealed trait Token
//case object TLambda extends Token
//case object TPunto extends Token
//case object TParIzq extends Token
//case object TParDer extends Token
//case object TEsp extends Token
//case class TVar(nombre: String)  extends Token

object Tokenizador {
 def tokenizar(linea: String): List[Token] = {
   separador(linea).map {
     case "λ" => TLambda
     case "." => TPunto
     case "(" => TParIzq
     case ")" => TParDer
     case " " => TEsp
     case nombre => TVar(nombre)
   }
 }

 def separador(linea: String): List[String] = {
   val soportados = Set('λ', '\\', '.', '(', ')', ' ')
   linea.foldRight(List[String]()) {
     case (c, acumulado) if soportados(c) => c.toString :: acumulado
     case (c, Nil) => List(c.toString)
     case (c, h::t) if soportados(h.head) => c.toString::h::t
     case (c, h::t) => (c.toString + h)::t
   }.filter(_.nonEmpty)
 }
}
