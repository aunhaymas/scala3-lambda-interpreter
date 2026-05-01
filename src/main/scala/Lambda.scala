sealed trait Lambda
//<λexp> ::=  | <var> #Variable
//            | <LAMBDA> <var> <DOT> <λexp> #Abstracción
//            | <LPAR> <λexp> <SPACE> <λexp> <RPAR> #Aplicación

case class Var(variable: String) extends Lambda
case class Abs(variable: String, cuerpo: Lambda) extends Lambda
case class App(funcion: Lambda, argumento: Lambda) extends Lambda


sealed trait Token
case object Lambda extends Token// (ctrl+shift+u 03bb)
case object Dot extends Token // .
case object LPar extends Token  // (
case object RPar extends Token  // )
case object Space extends Token
case class TVar(x: String) extends Token
//Para etiquetas
case class TTag(x:String) extends Token // !Tags

case object Equal extends Token
case object CalcVars extends  Token
case object Reduce extends  Token