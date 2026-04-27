sealed trait Lambda
//<λexp> ::=  | <var> #Variable
//            | <LAMBDA> <var> <DOT> <λexp> #Abstracción
//            | <LPAR> <λexp> <SPACE> <λexp> <RPAR> #Aplicación

case class Var(variable: String) extends Lambda
case class Abs(variable: String, cuerpo: Lambda) extends Lambda
case class App(funcion: Lambda, argumento: Lambda) extends Lambda
