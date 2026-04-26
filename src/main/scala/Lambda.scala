sealed trait Lambda

case class Var(nombre: String) extends Lambda
case class Abs(params: String, cuerpo: Lambda) extends Lambda
case class App(func: Lambda, arg: Lambda) extends Lambda
