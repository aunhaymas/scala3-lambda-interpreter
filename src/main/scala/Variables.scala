object Variables {
  def libres(exp: Lambda): List[String] = exp match {
    case Var(nombre) => List(nombre)
    case Abs(parametro, cuerpo) => libres(cuerpo).filter(_ != parametro)
    case App(funcion, argumento) => (libres(funcion) ++ libres(argumento)).distinct
  }

  def ligadas(exp: Lambda): List[String] = exp match {
    case Var(_) => List.empty
    case Abs(parametro, cuerpo) => (parametro :: ligadas(cuerpo)).distinct
    case App(funcion, argumento) => (ligadas(funcion) ++ ligadas(argumento)).distinct
  }
}
