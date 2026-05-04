object Reductor {
  def reducir(estrategia: Boolean, exp: Lambda): Lambda = {
    if (estrategia) reducirCBN(exp) else reducirCBV(exp)
  }

  def reducirCBN(exp: Lambda): Lambda = exp match {
    case App(Abs(parametro, cuerpo), argumento) =>
      reducirCBN(sustituir(cuerpo, parametro, argumento))
    case App(funcion, argumento) =>
      val funcionReducida = reducirCBN(funcion)
      funcionReducida match {
        case Abs(_, _) => reducirCBN(App(funcionReducida, argumento))
        case _ => App(funcionReducida, reducirCBN(argumento))
      }
    case Abs(parametro, cuerpo) => Abs(parametro, reducirCBN(cuerpo))
    case _ => exp
  }

  def reducirCBV(exp: Lambda): Lambda = exp match {
    case App(funcion, argumento) =>
      val funcionReducida = reducirCBV(funcion)
      val argumentoReducido = reducirCBV(argumento)
      funcionReducida match {
        case Abs(parametro, cuerpo) =>
          reducirCBV(sustituir(cuerpo, parametro, argumentoReducido))
        case _ => App(funcionReducida, argumentoReducido)
      }
    case Abs(parametro, cuerpo) => Abs(parametro, reducirCBV(cuerpo))
    case _ => exp
  }

  def sustituir(exp: Lambda, variable: String, valor: Lambda): Lambda = exp match {
    case Var(nombre) =>
      if (nombre == variable) valor else Var(nombre)
    case App(funcion, argumento) =>
      App(sustituir(funcion, variable, valor), sustituir(argumento, variable, valor))
    case Abs(parametro, cuerpo) =>
      if (parametro == variable) Abs(parametro, cuerpo)
      else if (Variables.libres(valor).contains(parametro)) {
        val ocupados = (Variables.libres(valor) ++ Variables.libres(cuerpo)).toSet
        val nuevoParametro = nombreFresco(parametro, ocupados)
        val cuerpoRenombrado = sustituir(cuerpo, parametro, Var(nuevoParametro))
        Abs(nuevoParametro, sustituir(cuerpoRenombrado, variable, valor))
      } else {
        Abs(parametro, sustituir(cuerpo, variable, valor))
      }
  }

  def nombreFresco(base: String, ocupados: Set[String]): String = {
    val candidato = base + "'"
    if (ocupados.contains(candidato)) nombreFresco(candidato, ocupados)
    else candidato
  }
}
