object Metodos {
  def calc_vars(tags: Tags, linea: String): (Tags, List[String]) = {
    val exp = Parser.parsear(resolverEtiquetas(tags, linea))
    val var_lib = Variables.libres(exp)
    val var_lig = Variables.ligadas(exp)
    val salida = List(
      s"Variables libres: {${var_lib.mkString(",")}}",
      s"Variables ligadas: {${var_lig.mkString(",")}}"
    )
    (tags, salida)
  }

  def etiquetar(tags: Tags, linea: String): (Tags, List[String]) = {
    val partes = linea.split("=", 2)
    val nombre = partes(0).trim
    val expTexto = partes(1).trim
    (tags + (nombre -> Parser.parsear(expTexto)), List.empty)
  }

  def reducir(tags: Tags, linea: String): (Tags, List[String]) = {
    (tags, List.empty)
  }

  def resolverEtiquetas(tags: Tags, linea: String): String = {
    tags.foldLeft(linea) {
      case (res, (nombre, exp)) => res.replace(s"!$nombre", resolverExp(exp))
    }
  }

  def resolverExp(exp: Lambda): String = exp match {
    case Var(x) => x
    case Abs(v, cuerpo) => s"λ$v.(${resolverExp(cuerpo)})"
    case App(f, argum) => s"${resolverExp(f)} ${resolverExp(argum)}"
  }
}
