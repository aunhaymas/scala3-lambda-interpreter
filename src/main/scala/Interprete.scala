object Interprete {
  def procesarLinea(tags: Tags, linea: String): (Tags, List[String]) = {
    val trimmed = linea.trim
    trimmed match {
      case "" => (tags, Nil)
      case l if l.startsWith("calc_vars") => Metodos.calc_vars(tags, l)
      case l if l.startsWith("reduce") => Metodos.reducir(tags, l)
      case l if l.contains("=") && !l.startsWith("calc_vars") && !l.startsWith("reduce") => Metodos.etiquetar(tags, l)
    }
  }

  def run(lineas: List[String]): List[String] = {
    lineas.foldLeft((Map.empty[String, Lambda], List.empty[String])) {
      case ((tags, salida), linea) => 
        val (tags2, lineas) = procesarLinea(tags, linea)
        (tags2, salida ++ lineas)
    }._2
  }
}
