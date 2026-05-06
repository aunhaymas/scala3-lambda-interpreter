object Interprete {
  def procesar(lineas: List[String], tags: Map[String, Lambda], salida: List[String]): List[String] = {
    if (lineas.isEmpty) salida
    else {
      val linea = lineas.head.trim
      val (nuevasTags, nuevaSalida) = ejecutar(linea, tags)
      procesar(lineas.tail, nuevasTags, salida ++ nuevaSalida)
    }
  }

  def ejecutar(linea: String, tags: Map[String, Lambda]): (Map[String, Lambda], List[String]) = {
    if (linea.isEmpty) (tags, List.empty)
    else if (linea.startsWith("calc_vars ")) Metodos.calcularVariables(linea.drop(10), tags)
    else if (linea.startsWith("reduce ")) Metodos.reducir(linea.drop(7), tags)
    else if (linea.contains("=")) Metodos.definirEtiqueta(linea, tags)
    else (tags, List.empty)
  }
}
