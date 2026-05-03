import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lineas = Source.stdin.getLines().toList
    val resultado = procesar(lineas, Map.empty[String, Lambda], List.empty[String])
    resultado.foreach(println)
  }

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
    else if (linea.startsWith("calc_vars ")) calcularVariables(linea.drop(10), tags)
    else if (linea.startsWith("reduce ")) (tags, List.empty)
    else if (linea.contains("=")) (definirEtiqueta(linea, tags), List.empty)
    else (tags, List.empty)
  }

  def calcularVariables(textoExp: String, tags: Map[String, Lambda]): (Map[String, Lambda], List[String]) = {
    val exp = parsearConTags(textoExp, tags)
    val libres = Variables.libres(exp).mkString(",")
    val ligadas = Variables.ligadas(exp).mkString(",")
    (tags, List(s"Variables libres: {$libres}", s"Variables ligadas: {$ligadas}"))
  }

  def definirEtiqueta(linea: String, tags: Map[String, Lambda]): Map[String, Lambda] = {
    val partes = linea.split("=", 2)
    val nombre = partes(0).trim
    val expTexto = partes(1).trim
    tags + (nombre -> parsearConTags(expTexto, tags))
  }

  def parsearConTags(texto: String, tags: Map[String, Lambda]): Lambda = {
    val textoResuelto = tags.foldLeft(texto) { (acc, par) =>
      acc.replace("!" + par._1, mostrar(par._2))
    }
    Parser.parsear(textoResuelto)
  }

  def mostrar(exp: Lambda): String = exp match {
    case Var(nombre) => nombre
    case Abs(parametro, cuerpo) => "λ" + parametro + ".(" + mostrar(cuerpo) + ")"
    case App(funcion, argumento) => mostrar(funcion) + " " + mostrar(argumento)
  }
}
