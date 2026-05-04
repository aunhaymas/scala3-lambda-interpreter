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
    else if (linea.startsWith("reduce ")) reducir(linea.drop(7), tags)
    else if (linea.contains("=")) (definirEtiqueta(linea, tags), List.empty)
    else (tags, List.empty)
  }

  def calcularVariables(textoExp: String, tags: Map[String, Lambda]): (Map[String, Lambda], List[String]) = {
    val exp = parsearConTags(textoExp, tags)
    val libres = Variables.libres(exp).mkString(",")
    val ligadas = Variables.ligadas(exp).mkString(",")
    (tags, List(s"Variables libres: {$libres}", s"Variables ligadas: {$ligadas}"))
  }

  def reducir(resto: String, tags: Map[String, Lambda]): (Map[String, Lambda], List[String]) = {
    val partes = resto.split(" ").toList
    val (estrategia, sinEstrategia) = partes match {
      case "cbn" :: t => (true, t)
      case "cbv" :: t => (false, t)
      case t => (true, t)
    }
    val expTexto = sinEstrategia match {
      case "debug" :: t => t.mkString(" ")
      case t => t.mkString(" ")
    }
    val exp = parsearConTags(expTexto, tags)
    val reducida = Reductor.reducir(estrategia, exp)
    (tags, List(mostrarFinal(reducida)))
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
    case App(funcion, argumento) => mostrarFuncion(funcion) + " " + mostrarArgumento(argumento)
  }

  def mostrarFuncion(exp: Lambda): String = exp match {
    case Abs(_, _) => "(" + mostrar(exp) + ")"
    case _ => mostrar(exp)
  }

  def mostrarArgumento(exp: Lambda): String = exp match {
    case App(_, _) => "(" + mostrar(exp) + ")"
    case Abs(_, _) => "(" + mostrar(exp) + ")"
    case _ => mostrar(exp)
  }

  def mostrarFinal(exp: Lambda): String = exp match {
    case Abs(parametro, Var(nombre)) => "λ" + parametro + "." + nombre
    case _ => mostrar(exp)
  }
}
