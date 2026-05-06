object Metodos {
  def calcularVariables(textoExp: String, tags: Map[String, Lambda]): (Map[String, Lambda], List[String]) = {
    intentarParsear(textoExp, tags) match {
      case Right(exp) =>
        val libres = Variables.libres(exp).mkString(",")
        val ligadas = Variables.ligadas(exp).mkString(",")
        (tags, List(s"Variables libres: {$libres}", s"Variables ligadas: {$ligadas}"))
      case Left(mensaje) =>
        (tags, List(mensaje))
    }
  }

  def reducir(resto: String, tags: Map[String, Lambda]): (Map[String, Lambda], List[String]) = {
    val partes = resto.split(" ").toList
    val (estrategia, sinEstrategia) = partes match {
      case "cbn" :: t => (true, t)
      case "cbv" :: t => (false, t)
      case t => (true, t)
    }
    val (debug, restoTexto) = sinEstrategia match {
      case "debug" :: t => (true, t)
      case t => (false, t)
    }
    val expTexto = restoTexto.mkString(" ")
    intentarParsear(expTexto, tags) match {
      case Right(exp) =>
        if (debug) (tags, trazaCompleta(estrategia, exp))
        else (tags, List(mostrarFinal(Reductor.reducir(estrategia, exp))))
      case Left(mensaje) =>
        (tags, List(mensaje))
    }
  }

  def trazaCompleta(estrategia: Boolean, exp: Lambda): List[String] = {
    val inicial = s"EXPR = ${mostrar(exp)}"
    inicial :: continuarTraza(estrategia, exp)
  }

  def continuarTraza(estrategia: Boolean, exp: Lambda): List[String] = {
    Reductor.unPaso(estrategia, exp) match {
      case None => List(mostrarFinal(exp))
      case Some(paso) =>
        val (etiqueta, nuevaExp) = formatearPaso(paso)
        s"$etiqueta = ${mostrar(nuevaExp)}" :: continuarTraza(estrategia, nuevaExp)
    }
  }

  def formatearPaso(paso: PasoReduccion): (String, Lambda) = paso match {
    case PasoCONV(viejo, nuevo, nuevaExp) =>
      (s"CONV[$viejo -> $nuevo]", nuevaExp)
    case PasoSUST(parametro, argumento, nuevaExp) =>
      (s"SUST[$parametro:=${mostrar(argumento)}]", nuevaExp)
  }

  def definirEtiqueta(linea: String, tags: Map[String, Lambda]): (Map[String, Lambda], List[String]) = {
    val partes = linea.split("=", 2)
    val nombre = partes(0).trim
    val expTexto = partes(1).trim
    if (tags.contains(nombre)) {
      (tags, List(s"Error: la etiqueta '$nombre' ya esta definida"))
    } else {
      intentarParsear(expTexto, tags) match {
        case Right(exp) => (tags + (nombre -> exp), List.empty)
        case Left(mensaje) => (tags, List(s"Error: expresion invalida en etiqueta '$nombre'"))
      }
    }
  }

  def intentarParsear(texto: String, tags: Map[String, Lambda]): Either[String, Lambda] = {
    try {
      parsearConTags(texto, tags) match {
        case Right(exp) => Right(exp)
        case Left(err) => Left(err)
      }
    } catch {
      case _: Throwable => Left(s"Error: no se pudo parsear la expresion '$texto'")
    }
  }

  def parsearConTags(texto: String, tags: Map[String, Lambda]): Either[String, Lambda] = {
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
    case Abs(parametro, Var(nombre)) if parametro != nombre => "λ" + parametro + "." + nombre
    case _ => mostrar(exp)
  }
}
