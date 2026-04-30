import scala.compiletime.testing.ErrorKind.Parser

object Metodos {
  def calc_vars(tags: Tags, linea: String): (Tags, List[String]) = {
    val exp = Parser.parsear(resolverEtiquetas(tags, linea)) // Hay que implementar el Parser
    val var_lib = Variables.libres(exp) 
    val var_lig = Variables.ligadas(exp)
    val salida = List(
      s"Variables libres: {${var_lib.mkString(",")}}",
      s"Variables ligadas: {${var_lig.mkString(",")}}"
    )
    (tags, salida)
  }
  
  def etiquetar(tags: Tags, linea: String): (Tags, List[String]) = {
    val partes = linea.split("=", 2).toList
    val (nombre, exp) = (partes.head, partes.tail)
    
    // Hay que implementar que si la tag ya estaba, no se agrege denuevo
    
    (tags + (nombre -> Parser.parsear(exp)), List(""))
  }
  
  def reducir(tags: Tags, linea: String): (Tags, List[String]) = {
    val partes = linea.trim.split("\\s+").toList
    
    // esto tambien se podria implementar de otra forma (creo)
    // true: call by name, false: call by value
    val (estrategia, cola) = partes match {
      case "cbn" :: t => (true, t)
      case "cbv" :: t => (false, t)
    }
    //
    val (debug, cola2) = cola match {
      case "debug" :: t => (true, t)
      case t => (false, t)
    }
    val expLinea = cola2.mkString(" ")
    val exp = Parser.parsear(resolverEtiquetas(tags, expLinea))
    
    val pasos = Reductor.reducir(estrategia, debug, exp)
    val head = s"EXPR = (FUNCION QUE IMPRIME UNA EXPRESION LAMBDA)"
    (tags, head :: pasos)
  }
  
  def resolverEtiquetas(tags:Tags, linea: String): String = { 
    tags.foldLeft(linea) {
      case (res, (nombre, exp)) => res.replace(s"!$nombre", "FUNCIÓN QUE IMPRIME UNA EXPRESION LAMBDA")
    }
  }
  
  def resolverExp(exp: Lambda): String = exp match {
    case Var(x) => x
    case Abs(v, cuerpo) => s"λ$v.(${print(cuerpo)})"
    case App(f, argum) =>
      
  }
}
