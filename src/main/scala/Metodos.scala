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
    
  }
}
