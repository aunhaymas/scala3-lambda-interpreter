import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lineas = Source.stdin.getLines().toList
    //val lineas = List("MI_EJEMPLO=(λx.(λy.(y)) (λx.(x x) λx.(x x)))", "calc_vars !MI_EJEMPLO", "reduce cbn !MI_EJEMPLO", "IDZ=λz.(z)", "calc_vars (λf.(f b)) (λx.(!IDZ) z) a", "reduce cbv debug (λf.(f b)) (λx.(!IDZ) z) a")
    val resultado = Interprete.procesar(lineas, Map.empty[String, Lambda], List.empty[String])
    resultado.foreach(println)
  }
}
