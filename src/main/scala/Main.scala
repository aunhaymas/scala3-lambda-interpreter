import java.io.PrintWriter

object Main {
  def main(args: Array[String]): Unit = {
    // TP1 MonaLambda: leer líneas por stdin (definiciones, calc_vars, reduce, …).
    // Implementación del intérprete a cargo del grupo.
    val salida = args.head.replaceAll("\\.txt$", "") + ".res"
    val resultado = Interprete.run(scala.io.Source.stdin.getLines())
    val escritor = new PrintWriter(salida)
    resultado.foreach(linea => escritor.println())
    escritor.close()
  }
}
