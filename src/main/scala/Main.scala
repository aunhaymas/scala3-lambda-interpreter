import scala.io.StdIn
import scala.annotation.tailrec

object Main {
  // capa impura que envuelve lo funcional
  def main(args: Array[String]): Unit = {
    terminal(Map.empty[String, Lambda])
  }


  @tailrec
  def terminal(contextoActual: Map[String, Lambda]): Unit = {
    // Mona anti sai efets
    Option(StdIn.readLine()) match {
      case None => ()
      case Some(linea) =>
        linea.trim match {
          case "" =>
            terminal(contextoActual)
          case texto => val (nuevoContexto, salida) = Interprete.ejecutarLinea(texto, contextoActual)
            printOutput(salida)
            terminal(nuevoContexto)
        }
    }
  }

  def printOutput(outputs: List[String]): Unit = outputs match {
    case Nil => ()
    case h :: t =>
      println(h)
      printOutput(t)
  }
}