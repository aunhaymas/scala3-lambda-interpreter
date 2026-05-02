object Reductor {
  
  // Devuelve la expresión reducida un paso, o None si ya está en forma normal
  def reducirPaso(exp: Lambda, estrategia: Boolean): Option[Lambda] = exp match {
    // 1. Beta-reducción directa: (λx.body) arg
    case App(Abs(v, body), arg) => Some(Sustituidor.aplicar(body, v, arg))
    
    // 2. Si no es un redex, aplicamos la estrategia (CBN o CBV)
    case App(f, a) =>
      if (estrategia) { // CBN: Call-by-name (reduce el izquierdo primero)
        reducirPaso(f, estrategia).map(App(_, a)).orElse(reducirPaso(a, estrategia).map(App(f, _)))
      } else { // CBV: Call-by-value (reduce el argumento primero)
        reducirPaso(a, estrategia).map(App(f, _)).orElse(reducirPaso(f, estrategia).map(App(_, a)))
      }
      
    case Abs(v, body) => reducirPaso(body, estrategia).map(Abs(v, _))
    case _ => None
  }

  def reducir(estrategia: Boolean, debug: Boolean, exp: Lambda): List[String] = {
    // Definimos una función que devuelva tanto la expresión final como los pasos
    def loop(e: Lambda, pasos: List[String]): (Lambda, List[String]) = {
      reducirPaso(e, estrategia) match {
        case Some(next) =>
          val nuevoPaso = if (debug) s"Paso: ${next.toString}" else ""
          // Si debug es true, guardamos el paso. Si no, seguimos reduciendo sin guardar.
          val nuevosPasos = if (debug) pasos :+ nuevoPaso else pasos
          loop(next, nuevosPasos)
        case None => 
          (e, pasos) // Ya no se puede reducir más, devolvemos la expresión final y la lista
      }
    }
    
    val (resultadoFinal, pasos) = loop(exp, Nil)
    
    // Aquí unes todo: La expresión final y los pasos que ocurrieron
    List(s"Resultado: ${resultadoFinal.toString}") ++ pasos
  }

  
  private object Sustituidor {
    def aplicar(exp: Lambda, varName: String, valor: Lambda): Lambda = exp match {
      case Var(x) => if (x == varName) valor else Var(x)
      
      case App(f, a) => App(aplicar(f, varName, valor), aplicar(a, varName, valor))
      
      case Abs(v, body) =>
        if (v == varName) Abs(v, body) 
        
        else if (Variables.libres(valor).contains(v)) {
          // Conflicto de nombres: Renombramos 'v' (Conversión-alpha)
          val nuevoV = v + "'"
          val nuevoBody = aplicar(body, v, Var(nuevoV))
          Abs(nuevoV, aplicar(nuevoBody, varName, valor))
        } else {
          Abs(v, aplicar(body, varName, valor))
        }
    }
  }



}