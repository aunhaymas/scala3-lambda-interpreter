import scala.annotation.tailrec

sealed trait PasoReduccion
case class PasoCONV(viejo: String, nuevo: String, nuevaExp: Lambda) extends PasoReduccion
case class PasoSUST(parametro: String, argumento: Lambda, nuevaExp: Lambda) extends PasoReduccion

object Reductor {
  def reducir(estrategia: Boolean, exp: Lambda): Lambda = {
    if (estrategia) reducirCBN(exp) else reducirCBV(exp)
  }

  def reducirCBN(exp: Lambda): Lambda = exp match {
    case App(Abs(parametro, cuerpo), argumento) =>
      reducirCBN(sustituir(cuerpo, parametro, argumento))
    case App(funcion, argumento) =>
      val funcionReducida = reducirCBN(funcion)
      funcionReducida match {
        case Abs(_, _) => reducirCBN(App(funcionReducida, argumento))
        case _ => App(funcionReducida, reducirCBN(argumento))
      }
    case Abs(parametro, cuerpo) => Abs(parametro, reducirCBN(cuerpo))
    case _ => exp
  }

  def reducirCBV(exp: Lambda): Lambda = exp match {
    case App(funcion, argumento) =>
      val funcionReducida = reducirCBV(funcion)
      val argumentoReducido = reducirCBV(argumento)
      funcionReducida match {
        case Abs(parametro, cuerpo) =>
          reducirCBV(sustituir(cuerpo, parametro, argumentoReducido))
        case _ => App(funcionReducida, argumentoReducido)
      }
    case Abs(parametro, cuerpo) => Abs(parametro, reducirCBV(cuerpo))
    case _ => exp
  }

  def sustituir(exp: Lambda, variable: String, valor: Lambda): Lambda = exp match {
    case Var(nombre) =>
      if (nombre == variable) valor else Var(nombre)
    case App(funcion, argumento) =>
      App(sustituir(funcion, variable, valor), sustituir(argumento, variable, valor))
    case Abs(parametro, cuerpo) =>
      if (parametro == variable) Abs(parametro, cuerpo)
      else if (Variables.libres(valor).contains(parametro)) {
        val ocupados = (Variables.libres(valor) ++ Variables.libres(cuerpo)).toSet
        val nuevoParametro = nombreFresco(parametro, ocupados)
        val cuerpoRenombrado = sustituir(cuerpo, parametro, Var(nuevoParametro))
        Abs(nuevoParametro, sustituir(cuerpoRenombrado, variable, valor))
      } else {
        Abs(parametro, sustituir(cuerpo, variable, valor))
      }
  }

  @tailrec
  def nombreFresco(base: String, ocupados: Set[String]): String = {
    val candidato = base + "'"
    if (ocupados.contains(candidato)) nombreFresco(candidato, ocupados)
    else candidato
  }

  def unPaso(estrategia: Boolean, exp: Lambda): Option[PasoReduccion] = {
    if (estrategia) unPasoCBN(exp) else unPasoCBV(exp)
  }

  def unPasoCBN(exp: Lambda): Option[PasoReduccion] = exp match {
    case App(Abs(parametro, cuerpo), argumento) =>
      Some(pasoBeta(parametro, cuerpo, argumento))
    case App(funcion, argumento) =>
      unPasoCBN(funcion) match {
        case Some(paso) => Some(envolver(paso, n => App(n, argumento)))
        case None => unPasoCBN(argumento).map(p => envolver(p, n => App(funcion, n)))
      }
    case Abs(parametro, cuerpo) =>
      unPasoCBN(cuerpo).map(p => envolver(p, n => Abs(parametro, n)))
    case _ => None
  }

  def unPasoCBV(exp: Lambda): Option[PasoReduccion] = exp match {
    case App(funcion, argumento) =>
      // Intenta reducir primero a la derecha
      unPasoCBV(argumento) match {
        case Some(paso) => Some(envolver(paso, n => App(funcion, n)))
        case None =>
          // si arg ya no se puede reducir, reduce izq (funcion)
          unPasoCBV(funcion) match {
            case Some(paso) => Some(envolver(paso, n => App(n, argumento)))
            case None =>
              // cuando sean "normales" --> reduccion Beta
              funcion match {
                case Abs(parametro, cuerpo) => Some(pasoBeta(parametro, cuerpo, argumento))
                case _ => None
              }
          }
      }
    case Abs(parametro, cuerpo) =>
      unPasoCBV(cuerpo).map(p => envolver(p, n => Abs(parametro, n)))
    case _ => None
  }



  def pasoBeta(parametro: String, cuerpo: Lambda, argumento: Lambda): PasoReduccion = {
    necesitaAlpha(cuerpo, parametro, argumento) match {
      case Some((viejo, nuevo)) =>
        val nuevoCuerpo = renombrarPrimero(cuerpo, viejo, nuevo)
        PasoCONV(viejo, nuevo, App(Abs(parametro, nuevoCuerpo), argumento))
      case None =>
        val resultado = sustituir(cuerpo, parametro, argumento)
        PasoSUST(parametro, argumento, resultado)
    }
  }

  def envolver(paso: PasoReduccion, contexto: Lambda => Lambda): PasoReduccion = paso match {
    case PasoCONV(v, n, e) => PasoCONV(v, n, contexto(e))
    case PasoSUST(p, a, e) => PasoSUST(p, a, contexto(e))
  }

  def necesitaAlpha(exp: Lambda, variable: String, valor: Lambda): Option[(String, String)] = exp match {
    case Abs(parametro, cuerpo) =>
      if (parametro == variable) None
      else if (Variables.libres(valor).contains(parametro)) {
        val ocupados = (Variables.libres(valor) ++ Variables.libres(exp)).toSet
        Some((parametro, nombreFresco(parametro, ocupados)))
      } else necesitaAlpha(cuerpo, variable, valor)
    case App(funcion, argumento) =>
      necesitaAlpha(funcion, variable, valor).orElse(necesitaAlpha(argumento, variable, valor))
    case Var(_) => None
  }

  def renombrarPrimero(exp: Lambda, viejo: String, nuevo: String): Lambda = exp match {
    case Abs(parametro, cuerpo) if parametro == viejo =>
      Abs(nuevo, sustituir(cuerpo, viejo, Var(nuevo)))
    case Abs(parametro, cuerpo) =>
      Abs(parametro, renombrarPrimero(cuerpo, viejo, nuevo))
    case App(funcion, argumento) =>
      val nuevaFuncion = renombrarPrimero(funcion, viejo, nuevo)
      if (nuevaFuncion != funcion) App(nuevaFuncion, argumento)
      else App(funcion, renombrarPrimero(argumento, viejo, nuevo))
    case _ => exp
  }
}
