object Variables {
  def libres(exp: Lambda): List[String] = exp match {
    case Var(x) => List(x)
    case Abs(v, cuerpo) => libres(cuerpo).filterNot(_ == v)
    case App(f, a) => dedup(libres(f) ++ libres(a))
  }

  def ligadas(exp: Lambda): List[String] = exp match {
    case Var(_) => List.empty
    case Abs(v, cuerpo) => dedup(v :: ligadas(cuerpo))
    case App(f, a) => dedup(ligadas(f) ++ ligadas(a))
  }

  private def dedup(xs: List[String]): List[String] =
    xs.foldLeft(List.empty[String])((acc, x) => if (acc.contains(x)) acc else acc :+ x)
}
