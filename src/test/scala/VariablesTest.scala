import org.scalatest.funsuite.AnyFunSuite

class VariablesTest extends AnyFunSuite:
  test("Variable suelta es libre") {
    assert(Variables.libres(Var("x")) == List("x"))
    assert(Variables.ligadas(Var("x")) == List.empty)
  }

  test("Abstraccion liga su parametro") {
    val exp = Abs("x", Var("x"))
    assert(Variables.libres(exp) == List.empty)
    assert(Variables.ligadas(exp) == List("x"))
  }

  test("Variable libre dentro de abstraccion") {
    val exp = Abs("x", Var("y"))
    assert(Variables.libres(exp) == List("y"))
    assert(Variables.ligadas(exp) == List("x"))
  }

  test("Aplicacion combina libres de funcion y argumento sin repetir") {
    val exp = App(Var("a"), Var("b"))
    assert(Variables.libres(exp) == List("a", "b"))
    assert(Variables.ligadas(exp) == List.empty)
  }

  test("Ejemplo del enunciado: (lambda x.(lambda y.(y))) a b") {
    val exp = App(App(Abs("x", Abs("y", Var("y"))), Var("a")), Var("b"))
    assert(Variables.libres(exp) == List("a", "b"))
    assert(Variables.ligadas(exp) == List("x", "y"))
  }

  test("Variable que aparece libre y ligada en distintas posiciones") {
    val exp = App(Abs("x", Var("x")), Var("x"))
    assert(Variables.libres(exp) == List("x"))
    assert(Variables.ligadas(exp) == List("x"))
  }
