import org.scalatest.funsuite.AnyFunSuite

class ReductorTest extends AnyFunSuite:
  test("Beta-reduccion simple: (lambda x.x) y -> y") {
    val exp = App(Abs("x", Var("x")), Var("y"))
    assert(Reductor.reducir(true, exp) == Var("y"))
  }

  test("Sustitucion sin captura: (lambda x.(lambda y.x)) z -> lambda y.z") {
    val exp = App(Abs("x", Abs("y", Var("x"))), Var("z"))
    assert(Reductor.reducir(true, exp) == Abs("y", Var("z")))
  }

  test("Alpha renombre cuando hay captura: (lambda x.(lambda y.x)) y -> lambda y'.y") {
    val exp = App(Abs("x", Abs("y", Var("x"))), Var("y"))
    assert(Reductor.reducir(true, exp) == Abs("y'", Var("y")))
  }

  test("Shadowing: variable interna no se sustituye") {
    val exp = App(Abs("x", Abs("x", Var("x"))), Var("z"))
    assert(Reductor.reducir(true, exp) == Abs("x", Var("x")))
  }

  test("CBN no evalua argumento no usado (omega)") {
    val omega = App(Abs("x", App(Var("x"), Var("x"))), Abs("x", App(Var("x"), Var("x"))))
    val exp = App(Abs("y", Var("y")), omega)
    assert(Reductor.reducir(true, exp) == omega)
  }

  test("unPaso devuelve PasoSUST cuando no hace falta alpha") {
    val exp = App(Abs("x", Var("x")), Var("y"))
    Reductor.unPaso(true, exp) match {
      case Some(PasoSUST("x", Var("y"), Var("x"))) => succeed
      case otro => fail(s"Esperaba PasoSUST, llego $otro")
    }
  }

  test("unPaso devuelve PasoCONV cuando hace falta alpha") {
    val exp = App(Abs("x", Abs("y", Var("x"))), Var("y"))
    Reductor.unPaso(true, exp) match {
      case Some(PasoCONV("y", "y'", _)) => succeed
      case otro => fail(s"Esperaba PasoCONV[y -> y'], llego $otro")
    }
  }
  test("omega_cbn") {
    //val exp = App(Abs("x", Abs("y", Var("y"))), App(Abs("x", App(Var("x"), Var("x"))), Abs("x", App(Var("x"), Var("x")))))
    val exp = Parser.parsear("(λx.(λy.(y)) (λx.(x x) λx.(x x)))") match {
      case Right(l) => l
      case Left(err) => fail(err)
    }
    val res = Abs("y", Var("y"))
    assert(Reductor.reducir(true, exp) == res)
  }
  test("abs_y_x_cbn") {
    val exp = Parser.parsear("λx.(λy.(y) x)") match {
      case Right(l) => l
      case Left(err) => fail(err)
    }
    val res = Abs("x", Var("x"))
    assert(Reductor.reducir(true, exp) == res)
  }
