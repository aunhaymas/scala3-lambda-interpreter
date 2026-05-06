import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite:
  test("enunciado") {
    val res = Parser.parsear("λx.(λy.(y)) (λx.(x x) λx.(x x))")
    assert(res == Right(App(Abs("x",Abs("y", Var("y"))), App(Abs("x", App(Var("x"), Var("x"))), Abs("x", App(Var("x"), Var("x")))))))
  }
  test("input_1") {
    val exp = Parser.parsear("λx.(λy.(y) x)") match {
      case Right(l) => l
      case Left(err) => fail(err)
    }
    assert(exp == Abs("x", App(Abs("y", Var("y")), Var("x"))))
  }
  test("input_2") {
    val exp = Parser.parsear("(λx.(λy.(x))) y") match {
      case Right(l) => l
      case Left(err) => fail(err)
    }
    assert(exp == App(Abs("x", Abs("y", Var("x"))), Var("y")))
  }
  test("Beta-reduccion simple: (λx.(x)) y -> y") {
    val exp = Parser.parsear("(λx.(x)) y") match {
      case Right(l) => l
      case Left(err) => fail(err)
    }
    assert(Reductor.reducir(true, exp) == Var("y"))
  }

  test("Sustitucion sin captura: (λx.(λy.(x))) z -> λy.z") {
    val exp = Parser.parsear("(λx.(λy.(x))) z") match {
      case Right(l) => l
      case Left(err) => fail(err)
    }
    assert(Reductor.reducir(true, exp) == Abs("y", Var("z")))
  }

  test("Alpha renombre cuando hay captura: (λx.(λy.(x))) y -> λy'.y") {
    val exp = Parser.parsear("(λx.(λy.(x))) y") match {
      case Right(l) => l
      case Left(err) => fail(err)
    }
    assert(Reductor.reducir(true, exp) == Abs("y'", Var("y")))
  }
