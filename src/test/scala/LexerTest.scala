//https://docs.scala-lang.org/scala3/book/tools-sbt.html#using-sbt-with-scalatest

import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite:
  test("Lexer tokenize λx.x") {
    val input = "λx.x"
    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected: List[Token] = List(Lambda, TVar("x"), Dot, TVar("x"))
    assert(tokens.equals(expected))
  }
  test("Lexer tokenize λx.x con espacios extra") {
    val input = "    λx.x"
    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected: List[Token] = List(Lambda, TVar("x"), Dot, TVar("x"))
    assert(tokens.equals(expected))
  }
  test ("Lexer tokeniza reduce cbn λx.(λy.(y) x)"){
    val input = "reduce cbn λx.(λy.(y) x)"
    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected:List[Token] = (
      List(Reduce,CBN,
        Lambda,TVar("x"),Dot,LPar,
        Lambda,TVar("y"),Dot,LPar,TVar("y"),RPar,Space,TVar("x"),RPar))
    assert(tokens.equals(expected))
    }
  test("Lexer tokeniza reduce cbn λx.(λy.(y) x con espacios extra)") {
    val input = "reduce  cbn λx .(λy. (y)     x)"
    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected: List[Token] = (
      List(Reduce, CBN,
        Lambda, TVar("x"), Dot, LPar,
        Lambda, TVar("y"), Dot, LPar, TVar("y"), RPar, Space, TVar("x"), RPar))
    assert(tokens.equals(expected))
  }
  test("Lexer tokeniza reduce cbn (λx.(λy.(x))) y ") {
    val input = "reduce cbn (λx. (λy. (x))) y"
    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected = List(
      Reduce, CBN, LPar, Lambda, TVar("x"), Dot,
      LPar, Lambda, TVar("y"), Dot, LPar, TVar("x"), RPar, RPar, RPar, Space, TVar("y")
    )
    assert(expected.equals(tokens))
    }
  test("reduce cbn (λx.(x) y) (λy.(y) z)"){
    var input = "reduce cbn (λx.(x) y) (λy.(y) z)"
    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected=List(
      Reduce,CBN,LPar,
      Lambda,TVar("x"),Dot,LPar,TVar("x"),RPar,Space,TVar("y"),RPar,
      Space,LPar,Lambda,TVar("y"),Dot,LPar,TVar("y"),RPar,Space,TVar("z"),RPar
    )
    assert(tokens.equals(expected))
    }
  test("Lexer tokeniza reduce cbn λx.(λy.(x)) (y a)") {
    val input = "reduce cbn λx.(λy.(x)) (y a)"
    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected = List(
      Reduce, CBN,
      Lambda, TVar("x"), Dot, LPar,
      Lambda, TVar("y"), Dot, LPar, TVar("x"), RPar, RPar,
      Space, LPar, TVar("y"), Space, TVar("a"), RPar
    )
    assert(tokens.equals(expected))
  }
  test("reduce cbn (λf.(f λx.(λy.(x)))) (λx.(λy.(λf.(f x y))) a b)") {
  val input = "reduce cbn (λf.(f λx.(λy.(x)))) (λx.(λy.(λf.(f x y))) a b)"
  val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
  val expected = List(
    Reduce, CBN,
    LPar, Lambda, TVar("f"), Dot, LPar, TVar("f"), Space, Lambda, TVar("x"), Dot, LPar, Lambda, TVar("y"), Dot, LPar, TVar("x"), RPar, RPar, RPar, RPar,
    Space, LPar, Lambda, TVar("x"), Dot, LPar, Lambda, TVar("y"), Dot, LPar, Lambda, TVar("f"), Dot, LPar, TVar("f"), Space, TVar("x"), Space, TVar("y"), RPar, RPar, RPar,
    Space, TVar("a"), Space, TVar("b"), RPar
  )
  assert(tokens.equals(expected))
}
  test("Lexer tokeniza reduce cbn (λx.(λy.(y)) (λx.(x x) λx.(x x)))") {
    val input = "reduce cbn (λx.(λy.(y)) (λx.(x x) λx.(x x)))"
    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected = List(
      Reduce, CBN,
      LPar, Lambda, TVar("x"), Dot,
      LPar, Lambda, TVar("y"), Dot,
      LPar, TVar("y"), RPar, RPar, Space,
      LPar, Lambda, TVar("x"), Dot, LPar, TVar("x"), Space, TVar("x"), RPar,
      Space, Lambda, TVar("x"), Dot, LPar, TVar("x"), Space, TVar("x"), RPar,
      RPar, RPar
    )
    assert(tokens.equals(expected))
  }
  test("Lexer tokeniza comandos con etiquetas y estrategias") {

    val input = "MI_EJEMPLO=(λx.(λy.(y)) (λx.(x x) λx.(x x))) calc_vars !MI_EJEMPLO reduce cbn !MI_EJEMPLO IDZ=λz.(z) calc_vars (λf.(f b)) (λx.(!IDZ) z) a reduce cbv debug (λf.(f b)) (λx.(!IDZ) z) a"

    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected = List(
      //ES UN MONTÓN ESTO
      // MI_EJEMPLO=(λx.(λy.(y)) (λx.(x x) λx.(x x)))
      TVar("MI_EJEMPLO"), Equal, LPar, Lambda, TVar("x"), Dot,
      LPar, Lambda, TVar("y"), Dot, LPar, TVar("y"), RPar, RPar,
      Space, LPar, Lambda, TVar("x"), Dot, LPar, TVar("x"), Space, TVar("x"), RPar,
      Space, Lambda, TVar("x"), Dot, LPar, TVar("x"), Space, TVar("x"), RPar, RPar, RPar,
      Space, // espacio para calcvars justo despues MI_EJEMPLO
      CalcVars, TTag("MI_EJEMPLO"),
      Reduce, CBN, TTag("MI_EJEMPLO"), Space,
      TVar("IDZ"), Equal, Lambda, TVar("z"), Dot, LPar, TVar("z"), RPar,
      Space, CalcVars, Space, LPar, Lambda, TVar("f"), Dot, LPar, TVar("f"),
      Space, TVar("b"), RPar, RPar, Space, LPar, Lambda, TVar("x"), Dot,
      LPar, TTag("IDZ"), RPar, Space, TVar("z"), RPar, Space, TVar("a"),
      Space, Reduce, Space, CBV, Space, Debug, Space,
      LPar, Lambda, TVar("f"), Dot, LPar, TVar("f"), Space, TVar("b"), RPar, RPar,
      Space, LPar, Lambda, TVar("x"), Dot, LPar, TTag("IDZ"), RPar, Space, TVar("z"), RPar, Space, TVar("a")
    )
    //assert(tokens.equals(expected))
  }

  test("reduce cbn (λx.(λx.(y x))) z") {
  val input = "reduce cbn (λx.(λx.(y x))) z"
  val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
  val expected = List(
    Reduce, CBN, LPar, Lambda, TVar("x"), Dot,
    LPar, Lambda, TVar("x"), Dot, LPar, TVar("y"), Space, TVar("x"),
    RPar, RPar, RPar, Space, TVar("z")
  )
  assert(tokens.equals(expected))
}
  test("reduce cbn λy.(λx.(y y)) a (λx.(x x) λx.(x x))") {
    val input = "reduce cbn λy.(λx.(y  y)) a (λx.(x x) λx.(x x))"
    val tokens = Lexer.tokenize(input).getOrElse(fail("Error en el lexer"))
    val expected = List(
      Reduce, CBN, Lambda, TVar("y"), Dot, LPar, Lambda, TVar("x"), Dot,
      LPar, TVar("y"), Space, TVar("y"), RPar, RPar, Space, TVar("a"), Space,
      LPar, Lambda, TVar("x"), Dot, LPar, TVar("x"), Space, TVar("x"), RPar,
      Space, Lambda, TVar("x"), Dot, LPar, TVar("x"), Space, TVar("x"), RPar, RPar
    )
    assert(tokens.equals(expected))
  }
  test("reduce cbn λy.(λx.(y y)) a (λx.(x x))") {
    val expected = Right(List(
      Reduce, CBN,
      Lambda, TVar("y"), Dot, LPar, Lambda, TVar("x"), Dot,
      LPar, TVar("y"), Space, TVar("y"), RPar, RPar,
      Space, TVar("a"),
      Space, LPar, Lambda, TVar("x"), Dot, LPar, TVar("x"), Space, TVar("x"), RPar, RPar
    ))

    println(Lexer.tokenize("reduce cbn λy.(λx.(y y)) a (λx.(x x))"))
    println(expected)
    assert(Lexer.tokenize("reduce cbn λy.(λx.(y y)) a (λx.(x x))").equals(expected))
  }