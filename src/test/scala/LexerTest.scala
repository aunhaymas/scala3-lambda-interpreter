//https://docs.scala-lang.org/scala3/book/tools-sbt.html#using-sbt-with-scalatest

import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite:
  test("Lexer tokenize λx.x") {
    val input = "λx.x"
    val tokens: List[Token] = Lexer.tokenize(input)
    val expected: List[Token] = List(Lambda, TVar("x"), Dot, TVar("x"))
    assert(tokens.equals(expected))
  }
  test("Lexer tokenize λx.x con espacios extra") {
    val input = "    λx.x"
    val tokens: List[Token] = Lexer.tokenize(input)
    val expected: List[Token] = List(Lambda, TVar("x"), Dot, TVar("x"))
    assert(tokens.equals(expected))
  }
  test ("Lexer tokeniza reduce cbn λx.(λy.(y) x)"){
    val input = "reduce cbn λx.(λy.(y) x)"
    val tokens = Lexer.tokenize(input)
    val expected:List[Token] = (
      List(Reduce,Space,CBN,Space,
        Lambda,TVar("x"),Dot,LPar,
        Lambda,TVar("y"),Dot,LPar,TVar("y"),RPar,Space,TVar("x"),RPar))
    assert(tokens.equals(expected))
    }
  test("Lexer tokeniza reduce cbn λx.(λy.(y) x con espacios extra)") {
    val input = "reduce  cbn λx .(λy. (y)     x)"
    val tokens = Lexer.tokenize(input)
    val expected: List[Token] = (
      List(Reduce, Space, CBN, Space,
        Lambda, TVar("x"), Dot, LPar,
        Lambda, TVar("y"), Dot, LPar, TVar("y"), RPar, Space, TVar("x"), RPar))
    assert(tokens.equals(expected))
  }
  test("Lexer tokeniza reduce cbn (λx.(λy.(x))) y ") {
    val input = "reduce cbn (λx. (λy. (x))) y"
    val tokens = Lexer.tokenize(input)
    val expected = List(
      Reduce, Space, CBN, Space, LPar, Lambda, TVar("x"), Dot,
      LPar, Lambda, TVar("y"), Dot, LPar, TVar("x"), RPar, RPar, RPar, Space, TVar("y")
    )
    assert(expected.equals(tokens))
    }
  test("reduce cbn (λx.(x) y) (λy.(y) z)"){
    val tokens = Lexer.tokenize("reduce cbn (λx.(x) y) (λy.(y) z)")
    val expected=List(
      Reduce,Space,CBN,Space,LPar,
      Lambda,TVar("x"),Dot,LPar,TVar("x"),RPar,Space,TVar("y"),RPar,
      Space,LPar,Lambda,TVar("y"),Dot,LPar,TVar("y"),RPar,Space,TVar("z"),RPar
    )
    assert(tokens.equals(expected))
    }
  test("Lexer tokeniza reduce cbn λx.(λy.(x)) (y a)") {
    val input = "reduce cbn λx.(λy.(x)) (y a)"
    val tokens = Lexer.tokenize(input)
    val expected = List(
      Reduce, Space, CBN, Space,
      Lambda, TVar("x"), Dot, LPar,
      Lambda, TVar("y"), Dot, LPar, TVar("x"), RPar, RPar,
      Space, LPar, TVar("y"), Space, TVar("a"), RPar
    )
    assert(tokens.equals(expected))
  }
  test("reduce cbn (λf.(f λx.(λy.(x)))) (λx.(λy.(λf.(f x y))) a b)") {
  val input = "reduce cbn (λf.(f λx.(λy.(x)))) (λx.(λy.(λf.(f x y))) a b)"
  val tokens = Lexer.tokenize(input)
  val expected = List(
    Reduce, Space, CBN, Space,
    LPar, Lambda, TVar("f"), Dot, LPar, TVar("f"), Space, Lambda, TVar("x"), Dot, LPar, Lambda, TVar("y"), Dot, LPar, TVar("x"), RPar, RPar, RPar, RPar,
    Space, LPar, Lambda, TVar("x"), Dot, LPar, Lambda, TVar("y"), Dot, LPar, Lambda, TVar("f"), Dot, LPar, TVar("f"), Space, TVar("x"), Space, TVar("y"), RPar, RPar, RPar,
    Space, TVar("a"), Space, TVar("b"), RPar
  )
  assert(tokens.equals(expected))
}


