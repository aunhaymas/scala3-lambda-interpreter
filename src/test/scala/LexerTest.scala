//https://docs.scala-lang.org/scala3/book/tools-sbt.html#using-sbt-with-scalatest

import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite:
  test("Lexer tokenize λx.x") {
    val input = "λx.x"
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



