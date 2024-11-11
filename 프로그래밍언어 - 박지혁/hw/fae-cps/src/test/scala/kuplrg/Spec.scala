package kuplrg

import Implementation.*

class Spec extends SpecBase {

  val expr1 = """
    val double = y => y * 2;
    val f = x => double(x) + 3;
    f(10)
  """
  val expr2 = """
    val double = y => y * 2;
    val add3 = y => y + 3;
    val mul5 = z => z * 5;
    double(add3(mul5(10)))
  """
  val expr3 = """
    val f = x => x + y;
    f(10)
  """
  val expr4 = """
    val f = x => x + 1;
    g(10)
  """
  val expr5 = """
    val f = x => x + 1;
    val g = y => y * 2;
    f(g(10))
  """
  val expr6 = """
    val f = x => x + y;
    val y = 1;
    f(2)
  """
  val expr7 = """
    val f = x => x + y;
    { val y = 1; f(2) } + { val y = 10; f(20) }
  """
  val expr8 = """
    val f = x => x + 1;
    val g = h => f * h;
    { val f = 1; g(2) } + { val f = 10; g(20) }
  """
  val expr9 = """
    val f = z => z * x + y;
    val g = y => f(20);
    val h = x => g(10);
    h(2)
  """
  val expr10 = """
    val f = z => z * x + y;
    val g = y => f(20);
    val h = x => g(10);
    h(2) + { val x = 3; g(5) * val y = 4; f(6) }
  """
  val expr11 = """
    val f = g => g(1);
    val add3 = x => x + 3;
    f(add3)
  """
  val expr12 = """
    val f = g => g(1);
    val add3 = x => x + 3;
    val mul5 = x => x * 5;
    f(add3) + f(mul5)
  """
  val expr13 = """
    val double = f => x => f(f(x));
    val add3 = x => x + 3;
    double(add3)(10)
  """
  val expr14 = """
    val compose = f => g => x => f(g(x));
    val add3 = x => x + 3;
    val mul5 = x => x * 5;
    compose(add3)(mul5)(10)
  """
  val expr15 = """
    val double = f => x => f(f(x));
    val compose = f => g => x => f(g(x));
    val add3 = x => x + 3;
    val mul5 = x => x * 5;
    double(compose(add3)(mul5))(10)
  """
  val expr16 = """
    val double = f => x => f(f(x));
    val compose = f => g => x => f(g(x));
    val add3 = x => x + 3;
    val mul5 = x => x * 5;
    val add3mul5 = compose(add3)(mul5);
    double(add3mul5)(10)
  """
  val expr17 = """
    val f = 42;
    val g = x => f(x + 1);
    val f = x => x * 2;
    g(10)
  """
  val expr18 = """
    val x = 10;
    val double = f => f(f(x));
    val x = 20;
    val add3 = x => x + 3;
    double(add3)
  """
  val expr19 = """
    val addN = n => x => x + n;
    addN(3)(5)
  """
  val expr20 = """
    val addN = n => x => x + n;
    val n = 10;
    addN(3)(5)
  """

  // -------------------------------------------------------------------------
  // interp (continuation with first-class functions)
  // -------------------------------------------------------------------------
  test(evalCPS("1 + 2 * 3 + 4"), "11")
  test(evalCPS("x => x + 1"), "<function>")
  test(evalCPS("(x => (y => y * 2)(x + 1))(42)"), "86")
  test(evalCPS("(x => (y => (x => x * 2)(y + 1))(x + 1))(6)"), "16")
  test(evalCPS("(f => x => y => x + y)(1)"), "<function>")
  test(evalCPS(expr1), "23")
  test(evalCPS(expr2), "106")
  testExc(evalCPS(expr3), "free identifier")
  testExc(evalCPS(expr4), "free identifier")
  test(evalCPS(expr5), "21")
  testExc(evalCPS(expr6), "free identifier")
  testExc(evalCPS(expr7), "free identifier")
  testExc(evalCPS(expr8), "invalid operation")
  testExc(evalCPS(expr9), "free identifier")
  testExc(evalCPS(expr10), "free identifier")
  test(evalCPS(expr11), "4")
  test(evalCPS(expr12), "9")
  test(evalCPS(expr13), "16")
  test(evalCPS(expr14), "53")
  test(evalCPS(expr15), "268")
  test(evalCPS(expr16), "268")
  testExc(evalCPS(expr17), "not a function")
  test(evalCPS(expr18), "16")
  test(evalCPS(expr19), "8")
  test(evalCPS(expr20), "8")

  // -------------------------------------------------------------------------
  // reduce (first-order representation of continuations)
  // -------------------------------------------------------------------------
  test(evalK("1 + 2 * 3 + 4"), "11")
  test(evalK("x => x + 1"), "<function>")
  test(evalK("(x => (y => y * 2)(x + 1))(42)"), "86")
  test(evalK("(x => (y => (x => x * 2)(y + 1))(x + 1))(6)"), "16")
  test(evalK("(f => x => y => x + y)(1)"), "<function>")
  test(evalK(expr1), "23")
  test(evalK(expr2), "106")
  testExc(evalK(expr3), "free identifier")
  testExc(evalK(expr4), "free identifier")
  test(evalK(expr5), "21")
  testExc(evalK(expr6), "free identifier")
  testExc(evalK(expr7), "free identifier")
  testExc(evalK(expr8), "invalid operation")
  testExc(evalK(expr9), "free identifier")
  testExc(evalK(expr10), "free identifier")
  test(evalK(expr11), "4")
  test(evalK(expr12), "9")
  test(evalK(expr13), "16")
  test(evalK(expr14), "53")
  test(evalK(expr15), "268")
  test(evalK(expr16), "268")
  testExc(evalK(expr17), "not a function")
  test(evalK(expr18), "16")
  test(evalK(expr19), "8")
  test(evalK(expr20), "8")

  // -------------------------------------------------------------------------
  // deep expressions
  // -------------------------------------------------------------------------
  lazy val deepExpr1 = (1 to 10000).mkString("+") // 1+2+3+...+10000
  lazy val deepExpr2 = "1" + ("+1" * 49999) // 1+1+1+...+1
  lazy val deepExpr3 = "1" + ("+2+(-1)" * 49999) // 1+2+(-1)+...+2+(-1)
  test(evalK(deepExpr1), "50005000", weight = 2)
  test(evalK(deepExpr2), "50000", weight = 4)
  test(evalK(deepExpr3), "50000", weight = 4)

  /* Write your own tests */
}
