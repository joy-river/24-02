package kuplrg

import Implementation.*

class Spec extends SpecBase {
  // tests for `isEvenPair`
  test(isEvenPair(0, 2), true)
  test(isEvenPair(2, 3), false)
  test(isEvenPair(-3, 5), true)
  test(isEvenPair(-4, -2), true)
  test(isEvenPair(1, -2), false)

  // tests for `validString`
  test(validString("Hello", 2, 5), true)
  test(validString("COSE212", 3, 4), false)
  test(validString("Scala", 4, 6), true)
  test(validString("Tutorial", 1, 3), false)
  test(validString("Test", 4, 6), true)

  // tests for `factorial`
  test(factorial(0), 1)
  test(factorial(2), 2)
  test(factorial(5), 120)
  test(factorial(7), 5040)
  test(factorial(10), 3628800)

  // tests for `magic`
  test(magic(2)(7), 22)
  test(magic(3)(42), 14)
  test(magic(5)(3), 20)
  test(magic(7)(21), 3)
  test(magic(10)(25), 280)

  // tests for `applyK`
  test(applyK(_ + 3, 2)(1), 7)
  test(applyK(_ + 2, 5)(7), 17)
  test(applyK(_ * 2, 10)(1), 1024)
  test(applyK(_ * 10, 3)(42), 42000)
  test(applyK(magic(2), 6)(7), 26)

  // tests for `productPos`
  test(productPos(List(1, 2, 3, 4, 5)), 120)
  test(productPos(List(1, 2, -3, -4, -5)), 2)
  test(productPos(List(1, -2, 3, -4, 5)), 15)
  test(productPos(List(-1, 2, 3, 4, -5)), 24)
  test(productPos(List(-1, -2, -3, -4, -5)), 1)

  // tests for `merge`
  test(merge(Nil), Nil)
  test(merge(List(1)), List(1))
  test(merge(List(1, 2)), List(3))
  test(merge(List(1, 2, 3, 4, 5)), List(3, 7, 5))
  test(merge(List(1, 2, 3, 4, 5, 6)), List(3, 7, 11))

  // tests for `generate`
  test(generate(1, _ + 2, 0), Nil)
  test(generate(7, _ + 2, 6), List(7, 9, 11, 13, 15, 17))
  test(generate(1, _ * 2, 5), List(1, 2, 4, 8, 16))
  test(generate(42, _ * 10, 4), List(42, 420, 4200, 42000))
  test(generate(7, magic(2), 7), List(7, 22, 11, 34, 17, 52, 26))

  // tests for `incKey`
  val m: Map[String, Int] = Map("A" -> 1, "B" -> 2, "C" -> 3)
  test(incKey(m, "A"), Map("A" -> 2, "B" -> 2, "C" -> 3))
  test(incKey(m, "B"), Map("A" -> 1, "B" -> 3, "C" -> 3))
  test(incKey(m, "C"), Map("A" -> 1, "B" -> 2, "C" -> 4))
  test(incKey(m, "D"), Map("A" -> 1, "B" -> 2, "C" -> 3))
  test(incKey(incKey(m, "A"), "B"), Map("A" -> 2, "B" -> 3, "C" -> 3))

  // tests for `validSums`
  test(validSums(List(1, 2), List(3, 4, 5), isEvenPair), Set(4, 6))
  test(validSums(List(1, 2), List(3, 4, 5), _ + _ == 7), Set(7))
  test(validSums(List(1, 2), List(3, 4, 5), !isEvenPair(_, _)), Set(5, 7))
  test(validSums(List(1, 2), List(3, 4, 5), (_, _) => true), Set(4, 5, 6, 7))
  test(validSums(List(1, 2), List(3, 4, 5), _ * _ > 3), Set(5, 6, 7))

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  //  8
  val tree1: Tree = Leaf(8)

  //    1
  //   / \
  //  2   3
  val tree2: Tree = Branch(Leaf(2), 1, Leaf(3))

  //    4
  //   / \
  //  5   2
  //     / \
  //    8   3
  val tree3: Tree = Branch(Leaf(5), 4, Branch(Leaf(8), 2, Leaf(3)))

  //    7
  //   / \
  //  2   3
  //     / \
  //    5   1
  //   / \
  //  1   8
  val tree4: Tree =
    Branch(Leaf(2), 7, Branch(Branch(Leaf(1), 5, Leaf(8)), 3, Leaf(1)))

  //      42
  //     /  \
  //    7    7
  //   / \   / \
  //  7   9 3   4
  val tree5: Tree =
    Branch(Branch(Leaf(7), 7, Leaf(9)), 42, Branch(Leaf(3), 7, Leaf(4)))

  // tests for `count`
  test(count(tree1, 8), 1)
  test(count(tree2, 4), 0)
  test(count(tree3, 2), 1)
  test(count(tree4, 1), 2)
  test(count(tree5, 7), 3)

  // tests for `heightOf`
  test(heightOf(tree1), 0)
  test(heightOf(tree2), 1)
  test(heightOf(tree3), 2)
  test(heightOf(tree4), 3)
  test(heightOf(tree5), 2)

  // tests for `min`
  test(min(tree1), 8)
  test(min(tree2), 1)
  test(min(tree3), 2)
  test(min(tree4), 1)
  test(min(tree5), 3)

  // tests for `sumLeaves`
  test(sumLeaves(tree1), 8)
  test(sumLeaves(tree2), 5)
  test(sumLeaves(tree3), 16)
  test(sumLeaves(tree4), 12)
  test(sumLeaves(tree5), 23)

  // tests for `inorder`
  test(inorder(tree1), List(8))
  test(inorder(tree2), List(2, 1, 3))
  test(inorder(tree3), List(5, 4, 8, 2, 3))
  test(inorder(tree4), List(2, 7, 1, 5, 8, 3, 1))
  test(inorder(tree5), List(7, 7, 9, 42, 3, 7, 4))

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  val T = Literal(true)
  val F = Literal(false)

  // #t
  val be1: BE = T

  // (#t => #f)
  val be2: BE = Imply(T, F)

  // (!(#t | #f) & !(#f | #t))
  val be3: BE = And(Not(Or(T, F)), Not(Or(F, T)))

  // ((#t & (#t => #f)) | (#f => (#t => #f)))
  val be4: BE = Or(And(T, Imply(T, F)), Imply(F, Imply(T, F)))

  // (!(#t => (#t & #f)) & (!#f => (#f | #t)))
  val be5: BE = And(Not(Imply(T, And(T, F))), Imply(Not(F), Or(F, T)))

  // tests for `countLiterals`
  test(isLiteral(be1), true)
  test(isLiteral(be2), false)
  test(isLiteral(be3), false)
  test(isLiteral(be4), false)
  test(isLiteral(be5), false)

  // tests for `countImply`
  test(countImply(be1), 0)
  test(countImply(be2), 1)
  test(countImply(be3), 0)
  test(countImply(be4), 3)
  test(countImply(be5), 2)

  // tests for `literals`
  test(literals(be1), List(true))
  test(literals(be2), List(true, false))
  test(literals(be3), List(true, false, false, true))
  test(literals(be4), List(true, true, false, false, true, false))
  test(literals(be5), List(true, true, false, false, false, true))

  // tests for `getString`
  test(getString(be1), "#t")
  test(getString(be2), "(#t => #f)")
  test(getString(be3), "(!(#t | #f) & !(#f | #t))")
  test(getString(be4), "((#t & (#t => #f)) | (#f => (#t => #f)))")
  test(getString(be5), "(!(#t => (#t & #f)) & (!#f => (#f | #t)))")

  // tests for `eval`
  test(eval(be1), true)
  test(eval(be2), false)
  test(eval(be3), false)
  test(eval(be4), true)
  test(eval(be5), true)

  /* Write your own tests */
}
