package kuplrg

trait Template {

  // ---------------------------------------------------------------------------
  // Basic Data Types
  // ---------------------------------------------------------------------------
  def isEvenPair(x: Int, y: Int): Boolean

  def validString(str: String, lower: Int, upper: Int): Boolean

  // ---------------------------------------------------------------------------
  // Functions
  // ---------------------------------------------------------------------------
  def factorial(n: Int): Int

  def magic(x: Int): Int => Int

  def applyK(f: Int => Int, k: Int): Int => Int

  // ---------------------------------------------------------------------------
  // Collections
  // ---------------------------------------------------------------------------
  def productPos(l: List[Int]): Int

  def merge(l: List[Int]): List[Int]

  def generate(init: Int, f: Int => Int, n: Int): List[Int]

  def incKey(map: Map[String, Int], key: String): Map[String, Int]

  def validSums(
    l: List[Int],
    r: List[Int],
    f: (Int, Int) => Boolean,
  ): Set[Int]

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  enum Tree:
    case Leaf(value: Int)
    case Branch(left: Tree, value: Int, right: Tree)

  def count(t: Tree, x: Int): Int

  def heightOf(t: Tree): Int

  def min(t: Tree): Int

  def sumLeaves(t: Tree): Int

  def inorder(t: Tree): List[Int]

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  enum BE:
    case Literal(value: Boolean)
    case And(left: BE, right: BE)
    case Or(left: BE, right: BE)
    case Imply(left: BE, right: BE)
    case Not(expr: BE)

  def isLiteral(expr: BE): Boolean

  def countImply(expr: BE): Int

  def literals(expr: BE): List[Boolean]

  def getString(expr: BE): String

  def eval(expr: BE): Boolean
}
