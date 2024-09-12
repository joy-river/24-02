package kuplrg

object Implementation extends Template {

  // ---------------------------------------------------------------------------
  // Basic Data Types
  // ---------------------------------------------------------------------------
  def isEvenPair(x: Int, y: Int): Boolean = {
    val sum = x + y
    if sum % 2 == 0 then true else false
  }

  def validString(str: String, lower: Int, upper: Int): Boolean = {
    val len = str.length
    if len >= lower && len <= upper then true else false  
  }

  // ---------------------------------------------------------------------------
  // Functions
  // ---------------------------------------------------------------------------
  def factorial(n: Int): Int = {
    if n <= 1 then 1 else n * factorial(n - 1)
  }

  def magic(x: Int): Int => Int = {
    (y: Int) => {
      if y % x  == 0 then y / x else (x + 1) * y + (x - y % x)
    }
  }

  def applyK(f: Int => Int, k: Int): Int => Int = {
    (x: Int) => {
      if k == 0 then x else applyK(f, k - 1)(f(x))
    }
  }

  // ---------------------------------------------------------------------------
  // Collections
  // ---------------------------------------------------------------------------
  def productPos(l: List[Int]): Int = {
    val pl = l.filter(_ > 0)
    pl.foldLeft(1)(_ * _)
  }

  def merge(l: List[Int]): List[Int] = {
    l match {
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: y :: xs => (x + y) :: merge(xs) 
    }
  }

  def generate(init: Int, f: Int => Int, n: Int): List[Int] = ???

  def incKey(map: Map[String, Int], key: String): Map[String, Int] = ???

  def validSums(
    l: List[Int],
    r: List[Int],
    f: (Int, Int) => Boolean,
  ): Set[Int] = ???

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def count(t: Tree, x: Int): Int = ???

  def heightOf(t: Tree): Int = ???

  def min(t: Tree): Int = ???

  def sumLeaves(t: Tree): Int = ???

  def inorder(t: Tree): List[Int] = ???

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def isLiteral(expr: BE): Boolean = ???

  def countImply(expr: BE): Int = ???

  def literals(expr: BE): List[Boolean] = ???

  def getString(expr: BE): String = ???

  def eval(expr: BE): Boolean = ???
}
