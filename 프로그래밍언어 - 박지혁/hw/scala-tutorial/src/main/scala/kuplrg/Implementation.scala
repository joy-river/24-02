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

  def generate(init: Int, f: Int => Int, n: Int): List[Int] = {
    if n == 0 then Nil else {
      init :: generate(f(init), f, n - 1)
    }
  }

  def incKey(map: Map[String, Int], key: String): Map[String, Int] = {
    val x = map.get(key)
    if x == None then map else {
      map + (key -> (x.getOrElse(0) + 1)) 
    }
  }

  def validSums(
    l: List[Int],
    r: List[Int],
    f: (Int, Int) => Boolean,
  ): Set[Int] = {

    l.flatMap (x => 
      r.filter (y => f(x, y))
       .map(y => x + y)
    ).toSet
  }

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def count(t: Tree, x: Int): Int = {
    t match {
      case Leaf(n) => if n == x then 1 else 0
      case Branch(l, n, r) => if n == x then 1 + count(l, x) + count(r, x) else count(l, x) + count(r, x)
    }
  }

  def heightOf(t: Tree): Int = {
    t match {
      case Leaf(n) => 0
      case Branch(l, n, r) => {
        val hl = heightOf(l) + 1
        val hr = heightOf(r) + 1
        if hl > hr then hl else hr
      }
    }
  }

  def min(t: Tree): Int = {
    t match {
      case Leaf(n) => n
      case Branch(l, n, r) => {
        val ml = min(l)
        val mr = min(r)
        if n <= ml && n <= mr then n else
          if ml <= n && ml <= mr then ml else mr
      }
    }
  }

  def sumLeaves(t: Tree): Int = {
    t match {
      case Leaf(n) => n
      case Branch(l, n , r) => {
        sumLeaves(l) + sumLeaves(r)
      }
    }
  }

  def inorder(t: Tree): List[Int] = {
    t match {
      case Leaf(n) => List(n)
      case Branch(l, n, r) => inorder(l) ::: List(n) ::: inorder(r)
    }
  }

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def isLiteral(expr: BE): Boolean = {
    expr match {
      case Literal(b) => true 
      case _ => false
    }
  }

  def countImply(expr: BE): Int = {
    expr match {
      case Imply(l, r) => 1 + countImply(l) + countImply(r)
      case And(l, r) => countImply(l) + countImply(r)
      case Or(l, r) => countImply(l) + countImply(r)
      case Not(b) => countImply(b)
      case _ => 0
    }
  }

  def literals(expr: BE): List[Boolean] ={
    expr match {
      case Literal(b) => List(b)
      case Imply(l, r) => literals(l) ::: literals(r)
      case And(l, r) => literals(l) ::: literals(r)
      case Or(l, r) => literals(l) ::: literals(r)
      case Not(b) => literals(b)
      case _ => Nil
    }
  }

  def getString(expr: BE): String = {
    expr match {
      case Literal(b) => if b then "#t" else "#f"
      case Imply(l, r) => "(" + getString(l) + " => " + getString(r) + ")"
      case And(l, r) => "(" + getString(l) + " & " + getString(r) + ")"
      case Or(l, r) => "(" + getString(l) + " | " + getString(r) + ")"
      case Not(b) => "!" + getString(b)
      case _ => ""
    }
  } 

  def eval(expr: BE): Boolean = {
    expr match {
      case Literal(b) => if b then true else false
      case Not(b) => if eval(b) then false else true
      case And(l, r) => if eval(l) && eval(r) then true else false
      case Or(l, r) => if eval(l) || eval(r) then true else false
      case Imply(l, r) => if !eval(l) || eval(r) then true else false
    }
  }
}
