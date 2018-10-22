package org.radekbor

class ListTaskSolver {

  def findLast[T](list: List[T]): T = {
    list match {
      case x :: Nil => x
      case _ :: tail => findLast(tail)
    }
  }

  def findPenultimate[T](list: List[T]): T = {
    list match {
      case (x: T) :: (_: T) :: Nil => x
      case _ :: tail => findPenultimate(tail)
    }
  }

  def nth[T](list: List[T], n: Int): T = {
    def nthInner(rest: List[T], current: Int = 0): T = {
      if (current == n) {
        rest.head
      } else {
        nthInner(rest.tail, current + 1)
      }
    }

    nthInner(list)
  }

  def length[T](list: List[T]): Int = {
    list match {
      case x :: Nil => 1
      case x :: (tail: List[T]) => 1 + length(tail)
    }
  }

  def revers[T](list: List[T]): List[T] = {
    list match {
      case x :: Nil => List(x)
      case _ => revers(list.tail) ::: List(list.head)
    }
  }

  def isPalindrome[T](list: List[T]): Boolean = {
    revers(list) == list
  }

  def flatten(list: List[Any]): List[Any] = list flatMap {
    case ls: List[_] => flatten(ls)
    case e => List(e)
  }

  def compress[T](in: List[T]): List[T] = {

    in.sliding(2)
      .map({
        case x :: y :: Nil if x == y => None: Option[T]
        case x :: y :: Nil if x != y => Option(x)
      })
      .filter(_.isDefined)
      .map(i => i.get)
      .toList

  }

  def pack[T](in: List[T]): List[List[T]] = {
    if (in.isEmpty) List(List())
    else {
      val (group, tail) = in span { _ == in.head }
      if (tail == Nil) List(group)
      else group :: pack(tail)
    }
  }

  def encode[T](in: List[T]) : List[(Int, T)] = {
    pack(in)
      .map(t => (t.length, t.head))
  }


}