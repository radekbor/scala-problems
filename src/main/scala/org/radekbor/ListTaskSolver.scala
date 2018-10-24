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
      val (group, tail) = in span {
        _ == in.head
      }
      if (tail == Nil) List(group)
      else group :: pack(tail)
    }
  }

  def encode[T](in: List[T]): List[(Int, T)] = {
    pack(in)
      .map(t => (t.length, t.head))
  }

  def encode2[T](in: List[T]): List[Any] = {
    encode(in).map({
      case (1, x) => x
      case x => x
    })
  }

  def decode[T](in: List[(Int, T)]): List[T] = {
    in.flatMap(x => {
      List.fill(x._1)(x._2)
    })
  }

  def encodeDirect[T](in: List[T]): List[(Int, T)] = {
    val (first, rest) = in.span(_ == in.head)
    (first.length, first.head) :: {
      if (rest.isEmpty) Nil
      else encodeDirect(rest)
    }
  }

  def duplicate[T](in: List[T]): List[T] = {
    duplicate(2, in)
  }

  def duplicate[T](times: Int, in: List[T]): List[T] = {
    in flatMap (i => List.fill(times)(i))
  }

  def drop[T](times: Int, in: List[T]): List[T] = {
    in.zipWithIndex
      .filter(x => (x._2 + 1) % 3 != 0)
      .map(_._1)
  }

  def split[T](times: Int, in: List[T]) = {

    val l = in.zipWithIndex
      .groupBy(x => {
        if (x._2 < times) 0 else 1
      })
      .toList
      .sortBy(x => x._1)
      .map(x => {
        x._2.map(x2 => x2._1)
      })

    (l.head, l(1))
  }

  // TODO make it faster
  def slice[T](start: Int, end: Int, in: List[T]) = {

    val l = in.zipWithIndex
      .groupBy {
        case x if x._2 < start => 0
        case x if x._2 < end => 1
        case _ => 2
      }
      .toList
      .sortBy(x => x._1)
      .map(x => {
        x._2.map(x2 => x2._1)
      })

    l(1)
  }

  def rotate[T](x: Int, in: List[T]): List[T] = {
    val res = split(x, in)
    res._2 ++ res._1
  }

  def rotate2[T](x: Int, in: List[T]): List[T] = {
    if (x == 0) {
      in
    } else {
      val res = in.tail :+ in.head
      rotate2(x - 1, res)
    }
  }

  def removeAt[T](x: Int, in: List[T]): (List[T], T) = {

    val items = in.zipWithIndex
      .groupBy(zi => {
        if (zi._2 == x) 1 else 0
      })
      .map(zi => {
        (zi._1, zi._2.map(_._1))
      })

    val head = items.getOrElse(0, List.empty[T])
    val tail = items.getOrElse(1, List.empty[T]).head
    (head, tail)
  }

  def addAt[T](n: T, x: Int, in: List[T]): List[T] = {

    if (x == 0) {
      n :: in
    } else {
      in.head :: addAt(n, x - 1, in.tail)
    }
  }

  def range(start: Int, end: Int): List[Int] = {
    if (start == end) {
      List(end)
    } else {
      start :: range(start + 1, end)
    }
  }

  def randomSelect[T](x: Int, in: List[T]): List[T] = {

    // TODO
    List(in.head)
  }

  def getNRadom(x: Int, max: Int) = {
    List(max)
  }

  def randomPermute[T](in: List[T]): List[T] = {
    in
  }

  def combinations[T](s: Int, in: List[T]): List[List[T]] = {
    List(in)
  }

  def sortByLengthOfSublist[T](in: List[List[T]]): List[List[T]] = {
    in.sortBy(_.length)
  }

  def lsortFreq[T](in: List[List[T]]): List[List[T]] = {
    in.sortBy(_.length)
  }


}