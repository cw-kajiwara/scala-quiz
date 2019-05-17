package com.chatwork.quiz.collection

import com.chatwork.quiz.{ MyNone, MyOption, MySome }

sealed trait MyList[+A] {

  // Easy
  def length: Int =
    foldLeft(0)((acc, _) => acc + 1)

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def loop(ls: MyList[A], z: B, f: (B, A) => B): B = ls match {
      case MyCons(head, tail) => loop(tail, f(z, head), f)
      case MyNil => z
    }
    loop(this, z, f)
  }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B =
    foldLeft(identity[B](_))((b, a) => b compose f.curried(a))(z)

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] =
    MyCons(b, this)

  // Normal
  def reverse: MyList[A] =
    foldLeft(MyList[A]())((ls, a) => a :: ls)

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] =
    foldRight(b)(_ :: _)
  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] =
    foldRight(MyList[B]())((a, ls) => f(a) :: ls)

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] = this match {
    case MyCons(head, tail) => f(head) ++ tail.flatMap(f)
    case MyNil => MyNil
  }

  // Normal
  def filter(f: A => Boolean): MyList[A] =
    foldRight(MyList[A]())((a, ls) => if (f(a)) a :: ls else ls)

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyList[A] =
    // 思いつきませんでした。
    filter(f)

  // Normal
  def find(f: A => Boolean): MyOption[A] = this match {
    case MyCons(head, tail) if f(head) => MySome(head)
    case MyCons(head, tail) => tail.find(f)
    case MyNil => MyNone
  }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = {
    @scala.annotation.tailrec
    def loop(a: MyList[A], b: MyList[B]): Boolean = {
      (a, b) match {
        case (MyCons(h1, t1), MyCons(h2, t2)) => if (h1 == h2) loop(t1, t2) else false
        case (MyCons(_, _), MyNil) => true
        case (MyNil, MyCons(_, _)) => false
        case (MyNil, MyNil) => true
      }
    }
    loop(this, prefix)
  }

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] =
    as.foldRight(empty[A])((a, ls) => MyCons(a, ls))

}
