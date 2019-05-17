package com.chatwork.quiz

/**
 * 値が存在する・しないの両状態を表すオブジェクト。いわゆるMaybeモナド。
 *
 * @tparam A 値の型
 */
sealed trait MyOption[+A] {

  /**
   * 格納された値を返す。
   *
   * @return 値
   * @throws 値が存在しない場合 NoSuchElementException をスローする
   */
  def get: A = this match {
    case MySome(x) => x
    case _ => throw new java.util.NoSuchElementException
  }

  /**
   * 値がないかどうかを返す。
   *
   * @return 値が存在しない場合はtrue。
   */
  def isEmpty: Boolean = this match {
    case MySome(_) => false
    case _ => true
  }

  /**
   * 値が存在する場合に、値の変換を行う。
   *
   * @param f 値を変換するための関数
   * @tparam B 新しい型
   * @return 新しい [[MyOption]]
   */
  def map[B](f: A => B): MyOption[B] =
    flatMap(a => MySome(f(a)))

  /**
   * 値が存在する場合に、値の変換を行う。
   *
   * @param f 値を変数するための関数
   * @tparam B 新しい型
   * @return 新しい [[MyOption]]
   */
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MySome(x) => f(x)
    case MyNone => MyNone
  }

  /**
   * 値が存在する場合に、値をフィルタリングする。
   *
   * @param f フィルターのための述語関数
   * @return 新しい [[MyOption]]
   */
  def filter(f: A => Boolean): MyOption[A] =
    flatMap(a => if (f(a)) MySome(a) else MyNone)

  /**
   * 格納された値を返す。値がない場合は指定された値を返す。
   *
   * @param elseValue 値がない場合に返す値
   * @tparam B 新しい要素型
   * @return 値
   */
  def getOrElse[B >: A](elseValue: B): B =
    if (isEmpty) elseValue else get

  /**
   * 値が存在しない場合に、指定した式を評価し返す。
   *
   * @param elseValue 値が存在しない場合に返す式
   * @tparam B 新しい要素型
   * @return elseValueを評価した値
   */
  def orElse[B >: A](elseValue: => MyOption[B]): MyOption[B] =
    map(MySome(_)) getOrElse elseValue

}

/**
 * 値が存在ない場合の[[MyOption]]。
 */
case object MyNone extends MyOption[Nothing]

/**
 * 値が存在する場合の[[MyOption]]。
 *
 * @param value 値
 * @tparam A 値の型
 */
case class MySome[+A](value: A) extends MyOption[A]

/**
 * [[MyOption]]のコンパニオンオブジェクト。
 */
object MyOption {

  /**
   * ファクトリメソッド。
   *
   * @param value 値
   * @tparam A 値の型
   * @return [[MyOption]]
   */
  def apply[A](value: A): MyOption[A] = MySome(value)

}
