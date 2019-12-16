// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.essentials
import java.time.Instant

object types {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // List all values of the type `Boolean`.
  //
  val BoolValues: List[Boolean] = List(true, false)

  //
  // EXERCISE 2
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
  val EitherUnitBoolValues: List[Either[Unit, Boolean]] = List(Left(()), Right(false), Right(true))

  //
  // EXERCISE 3
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: List[(Boolean, Boolean)] =
    ???

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: List[Either[Either[Unit, Unit], Unit]] =
    ???

  //
  // EXERCISE 5
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person = Pp
    case class Pp(age: Int, name: String)

  //
  // EXERCISE 6
  //
  // Prove that `A * 1` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to1[A](t: (A, Unit)): A = t._1
  def from1[A](a: A): (A, Unit) = (a, ())


  sealed trait Idd
  case class Robot(id: Int) extends Idd
  case class Person2(n: String) extends Idd

  //
  // EXERCISE 7
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or a person (a name).
  //
  type Identifier = Idd

  //
  // EXERCISE 8
  //
  // Prove that `A + 0` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to2[A](t: Either[A, Nothing]): A = ???
  def from2[A](a: A): Either[A, Nothing] = ???

  case class CC(num: Int, expDate: Int, sec: Int)
  //
  // EXERCISE 9
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //
  type CreditCard = CC

  //
  // EXERCISE 10
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  type PaymentMethod = PM

  sealed trait PM
  case class PC() extends PM
  case class PB() extends PM
  case class Crypto() extends PM
  //
  // EXERCISE 11
  //
  // Create either a sum type or a product type (as appropriate) to represent an
  // employee at a company, which has a title, salary, name, employment date.
  //
  type Employee = Employy

  case class Employy(name: String, title: String, salary: Int, employmentDate: Int)
  //
  // EXERCISE 12
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // piece on a chess board, which could be a pawn, rook, bishop, knight,
  // queen, or king.
  //
  type ChessPiece = ???

  //
  // EXERCISE 13
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //
  type GameWorld = ???



}

object functions {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // Convert the following non-function into a function.
  //
  import scala.util.Try
  def parseInt1(s: String): Int = s.toInt
  def parseInt2(s: String): Option[Int] = Try(s.toInt).toOption

  //
  // EXERCISE 2
  //
  // Convert the following non-function into a function.
  //
  def arrayUpdate1[A](arr: Array[A], i: Int, f: A => A): Unit =
    arr.updated(i, f(arr(i)))
  def arrayUpdate2[A](arr: Array[A], i: Int, f: A => A): Option[Array[A]] =
    if (0 <= i && i < arr.size) {
      val (arr1, arr2) = arr.splitAt(i)
      Some((arr1.init :+ f(arr(i))) ++ arr2)
    } else None

  //
  // EXERCISE 3
  //
  // Convert the following non-function into a function.
  //
  def divide1(a: Int, b: Int): Int = a / b
  def divide2(a: Int, b: Int): Option[Int] = Try(a/b).toOption

  //
  // EXERCISE 4
  //
  // Convert the following non-function into a function.
  //
  var id = 0
  def freshId1(): Int = {
    val newId = id
    id += 1
    newId
  }
  def freshId2(id: Int): (Int, Int) = (id, id + 1)

  //
  // EXERCISE 5
  //
  // Convert the following non-function into a function.
  //
  import java.time.LocalDateTime
  def afterOneHour1: LocalDateTime = LocalDateTime.now.plusHours(1)
  def afterOneHour2(time: LocalDateTime): LocalDateTime = time.plusHours(1)

  //
  // EXERCISE 6
  //
  // Convert the following non-function into function.
  //
  def head1[A](as: List[A]): A = {
    if (as.length == 0) println("Oh no, it's impossible!!!")
    as.head
  }

  def head2[A](as: List[A]): Option[A] = as match {
    case Nil => None
    case h :: t => Some(h)
  }

  //
  // EXERCISE 7
  //
  // Convert the following non-function into a function.
  //
  trait Account
  trait Processor {
    def charge(account: Account, amount: Double): Unit
  }
  case class Coffee() {
    val price = 3.14
  }

  def buyCoffee1(processor: Processor, account: Account): Coffee = {
    val coffee = Coffee()
    processor.charge(account, coffee.price)
    coffee
  }
  final case class Charge(amount: Double, account: Account)
  def buyCoffee2(account: Account): (Coffee, Charge) = (Coffee(), Charge(Coffee().price, account))

  //
  // EXERCISE 8
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def printLine(line: String): Unit = Unit

  //
  // EXERCISE 9
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def readLine: String = ""

  //
  // EXERCISE 10
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def systemExit(code: Int): Unit = Unit

  //
  // EXERCISE 11
  //
  // Rewrite the following non-function `printer1` into a pure function, which
  // could be used by pure or impure code.
  //
  def printer1(): Unit = {
    println("Welcome to the help page!")
    println("To list commands, type `commands`.")
    println("For help on a command, type `help <command>`")
    println("To exit the help page, type `exit`.")
  }
  def printer2[A](println: String => A, combine: (A, A) => A): A =
    List(
      "Welcome to the help page!",
      "To list commands, type `commands`.",
      "For help on a command, type `help <command>`",
      "To exit the help page, type `exit`.")
      .map(println)
      .reduce(combine)

  //
  // EXERCISE 12
  //
  // Create a purely-functional drawing library that is equivalent in
  // expressive power to the following procedural library.
  //
  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): List[List[Boolean]]
  }

  def draw1(size: Int): Draw = new Draw {
    val canvas = Array.fill(size, size)(false)
    var x = 0
    var y = 0

    def goLeft(): Unit = x -= 1
    def goRight(): Unit = x += 1
    def goUp(): Unit = y += 1
    def goDown(): Unit = y -= 1
    def draw(): Unit = {
      def wrap(x: Int): Int =
        if (x < 0) (size - 1) + ((x + 1) % size) else x % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }
    def finish(): List[List[Boolean]] =
      canvas.map(_.toList).toList
  }

  case class Draw2 private (canvas: Array[Array[Boolean]], x: Int, y: Int)

  object Draw2 {
    def apply(size: Int): Draw2 = Draw2(Array.fill(size, size)(false), 0, 0)

    def goLeft(draw: Draw2) = draw.copy(x = draw.x - 1)
    def goRight(draw: Draw2) = draw.copy(x = draw.x + 1)
    def goUp(draw: Draw2) = draw.copy(y = draw.y + 1)
    def goDown(draw: Draw2) = draw.copy(y = draw.y - 1)

    def draw(draw: Draw2): Option[Draw2] = {
      val canvas = draw.canvas
      def wrap(x: Int): Int = if (x < 0) (canvas.size - 1) + ((x + 1) % canvas.size) else x % canvas.size

      val x2 = wrap(draw.x)
      val y2 = wrap(draw.y)

      Try(canvas.updated(x2, canvas(x2).updated(y2, true)))
        .map(c => draw.copy(c))
        .toOption
    }

    def finish(draw: Draw2): List[List[Boolean]] = draw.canvas.map(_.toList).toList
  }

}

object higher_order {
  case class Parser[+E, +A](
    run: String => Either[E, (String, A)])

  def fail[E](e: E): Parser[E, Nothing] =
    Parser(input => Left(e))

  def point[A](a: => A): Parser[Nothing, A] =
    Parser(input => Right((input, a)))

  def char[E](e: E): Parser[E, Char] =
    Parser(input =>
      if (input.length == 0) Left(e)
      else Right((input.drop(1), input.charAt(0))))

  //
  // EXERCISE 1
  //
  // Implement the following higher-order function.
  //
  def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) = a => (f(a), g(a))

  //
  // EXERCISE 2
  //
  // Implement the following higher-order function.
  //
  def cross[A, B, C, D](f: A => B, g: C => D): (A, C) => (B, D) = (a, c) => (f(a), g(c))

  //
  // EXERCISE 3
  //
  // Implement the following higher-order function.
  //
  def either[A, B, C](f: A => B, g: C => B): Either[A, C] => B = e => e match {
    case Left(a) => f(a)
    case Right(c) => g(c)
  }

  //
  // EXERCISE 4
  //
  // Implement the following higher-order function.
  //
  def choice[A, B, C, D](f: A => B, g: C => D): Either[A, C] => Either[B, D] = e => e match {
    case Left(a) => Left(f(a))
    case Right(c) => Right(g(c))
  }

  //
  // EXERCISE 5
  //
  // Implement the following higer-order function.
  //
  def compose[A, B, C](f: B => C, g: A => B): A => C = g.andThen(f)

  //
  // EXERCISE 6
  //
  // Implement the following higher-order function.
  //
  def alt[E1, E2, A, B](l: Parser[E1, A], r: Parser[E2, B]): Parser[E2, Either[A, B]] = {
    val f: String => Either[E2, (String, Either[A, B])] = s => (l.run(s), r.run(s)) match {
        case (Left(_), Right(ll)) => Right((s, Right(ll._2)))
        case (Right(rr), _) => Right((s, Left(rr._2)))
        case (Left(_), Left(e2)) => Left(e2)
      }
    Parser(f)
  }
}

object poly_functions {
  //
  // EXERCISE 1
  //
  // Create a polymorphic function of two type parameters `A` and `B` called
  // `snd` that returns the second element out of any pair of `A` and `B`.
  //
  object snd {
    def apply[A, B](ab: (A, B)) = ab._2
  }
  // snd(1, "foo") // "foo"

  //
  // EXERCISE 2
  //
  // Create a polymorphic function called `repeat` that can take any
  // function `A => A`, and apply it repeatedly to a starting value
  // `A` the specified number of times.
  //
  object repeat {
    def repeat[A](n: Int)(a: A, f: A => A): A = if (n == 0) a else repeat(n - 1)(f(a), f)
  }
  // repeat[Int](100)(0, _ + 1) // 100
  // repeat[String](10)("", _ + "*") // "**********"

  //
  // EXERCISE 3
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample1[A, B](a: A, b: B): Either[A, B] = ???
  val countExample1Answer = ???

  //
  // EXERCISE 4
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample2[A, B](f: A => B, g: A => B, a: A): B = f(a)
  val countExample2Answer = ???

  //
  // EXERCISE 5
  //
  // Implement the function `groupBy`.
  //
  val Data =
    "poweroutage;2018-09-20;level=20" :: Nil
  val By: String => String =
    (data: String) => data.split(";")(1)
  val Reducer: (String, List[String]) => String =
    (date, events) =>
      "On date " +
        date + ", there were " +
        events.length + " power outages"
  val Expected =
    Map("2018-09-20" ->
      "On date 2018-09-20, there were 1 power outages")

  def groupBy1(
    l: List[String],
    by: String => String)(
      reducer: (String, List[String]) => String):
      Map[String, String] = l.groupBy(by).map(s => (s._1, reducer(s)))


  // groupBy1(Data, By)(Reducer) == Expected

  //
  // EXERCISE 6
  //
  // Make the function `groupBy1` as polymorphic as possible and implement
  // the polymorphic function. Compare to the original.
  //
  object groupBy2 {
    def apply[A, B, C](l: List[A], by: A => B)(reducer: (B, List[A]) => C): Map[B, C] =
      l.groupBy(by).map{case (k, vs) => (k, reducer(k, vs))}
  }
}

object higher_kinded {
  type ?? = Nothing
  type ???[A] = Nothing
  type ????[A, B] = Nothing
  type ?????[F[_]] = Nothing

  trait `* => *`[F[_]]
  trait `[*, *] => *`[F[_, _]]
  trait `(* => *) => *`[T[_[_]]]

  //
  // EXERCISE 1
  //
  // Identify a type constructor that takes one type parameter (i.e. has kind
  // `* => *`), and place your answer inside the square brackets.
  //
  type Answer1 = `* => *`[List]

  //
  // EXERCISE 2
  //
  // Identify a type constructor that takes two type parameters (i.e. has kind
  // `[*, *] => *`), and place your answer inside the square brackets.
  //
  type Answer2 = `[*, *] => *`[Either]

  //
  // EXERCISE 3
  //
  // Create a new type that has kind `(* -> *) -> *`.
  //
  type NewType1[A[_]] /* ??? */
  type Answer3 = `(* => *) => *`[NewType1]

  //
  // EXERCISE 4
  //
  // Create a trait with kind `*`.
  //
  trait Answer4 /*[]*/

  //
  // EXERCISE 5
  //
  // Create a trait with kind `[*, *, *] => *`.
  //
  trait Answer5[A, B, C]

  //
  // EXERCISE 6
  //
  // Create a trait with kind `[* => *, (* => *) => *] => *`.
  //
  trait Answer6[A[_], B[C[_]]] /*[]*/

  //
  // EXERCISE 7
  //
  // Create an implementation of the trait `CollectionLike` for `List`.
  //
  trait CollectionLike[F[_]] {
    def empty[A]: F[A]

    def cons[A](a: A, as: F[A]): F[A]

    def uncons[A](as: F[A]): Option[(A, F[A])]

    final def singleton[A](a: A): F[A] =
      cons(a, empty[A])

    final def append[A](l: F[A], r: F[A]): F[A] =
      uncons(l) match {
        case Some((l, ls)) => append(ls, cons(l, r))
        case None => r
      }

    final def filter[A](fa: F[A])(f: A => Boolean): F[A] =
      bind(fa)(a => if (f(a)) singleton(a) else empty[A])

    final def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      uncons(fa) match {
        case Some((a, as)) => append(f(a), bind(as)(f))
        case None => empty[B]
      }

    final def fmap[A, B](fa: F[A])(f: A => B): F[B] = {
      val single: B => F[B] = singleton[B](_)

      bind(fa)(f andThen single)
    }
  }

  val ListCollectionLike: CollectionLike[List] = new CollectionLike[List] {
    def empty[A]: List[A] = List.empty[A]

    def cons[A](a: A, as: List[A]): List[A] = a :: as

    def uncons[A](as: List[A]): Option[(A, List[A])] = as match {
      case a :: t => Some(a, t)
      case Nil => None
    }
  }

  //
  // EXERCISE 8
  //
  // Implement `Sized` for `List`.
  //
  trait Sized[F[_]] {
    // This method will return the number of `A`s inside `fa`.
    def size[A](fa: F[A]): Int
  }
  val ListSized: Sized[List] = new Sized[List] {
    def size[A](fa: List[A]): Int = fa.size
  }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to `String`.
  //
  val MapSized1: Sized[Map[String, ?]] = new Sized[Map[String, ?]] {
    def size[A](fa: Map[String, A]): Int = fa.size
  }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to a user-defined type parameter.
  //
  def MapSized2[K]: Sized[Map[K, ?]] = new Sized[Map[K, ?]] {
    def size[A](fa: Map[K, A]): Int = fa.size
  }

  //
  // EXERCISE 10
  //
  // Implement `Sized` for `Tuple3`.
  //
  def Tuple3Sized[A,B]: Sized[(A,B,?)] = new Sized[(A, B, ?)] {
    def size[C](fa: (A, B, C)): Int = 1
  }
}

object typeclasses {
  /**
   * {{
   * Reflexivity:   a ==> equals(a, a)
   *
   * Transitivity:  equals(a, b) && equals(b, c) ==>
   *                equals(a, c)
   *
   * Symmetry:      equals(a, b) ==> equals(b, a)
   * }}
   */
  trait Eq[A] {
    def equals(l: A, r: A): Boolean
  }

  object Eq {
    def apply[A](implicit eq: Eq[A]): Eq[A] = eq

    implicit val EqInt: Eq[Int] = (l: Int, r: Int) => l == r

    implicit def EqList[A: Eq]: Eq[List[A]] = (l: List[A], r: List[A]) =>
      (l, r) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => false
        case (l :: ls, r :: rs) =>
          Eq[A].equals(l, r) && equals(ls, rs)
      }
  }

  implicit class EqSyntax[A](val l: A) extends AnyVal {
    def === (r: A)(implicit eq: Eq[A]): Boolean =
      eq.equals(l, r)
  }

  //
  // Scalaz 7 Encoding
  //
  sealed trait Ordering
  case object EQUAL extends Ordering
  case object LT extends Ordering
  case object GT extends Ordering
  object Ordering {
    implicit val OrderingEq: Eq[Ordering] = (l: Ordering, r: Ordering) =>
        (l, r) match {
          case (EQUAL, EQUAL) => true
          case (LT, LT) => true
          case (GT, GT) => true
          case _ => false
        }
  }

  trait Ord[A] {
    def compare(l: A, r: A): Ordering
  }

  object Ord {
    def apply[A](implicit A: Ord[A]): Ord[A] = A

    implicit val OrdInt: Ord[Int] = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering =
        if (l < r) LT else if (l > r) GT else EQUAL
    }
  }
  implicit class OrdSyntax[A](val l: A) extends AnyVal {
    def =?= (r: A)(implicit A: Ord[A]): Ordering =
      A.compare(l, r)

    def < (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), LT)

    def <= (r: A)(implicit A: Ord[A]): Boolean =
      (l < r) || (this === r)

    def > (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), GT)

    def >= (r: A)(implicit A: Ord[A]): Boolean =
      (l > r) || (this === r)

    def === (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), EQUAL)

    def !== (r: A)(implicit A: Ord[A]): Boolean =
      !Eq[Ordering].equals(A.compare(l, r), EQUAL)
  }


  implicit val OrdString: Ord[String] = new Ord[String] {
    def compare(l: String, r: String): Ordering = if (l < r) LT else if (l > r) GT else EQUAL
  }

  case class Person(age: Int, name: String)
  object Person {
    implicit val OrdPerson: Ord[Person] = new Ord[Person] {
      def compare(l: Person, r: Person): Ordering =
        if (l.age < r.age) LT else if (l.age > r.age) GT
        else if (l.name < r.name) LT else if (l.name > r.name) GT
        else EQUAL
    }
    implicit val EqPerson: Eq[Person] = new Eq[Person] {
      def equals(l: Person, r: Person): Boolean =
        l == r
    }
  }

  //
  // EXERCISE 1
  //
  // Write a version of `sort1` called `sort2` that uses the polymorphic `List`
  // type constructor, and which uses the `Ord` type class, including the
  // compare syntax operator `=?=` to compare elements.
  //
  def sort1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort1(lessThan) ++ List(x) ++ sort1(notLessThan)
  }

  def sort2[A: Ord](l: List[A]): List[A] = {
    l.sortWith {  case (a, b) => implicitly[Ord[A]].compare(a, b) != GT }
  }

  //
  // Scalaz 8 Encoding
  //
  sealed abstract class InstanceOfModule {
    type InstanceOf[T] <: T
    def instanceOf[T](t: T): InstanceOf[T]
  }

  object InstanceOfModule {
    val impl: InstanceOfModule = new InstanceOfModule {
      override type InstanceOf[T] = T
      override def instanceOf[T](t: T) = t
    }
  }
  import InstanceOfModule.impl._

  type ???[A] = Nothing

  /**
   * {{
   * // Associativity:
   * (a <> b) <> c === a <> (b <> c)
   * }}
   */
  trait SemigroupClass[A] {
    def append(l: => A, r: => A): A
  }
  type Semigroup[A] = InstanceOf[SemigroupClass[A]]
  object SemigroupClass {
    def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A

    implicit val SemigroupString: Semigroup[String] =
      instanceOf(
        new SemigroupClass[String] {
          def append(l: => String, r: => String): String = l + r
        })
    implicit def SemigroupList[A]: Semigroup[List[A]] =
      instanceOf(
        new SemigroupClass[List[A]] {
          def append(l: => List[A], r: => List[A]): List[A] = l ++ r
        })
  }
  implicit def AnyToSemigroupSyntax[A](a: => A): SemigroupSyntax[A] =
    new SemigroupSyntax(() => a)
  class SemigroupSyntax[A](l: () => A) {
    def <> (r: => A)(implicit A: Semigroup[A]): A = A.append(l(), r)
  }
  //
  // EXERCISE 2
  //
  // Create an instance of the `Semigroup` type class for `java.time.Instant`.
  //
  implicit val SemigroupInstant: Semigroup[java.time.Instant] = new Semigroup[java.time.Instant] {
    def append(l: => java.time.Instant, r: => java.time.Instant): java.time.Instant = l.plusMillis(r.toEpochMilli)
  }

  //
  // EXERCISE 3
  //
  // Create an instance of the `Semigroup` type class for `Int`.
  //
  implicit val SemigroupInt: Semigroup[Int] = new Semigroup[Int] {
    def append(l: => Int, r: => Int): Int = l + r
  }

  //
  // EXERCISE 4
  //
  // Create an instance of the `Semigroup` type class for `Set[A]`.
  //
  implicit def SemigroupSet[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    def append(l: => Set[A], r: => Set[A]): Set[A] = l ++ r
  }

  //
  // EXERCISE 5
  //
  // Create an instance of the `Semigroup` type class for `Map[K, ?]`. Hint:
  // you will need some constraint applied to the values.
  //
  implicit def SemigroupMap[K, V: Semigroup]: Semigroup[Map[K, V]] = new Semigroup[Map[K, V]] {
    def append(l: => Map[K, V], r: => Map[K, V]): Map[K, V] = {
      val S = implicitly[Semigroup[V]]
      val t = r -- l.keys
      l.map { case (k, v) => r.get(k).fold((k, v))(v2 =>  (k, S.append(v, v2)))} ++ t
    }
  }

  //
  // EXERCISE 6
  //
  // Create a type class `Monoid[A]` that implies `Semigroup[A]` (that is, every
  // `Monoid[A]` must be a `Semigroup[A]`), which adds a single operation called
  // `zero`, which satisfies additional laws.
  //
  /**
   * {{
   * append(zero, a) === a
   * append(a, zero) === a
   * }}
   */
  trait MonoidClass[A] extends SemigroupClass[A] {
    def zero: A
  }

  object MonoidClass {
    def apply[A](implicit A: Monoid[A]): Monoid[A] = A
  }

  type Monoid[A] = InstanceOf[MonoidClass[A]]
  implicit def MonoidSemigroup[A](implicit M: Monoid[A]): Semigroup[A] =
    instanceOf(M)
  def empty[A: Monoid]: A = implicitly[Monoid[A]].zero

  //
  // EXERCISE 7
  //
  // Create an instance of the `Monoid` type class for `java.time.Instant`.
  //
  implicit val MonoidInstant: Monoid[java.time.Instant] = new Monoid[java.time.Instant] {
    def zero: Instant = Instant.ofEpochSecond(0)
    def append(l: => Instant, r: => Instant): Instant = SemigroupInstant.append(l, r)
  }

  //
  // EXERCISE 8
  //
  // Create an instance of the `Monoid` type class for `String`.
  //
  implicit val MonoidString: Monoid[String] = new Monoid[String] {
    override def zero: String = ""
    override def append(l: => String, r: => String): String = SemigroupClass.SemigroupString.append(l, r)
  }

  //
  // EXERCISE 9
  //
  // Create an instance of the `Monoid` type class for `List[A]`.
  //
  implicit def MonoidList[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def zero: List[A] = Nil
    override def append(l: => scala.List[A], r: => scala.List[A]): scala.List[A] = l ++ r
  }

  //
  // EXERCISE 10
  //
  // Create an instance of the `Monoid` type class for `Int`.
  //
  implicit val MonoidInt: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0
    override def append(l: => Int, r: => Int): Int = SemigroupInt.append(l, r)
  }

  //
  // EXERCISE 11
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the additive monoid, with addition as `append`, and 0 as
  // `zero`.
  //
  final case class Sum(run: Int)
  implicit val MonoidSum: Monoid[Sum] = new Monoid[Sum] {
    override def zero: Sum = Sum(MonoidInt.zero)
    override def append(l: => Sum, r: => Sum): Sum = Sum(MonoidInt.append(l.run, r.run))
  }

  //
  // EXERCISE 12
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the multiplicative monoid, with multiplication as `append`,
  // and 1 as `zero`.
  //
  final case class Product(run: Int)
  implicit val MonoidProduct: Monoid[Product] = new Monoid[Product] {
    override def zero: Product = Product(1)
    override def append(l: => Product, r: => Product): Product = Product(l.run * r.run)
  }

  //
  // EXERCISE 13
  //
  // Create an instance of the `Collection` type class for `List`.
  //
  trait CollectionClass[F[_]] {
    def empty[A]: F[A]
    def cons[A](a: A, as: F[A]): F[A]
    def uncons[A](fa: F[A]): Option[(A, F[A])]
  }
  object CollectionClass {
    def apply[F[_]](implicit F: Collection[F]): Collection[F] = F
  }
  type Collection[F[_]] = InstanceOf[CollectionClass[F]]
  implicit val ListCollection: Collection[List] = new Collection[List] {
    def empty[A]: List[A] = List.empty[A]

    def cons[A](a: A, as: List[A]): List[A] = a :: as

    def uncons[A](as: List[A]): Option[(A, List[A])] = as match {
      case a :: t => Some(a, t)
      case Nil => None
    }
  }
}
