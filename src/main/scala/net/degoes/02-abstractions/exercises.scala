// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz._
import Scalaz._

object algebra {
  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])
  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] = new Semigroup[NotEmpty[A]] {
    override def append(f1: NotEmpty[A], f2: => NotEmpty[A]): NotEmpty[A] = f1 match {
      case NotEmpty(h1, None) => NotEmpty(h1, Some(f2))
      case NotEmpty(h1, Some(h2)) => NotEmpty(h1, Some(append(h2, f2)))
    }
  }

  val example1 = NotEmpty(1, Some(NotEmpty(3, None))) |+| NotEmpty(2, None)

  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //
  sealed trait Permit
  case object Read extends Permit
  case object Write extends Permit
  case object Execute extends Permit

  case class Resource(id: String)
  case class User(name: String)


  case class Permission(permits: Map[User, Map[Resource, Set[Permit]]])

  implicit val MonoidPermission: Monoid[Permission] = new Monoid[Permission] {
    def zero: Permission = Permission(Map.empty)
    def append(f1: Permission, f2: => Permission): Permission = Permission(f1.permits |+| f2.permits)
  }

  val example2 = mzero[Permission] |+| Permission(Map.empty)

  //
  // EXERCISE 3
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]:
    Semigroup[(A, B)] = new Semigroup[(A, B)] {
      def append(l: (A, B), r: => (A, B)): (A, B) = (l._1 |+| r._1, l._2 |+| r._2)
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  //
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = ???
}

object functor {

  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A]

  case class Leaf[A](a: A) extends BTree[A]

  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        ???
    }

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  implicit val NothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    override def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  }

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])

  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] =
    new Functor[Parser[E, ?]] {
      def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] = Parser[E, B](s => fa.run(s).map(e => (e._1, f(e._2))))
    }

  def zip[E, A, B](l: Parser[E, A], r: Parser[E, B]): Parser[E, (A, B)] = Parser[E, (A, B)] { s =>
    (l.run(s), r.run(s)) match {
      case (Right(ll), Right(rr)) => Right(ll._1, (ll._2, rr._2))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }
  }


  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)

  implicit val DataTypeFunctor: Functor[DataType] =
    new Functor[DataType] {
      def map[A, B](fa: DataType[A])(f: A => B): DataType[B] = DataType(b => f(fa.f))
    }

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: G[A])

  implicit def FunctorProductFunctor[F[_] : Functor, G[_] : Functor]:
  Functor[FunctorProduct[F, G, ?]] = new Functor[FunctorProduct[F, G, ?]] {
    override def map[A, B](fa: FunctorProduct[F, G, A])(f: A => B): FunctorProduct[F, G, B] =
      FunctorProduct(fa.l.map(f), fa.r.map(f))
  }

  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])

  implicit def FunctorSumFunctor[F[_] : Functor, G[_] : Functor]:
  Functor[FunctorSum[F, G, ?]] = new Functor[FunctorSum[F, G, ?]] {
    override def map[A, B](fa: FunctorSum[F, G, A])(f: A => B): FunctorSum[F, G, B] =
      FunctorSum(fa.run match {
        case Left(ffa) => Left(ffa.map(f))
        case Right(gga) => Right(gga.map(f))
      })
  }

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])

  implicit def FunctorNestFunctor[F[_] : Functor, G[_] : Functor]:
  Functor[FunctorNest[F, G, ?]] = new Functor[FunctorNest[F, G, ?]] {
    override def map[A, B](fa: FunctorNest[F, G, A])(f: A => B): FunctorNest[F, G, B] = FunctorNest(fa.run.map(_.map(f)))
  }

  def zipOption[A, B](l: Option[A], r: Option[B]): Option[(A, B)] = l.flatMap(ll => r.map(rr => (ll, rr)))

  def zipWith[A, B, C](l: Option[A], r: Option[B])(f: ((A, B)) => C): Option[C] = zipOption(l, r).map(f)

  def zipList1[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, bs) =>
        zipList1(as, bs) ++ bs.map(b => (a, b))
      case (Nil, bs) => Nil
    }

  def zipList2[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, b :: bs) => ((a, b)) :: zipList2(as, bs)
      case _ => Nil
    }

  //
  // EXERCISE 8
  //
  // Define `Applicative` for `Option`.
  //
  implicit val OptionApplicative: Applicative[Option] =
  new Applicative[Option] {
    def point[A](a: => A): Option[A] = Some(a)

    def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] = fa.flatMap(a => f.map(ff => ff(a)))
  }

  //
  // EXERCISE 9
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  // Bonus: Implement `ap2` in terms of `zip`.
  //
  val example1 = (Option(3) |@| Option(5)) ((_, _))
  val example2 = zip(Option(3), Option("foo")): Option[(Int, String)]

  def zip[F[_] : Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] = (l |@| r) ((_, _))

  def ap2[F[_] : Applicative, A, B](fa: F[A], fab: F[A => B]): F[B] = zip(fa, fab).map(s => s._2(s._1))

  //
  // EXERCISE 10
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E, A] = Parser(s => Right((s, a)))

      def ap[A, B](fa: => Parser[E, A])(f: => Parser[E, A => B]): Parser[E, B] = Parser { s =>
        (fa.run(s), f.run(s)) match {
          case (Right(faa), Right(ff)) => Right(ff._1, ff._2(faa._1))
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
        }
      }
    }

  implicit def ApplicativeList: Applicative[List] = new Applicative[List] {
    def point[A](a: => A): List[A] = List(a)

    def ap[A, B](fa: => List[A])(f: => List[A => B]): List[B] = fa match {
      case h :: t => f.map(s => s(h)) ::: ap(t)(f)
      case Nil => Nil
    }
  }

  //
  // EXERCISE 11
  //
  // Define an instance of `Monad` for `BTree`.
  //
  implicit val MonadBTree: Monad[BTree] = new Monad[BTree] {
    def point[A](a: => A): BTree[A] = Leaf(a)

    def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] = fa match {
      case Leaf(a) => f(a)
      case Fork(l, r) => Fork(bind(l)(f), bind(r)(f))
    }
  }

  implicit val MonadList: Monad[List] = new Monad[List] {
    def point[A](a: => A): List[A] = List(a)

    def bind[A, B](fa: List[A])(f: A => List[B]): List[B] = fa match {
      case Nil => Nil
      case h :: t => f(h) ::: bind(t)(f)
    }
  }

  implicit val MonadOption: Monad[Option] = new Monad[Option] {
    def point[A](a: => A): Option[A] = Some(a)

    def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None => None
    }
  }

  //
  // EXERCISE 12
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] = new Monad[Parser[E, ?]] {
    def point[A](a: => A): Parser[E, A] = Parser(s => Right((s, a)))

    def bind[A, B](fa: Parser[E, A])(f: A => Parser[E, B]): Parser[E, B] =
      Parser(s => fa.run(s).flatMap(a => f(a._2).run(s)))
  }
}

object parser {
  case class Parser[+E, +A](run: String => Either[E, (String, A)]) { self =>
    def ~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, (A,B)] = for {
      a <- self
      b <- that
    } yield (a, b)

    def ~> [E1 >: E, B](that: Parser[E1, B]): Parser[E1, B] = (self ~ that).map(_._2)

    def <~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, A] = (self ~ that).map(_._1)

    def map[B](f: A => B): Parser[E, B] = flatMap(s => MonadParser[E].point(f(s)))

    def flatMap[E1 >: E, B](f: A => Parser[E1, B]): Parser[E1, B] = new Parser[E1, B](input =>
      self.run(input) match {
        case Left(e) => Left(e)
        case Right((str, r)) => f(r).run(str)
      }
    )

    def orElse[E1 >: E, B](that: Parser[E1, B]): Parser[E1, Either[A,B]] = ???

    def | [E1 >: E, A1 >: A](that: Parser[E1, A1]): Parser[E1, A1] = (self orElse (that)).map(_.merge)
  }

  sealed trait Error
  case object ExpectedLit extends Error
/*
  val parser: Parser[Error, List[Int]] = for {
    c <- char(ExpectedLit)
    _ <- if (c == '[') Parser.point(()) else Parser.fail(ExpectedLit)
  }
*/
  object Parser {
    def fail[E, A](e: E): Parser[E, A] =
      Parser(input => Left(e))

    def point[E, A](a: => A): Parser[E, A] =
      Parser(input => Right((input, a)))

    def char[E](e: E): Parser[E, Char] =
      Parser(input =>
        if (input.length == 0) Left(e)
        else Right((input.drop(1), input.charAt(0))))

    def digit[E](e: E): Parser[E, Int] =
      for {
        c <- char(e)
        opt = scala.util.Try(c.toString.toInt).toOption
        d <- opt.fold(Parser.fail[E, Int](e))(point[E, Int](_))
      } yield d
  }

  implicit def MonadParser[E]: Monad[Parser[E, ?]] =
    new Monad[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] = new Parser(run = s => Right((s,a)))

      def bind[A, B](fa: Parser[E,A])(f: A => Parser[E,B]): Parser[E,B] = new Parser[E, B](input =>
        fa.run(input) match {
          case Right((s, a)) => f(a).run(s)
          case Left(e) => Left(e)
        }
      )
    }
}

object foldable {
  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val FoldableBTree: Foldable[BTree] =
    new Foldable[BTree] {
      def foldMap[A, B](fa: BTree[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
        case Leaf(a) => f(a)
        case Fork(l, r) => foldMap(l)(f) |+| foldMap(r)(f)
      }

      def foldRight[A, B](fa: BTree[A], z: => B)(f: (A, => B) => B): B = fa match {
        case Leaf(a) => f(a, z)
        case Fork(l, r) => {
          val left = foldRight(l, z)(f)
          foldRight(r, left)(f)
        }
      }
    }


  implicit val FoldableList: Foldable[List] =
    new Foldable[List] {
      def foldMap[A, B](fa: List[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
        case Nil => mzero[B]
        case h :: t => f(h) |+| foldMap(t)(f)
      }

      def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B = fa match {
        case Nil => z
        case h :: t => foldRight(t, f(h, z))(f)
      }
    }

  //
  // EXERCISE 2
  //
  // Try to define an instance of `Foldable` for `A => ?`.
  //
  implicit def FunctionFoldable[A]: Foldable[A => ?] = ???

  //
  // EXERCISE 3
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  implicit val TraverseBTree: Traverse[BTree] =
    new Traverse[BTree] {
      def traverseImpl[G[_]: Applicative, A, B](fa: BTree[A])(f: A => G[B]): G[BTree[B]] = fa match {
        case Leaf(a) => f(a).map(s => Leaf(s))
        case Fork(left, right) => (traverseImpl(left)(f) |@| traverseImpl(right)(f))(Fork(_, _))
      }
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] = ???
}

object optics {
  sealed trait Country
  object Country {
    val usa: Prism[Country, Unit] = Prism (
      c => c match {
        case USA => Some(())
        case _ => None
      },
      a => USA
    )
  }
  case object USA extends Country
  case object UK extends Country
  case object Poland extends Country

  case class Org(name: String, address: Address, site: Site)
  object Org {
    val site: Lens[Org, Site] =
      Lens[Org, Site](_.site, l => _.copy(site = l))
  }
  case class Address(
    number: String,
    street: String,
    postalCode: String,
    country: Country)
  case class Site(
    manager: Employee,
    address: Address,
    employees: Set[Employee])
  object Site {
    val manager: Lens[Site, Employee] =
      Lens[Site, Employee](_.manager, m => _.copy(manager = m))
  }
  case class Employee(
    name: String,
    dob: java.time.Instant,
    salary: BigDecimal,
    address: Address)
  object Employee {
    val salary: Lens[Employee, BigDecimal] =
      Lens[Employee, BigDecimal](_.salary, s => _.copy(salary = s))
  }

  lazy val org: Org = ???

  lazy val org2 =
    org.copy(site =
      org.site.copy(manager = org.site.manager.copy(
        salary = org.site.manager.salary * 0.95
      ))
    )

  //
  // EXERCISE 1
  //
  // Implement the `>>>` method of `Lens`.
  //·
  final case class Lens[S, A](
    get: S => A,
    set: A => (S => S)
  ) { self =>
    def xx[B](that: Lens[A, B]): Lens[S, B] = Lens[S, B](s => that.get(self.get(s)), b => self.set(self.get(b)))

    final def update(f: A => A): S => S =
      (s: S) => self.set(f(self.get(s)))(s)
  }

  //
  // EXERCISE 2
  //
  // Create a version of `org2` that uses lenses to update the salaries.
  //
  val org2_lens: Org =
  (Org.site >>> Site.manager >>> Employee.salary).update(_ * 0.95)(org)
  //
  // EXERCISE 3
  //
  // Implement `>>>` for `Prism`.
  //
  final case class Prism[S, A](
    get: S => Option[A],
    set: A => S) { self =>
    def >>> [B](that: Prism[A, B]): Prism[S, B] = Prism[S, B](
      s => get(s).flatMap(that.get),
      b => self.set(that.set(b))
    )

    final def select(implicit ev: Unit =:= A): S =
      set(ev(()))
  }

  //
  // EXERCISE 4
  //
  // Implement `_Left` and `_Right`.
  //
  def _Left[A, B]: Prism[Either[A, B], A] =
    Prism(_.fold(Some(_), _ => None), Left(_))
  def _Right[A, B]: Prism[Either[A, B], B] =
    Prism(_.fold(_ => None, Some(_)), Right(_))
}
