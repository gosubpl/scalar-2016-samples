package com.gft.scala.scalarconf

import scalaz._
import Scalaz._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

class Foo(val s: String) {
  override def toString = s
}

object Main extends App {
  import scala.language.higherKinds

  def buildTwo[F[_], B](a: F[B], b: F[B])(implicit F: Apply[F]) = a |@| b

  def transformWithFunc[F[_], A, B](a: F[A], fa: A => B)(implicit F: Functor[F]) = F.map(a)(fa)

  def transformIntWithFunc[F[_], B](a: F[Int], fa: Int => B)(implicit F: Functor[F]) = F.map(a)(fa)

  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil => (Nil: List[A]).pure[F]
    case x :: xs => (x |@| sequenceA(xs)).apply { _ :: _ }
  }

  val a: Int = 7
  val b: Int = 5
  val c: Int = 7
  val s: String = "uuu"
  if (a === b) {
    println("hello")
  } else if (a === c) {
    println("world")
  }
  if (s == a) {
    println("cannot be")
  }

  println(1.0 ?|? 2.0)

  val oa: Option[Int] = Some(3)
  val ob: Option[Int] = Some(5)

  val od = oa |@| ob

  val odx = od.apply(_ + _)

  println(odx)

  val oe = 3.some |@| 5.some

  val oex = od.apply(_ + _)

  println(oex)

  val on = 1.some |@| none[Int]

  println(on.apply(_ + _))

  val oc = oa |+| ob
  println(oc)

  val al: List[Int] = List(3)
  val bl: List[Int] = List(5)

  val le = al |@| bl

  println(le.apply(_ + _))

  val lee = buildTwo(al, bl)
  val lop = buildTwo(oa, ob)

  def twice(x: Int) = 2 * x

  def sort(x: List[Int]) = x.sorted

  println(transformWithFunc(List(3, 2, 1).some, sort))

  val futList: Future[List[Int]] = Future { List(1, 2, 3) }

  println("Future now")

  val transFut = transformWithFunc(futList, sort)

  println(transFut.isCompleted)

  println(transformWithFunc(List(1, 2, 3), twice))

  println(transformWithFunc(1.some, twice))

  //println(transformWithFunc("aa".some, twice)

  println(transformIntWithFunc(5.some, twice))

  println("abc" === "cde")

  println("abc" =/= "cde")

  println(5 === 5)

  println("abc".show)

  println(10.show)

  val foo: Foo = new Foo("foo")

  println(foo)

  /*
  implicit val fooShow: Show[Foo] =
    new Show[Foo] {
      def show(foo: Foo) = foo.s
    }
   */
  implicit val fooFromToStringShow: Show[Foo] =
    Show.showFromToString[Foo]

  println(foo.show)

  println(1.0 compare 2.0)

  implicit val fooEqualInstance: Equal[Foo] =
    new Equal[Foo] {
      def equal(lhs: Foo, rhs: Foo): Boolean = lhs.s === rhs.s
    }

  println(foo === foo)

  println(1.some *> 2.some)

  println(sequenceA(List(1.some, 2.some, 3.some)))

  println(sequenceA(List(1.some, none[Int], 2.some)))

  println(sequenceA(List(1.some, "a".some)))

  //println(sequenceA(List(1.some, List(2))))

  println(sequenceA(List(List(1), List(2))))

  println(Applicative[Option].pure(1))
  println(Applicative[List].pure(1))

  val propsStringGood = List("db.user=gosubpl", "db.pass=pass", "db.name=dbase1")
  val propsStringBad = List("db.user=gosubpl", "db.pass=pass")

  println(propsStringGood.find(_.startsWith("db.user=")).map(_.replace("db.user=", "")))
  println(propsStringBad.find(_.startsWith("db.name=")).map(_.replace("db.name=", "")))

  def readProperty(prop: String)(props: List[String]): Option[String] = props.find(_.startsWith(prop + "=")).map(_.replace(prop + "=", ""))

  val dbUser = readProperty("db.user") _
  val dbName = readProperty("db.name") _
  val dbPass = readProperty("db.pass") _

  def dbProps(config: List[String]) = (dbUser(config) |@| dbName(config) |@| dbPass(config))

  println(dbProps(propsStringGood).apply(_ + " " + _ + " " + _))
  println(dbProps(propsStringBad).apply(_ + " " + _ + " " + _))

  def applyProps(f: Function1[List[String], Option[String]], s: List[String]) = f(s)

  println(sequenceA(List(dbUser, dbName, dbPass).map(applyProps(_, propsStringGood))))

  println(List(dbUser, dbName, dbPass).map(applyProps(_, propsStringGood)).sequence)

  println(List(dbUser, dbName, dbPass).map(applyProps(_, propsStringGood)).traverse(identity))

  println(List(dbUser, dbName, dbPass).map(applyProps(_, propsStringGood)).traverse(_.map(_.toUpperCase())))

  def readPropertyV(prop: String)(props: List[String]): Validation[String, String] =
    props.find(_.startsWith(prop + "=")).map(_.replace(prop + "=", "")) match {
      case None => ("Property " + prop + " not found in config").failure[String]
      case Some(s) => s.success[String]
    }

  val dbUserV = readPropertyV("db.user") _
  val dbNameV = readPropertyV("db.name") _
  val dbPassV = readPropertyV("db.pass") _

  def applyPropsV(f: Function1[List[String], Validation[String, String]], s: List[String]) = f(s)

  println(List(dbUserV, dbNameV, dbPassV).map(applyPropsV(_, propsStringBad)).traverseU(identity))

  val propsStringVeryBad = List("db.user=gosubpl")

  println(List(dbUserV, dbNameV, dbPassV).map(applyPropsV(_, propsStringVeryBad)).sequenceU)

}
