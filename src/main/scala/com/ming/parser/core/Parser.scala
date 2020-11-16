package com.ming.parser.core
import cats._
import cats.implicits._

case class Parser[A](run: String => Option[(String, A)])

object Parser {
  implicit val functorP = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      Parser(s => fa.run(s).map { case (s1, a1) => (s1, f(a1)) })
  }

  implicit val applicativeP = new Applicative[Parser] {
    override def pure[A](x: A): Parser[A] = Parser(s => (s, x).some)

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
      Parser(s => for {
        (s1, f1) <- ff.run(s)
        (s2, a2) <- fa.run(s1)
      } yield (s2, f1(a2))
      )
  }

  implicit val alternativeP = new MonoidK[Parser] {
    override def empty[A]: Parser[A] = Parser(_ => None)

    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] =
      Parser(s => x.run(s) <+> y.run(s))
  }

  implicit val monadP = new Monad[Parser] {
    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = Parser(s =>
      for {
        (s1, a1) <- fa.run(s)
        (s2, a2) <- f(a1).run(s1)
      } yield (s2, a2)
    )

    override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = Parser(s => {
      f(a).run(s) match {
        case Some((s1, Right(e1))) => Some((s1, e1))
        case Some((s1, Left(e1))) => tailRecM(e1)(f).run(s1)
        case None => None
      }
    })

    override def pure[A](x: A): Parser[A] = Applicative[Parser].pure(x)
  }

  def char(c: Char): Parser[Char] = Parser(s =>
    for {
      v <- s.headOption if (v == c)
    } yield (s.tail, c)
  )

  def string(str: String): Parser[String] = {
    val x: List[Parser[Char]] = str.map(char).toList
    val y: Parser[List[Char]] = x.sequence
    y.map(_.mkString)
  }

  def digit: Parser[Char] = Parser(s =>
    for {
      c <- s.headOption if (c.isDigit)
    } yield (s.tail, c)
  )

  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    val ff = f.curried
    p1.map(v => ff(v)).ap(p2)
  }

//  def many[A](p: Parser[A]): Parser[A] = Parser(s => {
//    p.run(s) match {
//      case Some((s1, a1)) => many(Applicative[Parser].pure(a1)).run(s1)
//      case x => x
//    }
//
//  })

  def naturalNumber: Parser[Int] = Parser(s => {
    val reg = "(^\\d+)(.*)".r
    reg.unapplySeq(s).map(v => (v.last, v.head.toInt))
  })
}




