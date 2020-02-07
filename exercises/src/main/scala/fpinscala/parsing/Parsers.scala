package fpinscala.parsing

import language.higherKinds
import fpinscala.testing._

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) =
    ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) p.map2(listOfN(n - 1, p))(_ :: _)
    else succeed(Nil)
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def many[A](p: Parser[A]): Parser[List[A]] =
    p.map2(many(p))(_ :: _) or succeed(Nil)
  def many1[A](p: Parser[A]): Parser[List[A]] =
    p.map2(many(p))(_ :: _)
  def contextSensitiveMany[A](p: Parser[A]): Parser[List[A]] = for {
    digits <- "\\d+".r
    num = digits.toInt
    la <- listOfN(num, p)
  } yield la
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap { succeed[B] _ compose f }
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)
  def slice[A](p: Parser[A]): Parser[String]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = for {
    a <- p
    b <- p2
  } yield (a, b)
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {
    a <- p
    b <- p2
  } yield f(a, b)
  def map3[A,B,C,D](p: Parser[A], p2: => Parser[B], p3: => Parser[C])(f: (A, B, C) => D): Parser[D] =
    p flatMap(a => p2.map2(p3)(f(a, _, _)))
  def flatMap[A,B](a: Parser[A])(f: A => Parser[B]): Parser[B]
  implicit def regex(r: Regex): Parser[String]
  def traverse[A](p: List[Parser[A]]): Parser[List[A]] =
    p.foldRight(succeed(List[A]()))(_.map2(_)(_ :: _))
  def separated[A, B](separator: Parser[B])(p: => Parser[A]): Parser[List[A]] = {
    lazy val pp = p
    def rest: Parser[List[A]] =
      separator.flatMap(_ => pp).many
    pp.map2(rest)(_ :: _)
  }
  def delimited[A, B](leftDelimiter: Parser[B], rightDelimiter: Parser[B])(p: => Parser[A]): Parser[A] = for {
    _ <- leftDelimiter
    a <- p
    _ <- rightDelimiter
  } yield a
  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def attempt[A](p: Parser[A]): Parser[A]
  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    label("unexpected trailing characters")(regex("\\z".r))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def map2[B,C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def map3[B,C,D](p2: => Parser[B], p3: => Parser[C])(f: (A, B, C) => D): Parser[D] = self.map3(p, p2, p3)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))
    def productAssociativityLaw[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(in: Gen[String]): Prop = {
      def unbiasL(p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
      def unbiasR(p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)
      equal((pa ** pb) ** pc map unbiasL, pa ** (pb ** pc) map unbiasR)(in)
    }
    def productMapRelationshipLaw[A,B,C,D](pa: Parser[A], pb: Parser[B])(f: A => C, g: B => D)(in: Gen[String]): Prop =
      equal((pa map f) ** (pb map g), pa ** pb map {case (a, b) => (f(a), g(b))})(in)
  }
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def concat(seq: Parser[String]*): Parser[String] =
      traverse(seq toList) map (_.mkString)

    def oneNine: Parser[String] =
      "[1-9]".r

    def digit: Parser[String] =
      "0" or oneNine

    def whitespace: Parser[Unit] =
      (" " or "\t" or "\n" or "\r").many map(_ => ())

    def jNumber: Parser[JNumber] = {
      def digits: Parser[String] =
        many1(digit) map (_.mkString)

      def int: Parser[String] =
        concat("-" or "", attempt(concat(oneNine, digits)) or digit)

      def frac: Parser[String] =
        attempt(concat(".", digits)) or ""

      def exp: Parser[String] = {
        def sign: Parser[String] =
          "+" or "-" or ""
        concat("E" or "e", sign, digits) or ""
      }

      concat(int, frac, exp) map (JNumber compose (_ toDouble))
    }

    def jString: Parser[JString] = {
      def characters: Parser[String] = {
        def character: Parser[String] = {
          def escape: Parser[String] = {
            def simpleEscape: Parser[String] =
              ("\"" or "\\" or "/" or "b" or "n" or "r" or "t").map({
                case "b" => "\b"
                case "n" => "\n"
                case "r" => "\r"
                case "t" => "\t"
                case x => x
              })

            def codeEscape: Parser[String] = {
              def hex: Parser[String] =
                digit or "[a-fA-F]".r

              for {
                _ <- char('u')
                hexList <- listOfN(4, hex)
              } yield Integer.parseInt(hexList.mkString, 16).toChar.toString
            }

            simpleEscape or codeEscape
          }

          """[\x{0020}-\x{10FFFF}&&[^"\\]]""".r or (char('\\') flatMap(_ => escape))
        }

        character.many.map(_.mkString)
      }

      for {
        _ <- char('"')
        s <- characters
        _ <- char('"')
      } yield JString(s)
    }

    def jElement: Parser[JSON] = {
      def value: Parser[JSON] =
        attempt(jNull) or attempt(jBool) or attempt(jNumber) or attempt(jString) or attempt(jArray) or jObject

      delimited(whitespace, whitespace)(value)
    }

    def jArray: Parser[JArray] = {
      def elements: Parser[List[JSON]] =
        separated(",")(jElement)

      delimited("[", "]")(attempt(elements) or whitespace.map(_ => Nil))
        .map(JArray compose (_ toIndexedSeq))
    }

    def jObject: Parser[JObject] = {
      def members: Parser[List[(String, JSON)]] = {
        def member: Parser[(String, JSON)] = for {
          k <- delimited(whitespace, whitespace)(jString)
          _ <- char(':')
          v <- jElement
        } yield (k.get, v)

        separated(",")(member)
      }

      delimited("{", "}")(attempt(members) or whitespace.map(_ => Nil))
        .map(JObject compose (_.toMap))
    }

    def jNull: Parser[JNull.type] =
      string("null") map (_ => JNull)

    def jBool: Parser[JBool] =
      "true" or "false" map (JBool compose (_ toBoolean))

    jElement.map2(eof)((json, _) => json)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc,msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_,s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location,String)] =
    stack.lastOption
}

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, isCommitted) => Failure(f(e), isCommitted)
    case _ => this
  }
  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, false)
    case _ => this
  }
  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e,c) => Failure(e, c || isCommitted)
    case _ => this
  }
  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a,m) => Success(a,n+m)
    case _ => this
  }
}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError,
                   isCommitted: Boolean = true) extends Result[Nothing]

object Parsers {
  type Parser[+A] = Location => Result[A]

  object SimpleParsers extends Parsers[Parser] {
    def run[A](p: Parser[A])(input: String): Either[ParseError,A] =
      p(Location(input)) match {
        case Success(a, _) => Right(a)
        case Failure(e, _) => Left(e)
      }
    def string(s: String): Parser[String] = {
      /** Returns -1 if s1.startsWith(s2), otherwise returns the
        * first index where the two strings differed. If s2 is
        * longer than s1, returns s1.length. */
      def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
        var i = 0
        while (i + offset < s1.length && i < s2.length) {
          if (s1.charAt(i+offset) != s2.charAt(i)) return i
          i += 1
        }
        if (s1.length-offset >= s2.length) -1
        else s1.length-offset
      }

      scope("Trying to match: '" + s + "'") { l: Location =>
        val i = firstNonmatchingIndex(l.input, s, l.offset)
        if (i == -1) // they matched
          Success(s, s.length)
        else {
          val advancedLoc = l.advanceBy(i)
          val errorMsg = "Error while parsing: '" + advancedLoc. input + "', at line: " + advancedLoc.line + ", col: " + advancedLoc.col
          Failure(advancedLoc.toError(errorMsg), i != 0)
        }
      }
    }
    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] =
      (l: Location) =>
        s1(l) match {
          case Failure(_, false) => s2(l)
          case r => r
        }
    def slice[A](p: Parser[A]): Parser[String] =
      (l: Location) =>
        p(l) match {
          case Success(_, charsConsumed) => Success(l.input.substring(0, charsConsumed), charsConsumed)
          case f@Failure(_, _) => f
        }
    def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
      (l: Location) =>
        p(l) match {
          case Success(a, n) =>
            f(a)(l.advanceBy(n))
              .addCommit(n != 0)
              .advanceSuccess(n)
          case e@Failure(_, _) => e
        }
    def regex(r: Regex): Parser[String] =
      (l: Location) =>
        r.findPrefixOf(l.input.substring(l.offset))
          .map(s => Success(s, s.length))
          .getOrElse(Failure(l.toError("Expected to match regex " + r), false))
    def label[A](msg: String)(p: Parser[A]): Parser[A] =
      (l: Location) =>
        p(l).mapError(_.label(msg))
    def scope[A](msg: String)(p: Parser[A]): Parser[A] =
      (l: Location) =>
        p(l).mapError(_.push(l, msg))
    def attempt[A](p: Parser[A]): Parser[A] =
      (l: Location) =>
        p(l) uncommit
    override def succeed[A](a: A): Parser[A] =
      _ => Success(a, 0)
  }
}

object Test {
  import fpinscala.parsing.Parsers.SimpleParsers

  def main(args: Array[String]): Unit = {
//    println(SimpleParsers.run(JSON.jsonParser(SimpleParsers))("null"))
    SimpleParsers.run(JSON.jsonParser(SimpleParsers))("[true]").toString
  }
}