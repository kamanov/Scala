/**
 * Created by karama on 21.10.14.
 */

import scala.util.parsing.combinator._

abstract class Expr
case class Num(value : Double) extends Expr
case class BinOperation(op : String, left : Expr, right : Expr) extends Expr

object RpnParser extends RegexParsers {
  def number: Parser[Num] = """\d+(\.\d*)?""".r ^^ {
    value => Num(value.toDouble)
  }
  def operation : Parser[String] = "+" | "-" | "*" | "/"

  def expr : Parser[Expr] = (factor ~ term | number) ^^ {
    case n : Num => n
    case (f : Expr) ~ (t : List[Expr => Expr]) => if (t.isEmpty) f else t.foldLeft(f)((l, r) => r(l))
  }

  def factor : Parser[Expr] = (number ~ expr ~ operation) ^^ {
    case n ~ e ~ o => BinOperation(o, n, e)
  }

  def term : Parser[List[Expr => Expr]] = rep(expr ~ operation ^^ {
    case e ~ o => (x : Expr) => BinOperation(o, x, e)
  })

}

object Calculator {
  def apply(input : String): Option[Double] = {
    val root: Option[Expr] = RpnParser.parseAll(RpnParser.expr, input) match {
      case RpnParser.Success(result, _) => Some(result)
      case failure: RpnParser.NoSuccess => println("failed to parse expression :%s".format(failure.msg)); None
    }
    def eval(expr: Expr): Double = expr match {
      case Num(value) => value
      case BinOperation(op, left, right) => op match {
        case "+" => eval(left) + eval(right)
        case "-" => eval(left) - eval(right)
        case "*" => eval(left) * eval(right)
        case "/" => eval(left) / eval(right)
      }
    }
    root.map(expr => eval(expr))
  }
}

object Main extends App {
  val example = "5 1 2 + 4 * + 3-"
  println(Calculator(example).getOrElse("ERROR"))
}
