/**
 * Created by karama on 21.10.14.
 */


import scala.util.parsing.combinator._

object JsonParser extends JavaTokenParsers {

  def bool : Parser[Boolean] = ("false" | "true") ^^ {
    case "false" => false
    case "true" => true
  }
  def string : Parser[String] = "\"" ~> "[^\"]*".r <~ "\""
  def number : Parser[Float] = floatingPointNumber ^^ {_.toFloat}
  def nullValue : Parser[Null] = "null" ^^^ {null}
  def arr : Parser[Array[Any]] = "[" ~> (repsep(value, ",") ^^ {(x:List[Any]) => x.toArray}) <~ "]"
  def map : Parser[Map[String,Any]] = "{" ~> (repsep((key <~ ":") ~ value, ",") ^^ {
    (x : List[String ~ Any]) => x.map({case (k ~ v) => (k, v)}).toMap
  }) <~ "}"
  def value : Parser[Any] = string | number | nullValue | bool | arr | map | failure("invalid value")
  def key : Parser[String] = string | failure("invalid key")

  def apply(input : String) : Option[Map[String, Any]] = parseAll(map, input) match {
    case Success(result, _) => Some(result)
    case failure : NoSuccess => println("failed to parse JSON : %s".format(failure.msg));None
  }
}

object TestParser extends App {
  val example =
    """{"a" : 3 ,
      | "b" : 7,
      | "c" : false,
      | "g" : null,
      | "z" : "str",
      | "l" : [1, 2, 3] ,
      | "s" :
      | {"c": 1, "v" : true}
      | }""".stripMargin
  val res = JsonParser(example)
  println(res.getOrElse(""))
}