import fpinscala.parsing._
import scala.util.matching.Regex
import fpinscala.parsing.Parsers._

"a" + "\t" + "a"

("\\" + "t").codePointAt(0)


"//"

"\t".codePointAt(0)

"""^["\\/bnrt]""".r.findFirstIn("b")
"\""

"\\/"

"a\\nb"

"a" + 9.toChar + "a"

Integer.parseInt("00Fa", 16)

val x = 9.toChar

'a' + Character.toString(x) + 'a'
Char.char2int("\t".charAt(0))
Character.toChars(9)

"""["\\]""".r.findFirstIn("\"\\")

List(("a", 4), ("b", 10)).toMap

"foo" + "abc\\d[-!]".r


Some(3).toList
None.toList

SimpleParsers.run(JSON.jsonParser(SimpleParsers))("null  ").toString
SimpleParsers.run(JSON.jsonParser(SimpleParsers))("true").toString
SimpleParsers.run(JSON.jsonParser(SimpleParsers))(" false").toString
SimpleParsers.run(JSON.jsonParser(SimpleParsers))("\"asdf\"").toString
SimpleParsers.run(JSON.jsonParser(SimpleParsers))("4.6e4").toString
SimpleParsers.run(JSON.jsonParser(SimpleParsers))("[null  , true,   [1, 3, \"foo\"],  false, \"asdf\"]").toString
SimpleParsers.run(JSON.jsonParser(SimpleParsers))("{ \"foo\"  : 3 , \"bar\": [null  , true, {},  [1, 3, \"foo\"],  false, \"asdf\"]} ").toString

SimpleParsers.run(JSON.jsonParser(SimpleParsers))("[true] asdf").toString
SimpleParsers.run(JSON.jsonParser(SimpleParsers))("[ ]")
SimpleParsers.run(JSON.jsonParser(SimpleParsers))("{ }")

SimpleParsers.run(JSON.jsonParser(SimpleParsers))("\"a b\"")
SimpleParsers.run(JSON.jsonParser(SimpleParsers))("30.66")



val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""
SimpleParsers.run(JSON.jsonParser(SimpleParsers))(jsonTxt).fold(println, println)

val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""
SimpleParsers.run(JSON.jsonParser(SimpleParsers))(malformedJson1).fold(println, println)

val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""
SimpleParsers.run(JSON.jsonParser(SimpleParsers))(malformedJson2).fold(println, println)

