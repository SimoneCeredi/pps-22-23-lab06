package u06lab.mandatory

/** Consider the Parser example shown in previous lesson. Analogously to NonEmpty, create a mixin NotTwoConsecutive,
 * which adds the idea that one cannot parse two consecutive elements which are equal. Use it (as a mixin) to build
 * class NotTwoConsecutiveParser, used in the testing code at the end. Note we also test that the two mixins can work
 * together!!
 */

abstract class Parser[T]:
  def parse(t: T): Boolean // is the token accepted?

  def end: Boolean // is it ok to end here

  def parseAll(seq: Seq[T]): Boolean = (seq forall parse) & end // note &, not &&

object Parsers:
  val todo = ??? // put the extensions here..

class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char): Boolean = chars.contains(t)

  override def end: Boolean = true

trait NonEmpty[T] extends Parser[T]:
  private[this] var empty = true

  abstract override def parse(t: T): Boolean =
    empty = false;
    super.parse(t) // who is super??

  abstract override def end: Boolean = !empty && super.end

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]:
  private[this] var prev: Option[T] = Option.empty
  private[this] var tc = false

  abstract override def parse(t: T): Boolean =
    if prev.nonEmpty && prev.get == t then tc = true
    prev = Option(t)
    super.parse(t)

  abstract override def end: Boolean = !tc && super.end

trait ShorterThanN[T](private var n: Int) extends Parser[T]:
  abstract override def parse(t: T): Boolean =
    n = n - 1
    super.parse(t)

  abstract override def end: Boolean = n > 0 && super.end

class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]

@main def checkParsers(): Unit =
  println("parser")

  def parser = new BasicParser(Set('a', 'b', 'c'))

  println(parser.parseAll("aabc".toList)) // true
  println(parser.parseAll("aabcdc".toList)) // false
  println(parser.parseAll("".toList)) // true

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  println("\n\nNE parser")

  def parserNE = new NonEmptyParser(Set('0', '1'))

  println(parserNE.parseAll("0101".toList)) // true
  println(parserNE.parseAll("0123".toList)) // false
  println(parserNE.parseAll(List())) // false

  println("\n\nNTC parser")

  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))

  println(parserNTC.parseAll("XYZ".toList)) // true
  println(parserNTC.parseAll("XYYZ".toList)) // false
  println(parserNTC.parseAll("".toList)) // true

  // note we do not need a class name here, we use the structural type
  println("\n\nNTCNE parser")

  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]

  println(parserNTCNE.parseAll("XYZ".toList)) // true
  println(parserNTCNE.parseAll("XYYZ".toList)) // false
  println(parserNTCNE.parseAll("".toList)) // false


  println("\n\nsparser")
  extension (s: String)
    def charParser(): Parser[Char] = new BasicParser(s.toSet)

  def sparser: Parser[Char] = "abc".charParser()

  println(sparser.parseAll("aabc".toList)) // true
  println(sparser.parseAll("aabcdc".toList)) // false
  println(sparser.parseAll("".toList)) // true

  println("\n\nShorterThanN")

  def shorterThanN = new BasicParser(Set('a', 'b', 'c')) with ShorterThanN[Char](5)

  println(shorterThanN.parseAll("aabc".toList)) // true
  println(shorterThanN.parseAll("aabcb".toList)) // false
  println(shorterThanN.parseAll("".toList)) // true
