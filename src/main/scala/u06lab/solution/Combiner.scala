package u06lab.solution

import scala.collection.Seq

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double

  def concat(a: Seq[String]): String

  def max(a: List[Int]): Int // gives Int.MinValue if a is empty


//object FunctionsImpl extends Functions:
//  override def sum(a: List[Double]): Double = a.foldLeft(0.0)((t, el) => t + el)
//
//  override def concat(a: Seq[String]): String = a.fold("")((tot, n) => tot ++ n)
//
//  override def max(a: List[Int]): Int = a.fold(Int.MinValue)((min, el) => if el < min then el else min)

object FunctionsImpl extends Functions:

  import GivenCombiners.given

  override def sum(a: List[Double]): Double = combine(a)

  override def concat(a: Seq[String]): String = combine(a.toList)

  override def max(a: List[Int]): Int = combine(a)

  def combine[T: Combiner](a: List[T]): T = a.foldLeft(summon[Combiner[T]].unit)((t, el) => summon[Combiner[T]].combine(t, el))

/*
 * 2) To apply DRY principle at the best,
 * note the three methods in Functions do something similar.
 * Use the following approach:
 * - find three implementations of Combiner that tell (for sum,concat and max) how
 *   to combine two elements, and what to return when the input list is empty
 * - implement in FunctionsImpl a single method combiner that, other than
 *   the collection of A, takes a Combiner as input
 * - implement the three methods by simply calling combiner
 *
 * When all works, note we completely avoided duplications..
 */

object GivenCombiners:
  given Combiner[Double] with
    override def unit: Double = 0.0

    override def combine(a: Double, b: Double): Double = a + b

  given Combiner[String] with
    override def unit: String = ""

    override def combine(a: String, b: String): String = a ++ b

  given Combiner[Int] with
    override def unit: Int = Int.MinValue

    override def combine(a: Int, b: Int): Int = if a > b then a else b


trait Combiner[A]:
  def unit: A

  def combine(a: A, b: A): A

@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f.sum(List())) // 0.0
  println(f.concat(Seq("a", "b", "c"))) // abc
  println(f.concat(Seq())) // ""
  println(f.max(List(-10, 3, -5, 0))) // 3
  println(f.max(List())) // -2147483648
