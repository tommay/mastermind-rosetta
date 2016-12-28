import scala.math

object Mastermind {
  def main(args: Array[String]) {
    val startingGuesses = uniqBy(categorize, allCodes)
    val categories = startingGuesses.map{categorize}
    val stats = startingGuesses.map{computeStats}
    val zipped = categories.zip(stats)
    zipped.foreach{case (category, stats) => println(s"$category: $stats")}
  }

  //val choices = 1 to 6
  val choices = List.range(1, 6)
  val codeSize = 4

  def computeStats(startingGuess: List[Int]) : Stats = {
    allCodes.foldLeft(new Stats) {
      foldCode(startingGuess)
    }
  }

  def foldCode(startingGuess: List[Int])(stats: Stats, code: List[Int])
    : Stats =
  {
    foldGuess(code, 1, allCodes)(stats, startingGuess)
  }

  def foldGuess(code: List[Int],
		depth: Int,
		possibilities: List[List[Int]])
	       (stats: Stats,
		guess: List[Int]) : Stats =
  {
    val score = computeScore(guess, code)
    if (score._1 == codeSize) {
      stats.solvedIt(depth)
    }
    else {
      val remainingPossibilities =
	possibilities.filter{computeScore(guess, _) == score}
      remainingPossibilities.foldLeft(stats) {
	  foldGuess(code, depth + 1, remainingPossibilities)
      }
    }
  }

  // Given a guess and a code, return a pair containing the numnber of
  // exact matches (red), and the number of "right color wrong place"
  // (white).  The result is the same if guess and code are swapped.

  def computeScore(guess: List[Int], code: List[Int]) : (Int, Int) = {
    val mismatched = code.zip(guess).filter{case (g, c) => c != g}
    val red = codeSize - mismatched.length
    // XXX For haskell, Using the pattern match + list comprrehension
    // is way faster than map fst/snd. 
    // val c2 = mismatched.map{_._1}
    // val g2 = mismatched.map{_._2}
    val c2 = for ((c, _) <- mismatched) yield c
    val g2 = for ((_, g) <- mismatched) yield g
    val white = countWhite(g2, c2)
    (red, white)
  }

  def countWhite(guess: List[Int], code: List[Int]) : Int = {
    guess match {
      case Nil => 0
      case g::rest =>
        remove(g, code) match {
	  case None => countWhite(rest, code)
	  case Some(newCode) => 1 + countWhite(rest, newCode)
	}
    }
  }

// remove first occurrence of the element from the list.  If the
// element was not found returns Nothing, otherwise Just list.

  def remove[T](element: T, list: List[T]) : Option[List[T]] = {
    list match {
      case Nil => None
      case first::rest => 
	if (element == first) {
	  Some(rest)
	}
	else {
	  // OMFG!  I can map over the Option, Functor-style!
	  // Reach right inside the Some an tweak it.
	  remove(element, rest).map{first :: _}
	}
    }
  }

  val allCodes = makeCodes(choices, codeSize)

  def makeCodes[T](choices: List[T], size: Int) : List[List[T]] = {
    size match {
      case 0 => List(List())
      case _ =>
	val codes = makeCodes(choices, size - 1)
	choices.flatMap{choice => codes.map{choice :: _}}
    }
  }

  // Given a code, compute the frequencies of the unique digits in the
  // code and sort into reverse numerical order.  E.g., a code with
  // all unique digits maps to (1,1,1,1), a code with two digits the
  // same maps to (2,1,1), etc.
  // This isn't as good as the Haskell version that uses
  // Map.insertWith because it has to actually construct the groupBy
  // map values but it's ok for here.

  def categorize[T](code: List[T]) : List[Int] = {
    code.groupBy{identity}.values.map{_.length}.toList.sorted.reverse
  }

  // Takes a list of items and a function that maps an item to a key,
  // and returns a list of unique items based on the key.

  def uniqBy[T,K](func: T => K, list: List[T]) : Iterable[T] = {
    list.foldLeft(Map[K,T]()) {
      (m, e) => m + (func(e) -> e)
    }.values
  }

  class Stats (
    val solved : Int = 0,
    val total : Int = 0,
    val maxDepth : Int = 0)
  {
    override
    def toString : String = {
      val average = total.toDouble / solved.toDouble
      s"Solved: $solved, average: $average, maxDepth: $maxDepth"
    }

    def solvedIt(depth : Int) : Stats = {
      new Stats(solved = solved + 1,
		total = total + depth,
		maxDepth = math.max(depth, maxDepth))
    }
  }
}
