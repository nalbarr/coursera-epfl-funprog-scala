package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  // NAA.
  // - Relevant lectures:
  //   - understanding tuples and placeholder notation
  //   - list, map operations (e.g., sort)
  //   - map to list conversions
  //   - pattern matches using pair/tuples
  // 1. normalize chars by lower case; map chars by frequency
  // 2. convert Map(Char, List(Char)) to Map(Char, Int) to capture ordinal frequency values
  // 3. flatten map to List of (Char, Int) pairs
  // 4. sort list by pair first element (i.e.,  alpha order)
  // 5. return  
  def wordOccurrences(w: Word): Occurrences = {
  	val charsLowerCase = w.toList.groupBy(c => c toLower)
  	val charsMapByFreq = charsLowerCase map ( _ match {
  		case (char, chars) => (char, chars.length)
  	})
  	val charsListByFreq = charsMapByFreq.toList
  	val charsSorted = charsListByFreq.sortWith (_._1 < _._1)
  	charsSorted
  }                                               
     
  /** Converts a sentence into its character occurrence list. */
  // NAA
  // - major concepts:
  //   - for comprehension can be decomposed to generators and filters
  //   -
  // 1. recursively pattern match sentence mapping to word occurrences
  // 2. reduce, group, map/apply sum function, sort
  def sentenceOccurrences(s: Sentence): Occurrences = s match {
      case Nil => Nil
      case x :: xs => s.map(wordOccurrences(_))
      	.reduce(_ ++ _)
      		.groupBy(_._1)
      			.map(_ match {
        			case (char, xs) => (char, xs.map(_._2).sum)
      			}
      	).toList.sortWith(_._1 < _._1)
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  // NAA.
  // 1. use map groupBy operation to return map of occurrences -> List[Word]
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(wordOccurrences(_))
  
  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
  	dictionaryByOccurrences getOrElse (wordOccurrences(word), Nil)
  	
  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  // NAA.
  // - concepts
  //   - pattern match on empty (i.e., (zero) frequency) to Some (i.e., (!= 0) frequency)
  //   - "::" concatenation pattern to assist matching patterns
  // 1. recursively pattern match occurrences mapping to occurrences list
  // 2. re-use combinations for recursion on occurrences
  def combinations(xs: Occurrences): List[Occurrences] = xs match {
    case Nil => List(Nil)
    case (_, 0) :: ys => combinations(ys)
    case (char, n) :: ys => {
	    combinations((char, n - 1) :: ys) ++ combinations((char, n - 1) :: ys)
	    	.map(zs =>
	      		zs.find(_._1 == char) match {
	      			case Some(_) => zs.map(_ match {
	      					case (char2, o) => 
	      					  if (char2 == char) (char, o + 1) else (char2, o)
	      				}
	      			)
	      			case None => (char, 1) :: zs
	      	}
	     )
    }
  }    
  
  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  // NAA.
  // 1. determine intersection by x and y occurrences
  // 2. re-map x occurrences and remove duplicate y occurrences
  // 3. filter any char with 0 occurrence
  def subtract(xs: Occurrences, ys: Occurrences): Occurrences = {
  	val xMapped = xs.map(x => ys.find(_._1 == x._1) match {
  		case Some(y) => (x._1, x._2 - y._2)
  		case None => x
  		}
  	)
  	val xFiltered = xMapped.filter (_._2 != 0)
  	xFiltered
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  // NAA.
  // - major concepts
  //   - help recursive function
  //   - List, Map conversions
  //   - flatten, flatMap operations
  // 1. define helper function to get occurrences from dictionary and flatten as word iterables
  // 2. recurse through passed sentence occurrences and determine all anagram permutations and return as sentence list
  // 3. re-use combinations function to get permutations of anagrams
    def sentenceAnagrams(s: Sentence): List[Sentence] = {
    def getWords(xs: Occurrences): Iterable[Word] = dictionaryByOccurrences.get(xs).flatten
    def sentenceAnagrams0(xs: Occurrences): List[Sentence] = xs match {
      case Nil => List(Nil)
      case _ => combinations(xs) flatMap
      	(char => getWords(char) flatMap (
      		w => sentenceAnagrams0(subtract(xs, char))
      			.map(w :: _)
      		)
      	)
    }
    sentenceAnagrams0(sentenceOccurrences(s))
  }  
  
}
