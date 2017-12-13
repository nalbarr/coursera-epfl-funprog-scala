package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

}

abstract class TweetSet {

  /** This method takes a predicate and returns a subset of all the elements
   *  in the original set for which the predicate is true.
   */
  // NAA. this is a concrete method that seeds a recursion helper method but
  // will be overridden by Empty and NonEmpty since behaviors of filter0 will
  // vary.  Key idiom is binary tree/polymorphism
  def filter(p: Tweet => Boolean): TweetSet = filter0(p, new Empty)
  // NAA. this is abstract helper method for recursion seeding with an empty tweet set
  // Subclasses will override behavior as necesary
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet
  
  // NAA. this is abstract method defining union operator between TweetSets
  def union(that: TweetSet): TweetSet
  
  // Hint: the method "remove" on TweetSet will be very useful.
  def ascendingByRetweet: Trending =
    ascendingByRetweet0(new EmptyTrending)

  // NAA. use same recursion helper method approach as with filter and use
  // Ascending as encapsulating class leveraging binary tree/polymorphism
  def ascendingByRetweet0(accu: Trending): Trending

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def head: Tweet
  def tail: TweetSet

  /** This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }

  def remove(tw: Tweet): TweetSet

  def findMin0(curr: Tweet): Tweet =
    if (this.isEmpty) curr
    else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
    else this.tail.findMin0(curr)

  def findMin: Tweet =
    this.tail.findMin0(this.head)
  // -------------------------------------------------------------------------
}

class Empty extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = accu

  // NAA. Lecture 3.4 - Union of an empty set will always be the passed in other set
  // since Empty has no tweets.
  override def union(that: TweetSet): TweetSet = that  
  
  // NAA. Provide empty implementation by seeding with accumulator
  override def ascendingByRetweet0(accu: Trending): Trending = accu

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean = false
  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
  def isEmpty = true
  def head = throw new Exception("Empty.head")
  def tail = throw new Exception("Empty.tail")
  def remove(tw: Tweet): TweetSet = this
  // -------------------------------------------------------------------------
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  // NAA. Lecture 3.4 - IntSet binary tree traversal
  //def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = ???
  override def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet =
    if (p(elem)) left.filter0(p, right.filter0(p, accu.incl(elem)))
    else left.filter0(p, right.filter0(p, accu))
      
  // NAA.
  // Union can be derived using merging passed in TweetSet with tail of
  // current TweetSet and than re adding head
  override def union(that: TweetSet): TweetSet =
    that.union(tail).incl(head)
    
  // NAA.  Provide non empty implementatioin by ensuring accumumlator
  // applies "add" operator using minimum index (i.e., lexical body)
  override def ascendingByRetweet0(accu: Trending): Trending =
    remove(findMin).ascendingByRetweet0(accu + findMin)

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def isEmpty = false
  def head = if (left.isEmpty) elem else left.head
  def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  // -------------------------------------------------------------------------
}


/** This class provides a linear sequence of tweets.
 */
abstract class Trending {
  def + (tw: Tweet): Trending
  def head: Tweet
  def tail: Trending
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }
}

class EmptyTrending extends Trending {
  def + (tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)
  def head: Tweet = throw new Exception
  def tail: Trending = throw new Exception
  def isEmpty: Boolean = true
  override def toString = "EmptyTrending"
}

class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
  /** Appends tw to the end of this sequence.
   */
  def + (tw: Tweet): Trending =
    new NonEmptyTrending(elem, next + tw)
  def head: Tweet = elem
  def tail: Trending = next
  def isEmpty: Boolean = false
  override def toString =
    "NonEmptyTrending(" + elem.retweets + ", " + next + ")"
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
  
  // NAA. use filter operation with higher order function to create succinct implementations
  val googleTweets: TweetSet = TweetReader.allTweets.filter(tw => google.exists(k => tw.text.contains(k)))
  val appleTweets: TweetSet = TweetReader.allTweets.filter(tw => apple.exists(k => tw.text.contains(k)))

  // Q: from both sets, what is the tweet with highest #retweets?
  val trending: Trending = googleTweets.union(appleTweets).ascendingByRetweet  
}

object Main extends App {
  // Some help printing the results:
  println("RANKED:")
  GoogleVsApple.trending foreach println
}
