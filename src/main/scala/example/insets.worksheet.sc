abstract class IntSet:
  def contains(x: Int): Boolean

  def include(x: Int): IntSet

  def union(s: IntSet): IntSet

/**
 *
 */
class Empty() extends IntSet :
  def contains(x: Int): Boolean = false

  def include(x: Int): IntSet = NonEmpty(x, Empty(), Empty())

  def union(s: IntSet): IntSet = s

end Empty

/**
 * @param x
 * @param left
 * @param right
 */
class NonEmpty(element: Int, left: IntSet, right: IntSet) extends IntSet :
  def contains(x: Int): Boolean =
    if x < element then left.contains(x)
    else if x > element then right.contains(x)
    else true

  def include(x: Int): IntSet =
    if x < element then NonEmpty(element, left.include(x), right)
    else if x > element then NonEmpty(element, left, right.include(x))
    else this

  def union(s: IntSet): IntSet =
    left.union(right).union(s).include(element)

end NonEmpty
