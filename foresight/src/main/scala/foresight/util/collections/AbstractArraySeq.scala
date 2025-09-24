package foresight.util.collections

/**
 * An abstract base class for array-backed indexed sequences.
 *
 * This class provides common functionality for indexed sequences that are backed by an array.
 * Subclasses must implement methods to create new arrays and instances of the subclass.
 *
 * @param _array The underlying array storing the elements of the sequence.
 * @param length The number of valid elements in the sequence.
 * @tparam A    The type of elements in the sequence.
 * @tparam This The concrete type of the subclass extending this abstract class.
 */
abstract class AbstractArraySeq[A <: AnyRef, This <: AbstractArraySeq[A, This]](private val _array: Array[A], val length: Int)
  extends IndexedSeq[A] {

  /**
   * Creates a new array of the specified size.
   */
  protected def newArray(size: Int): Array[A]

  /**
   * Creates a new instance of This with the given array and length.
   */
  protected def create(newArray: Array[A], newLength: Int): This

  private def createOrThis(newArray: Array[A], newLength: Int): This = {
    if ((newArray eq _array) && newLength == length) this.asInstanceOf[This]
    else create(newArray, newLength)
  }

  override def apply(i: Int): A = _array(i)

  override def slice(from: Int, until: Int): This = {
    if (from > 0) {
      val size = until - from
      val newArr = newArray(size)
      Array.copy(_array, from, newArr, 0, size)
      create(newArr, size)
    } else {
      createOrThis(_array, until)
    }
  }

  /**
   * Applies a function to all elements of the sequence, returning a new sequence with the results.
   * If the function does not change any elements (i.e., all results are reference-equal to the originals),
   * the original sequence is returned.
   *
   * @param f The function to apply to each element.
   * @return A new sequence with the results of applying the function, or the original sequence if unchanged.
   */
  def map(f: A => A): This = {
    var i = 0
    var newArr: Array[A] = null

    while (i < length) {
      val orig = _array(i)
      val res  = f(orig)

      if (newArr eq null) {
        // Still in the identical prefix
        if (res ne orig) {
          // First divergence: allocate once and copy the prefix
          newArr = newArray(length)
          if (i > 0) Array.copy(_array, 0, newArr, 0, i)
          newArr(i) = res
        }
      } else {
        // Already diverged: just keep filling the new array
        newArr(i) = res
      }

      i += 1
    }

    if (newArr eq null) this.asInstanceOf[This]
    else create(newArr, length)
  }

  /**
   * Concatenates this sequence with another sequence of the same type.
   * If either sequence is empty, the other sequence is returned.
   *
   * @param that The sequence to concatenate with this one.
   * @return A new sequence containing elements from both sequences.
   */
  def concat(that: This): This = {
    if (this.length == 0) {
      that
    } else if (that.length == 0) {
      this.asInstanceOf[This]
    } else {
      val newArr = newArray(this.length + that.length)
      Array.copy(this._array, 0, newArr, 0, this.length)
      Array.copy(that._array, 0, newArr, this.length, that.length)
      create(newArr, this.length + that.length)
    }
  }

  /**
   * Concatenates this sequence with another sequence of the same type.
   * If either sequence is empty, the other sequence is returned.
   *
   * @param that The sequence to concatenate with this one.
   * @return A new sequence containing elements from both sequences.
   */
  def ++(that: This): This = concat(that)
}
