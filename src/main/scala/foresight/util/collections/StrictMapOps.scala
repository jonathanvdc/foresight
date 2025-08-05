package foresight.util.collections

import scala.language.implicitConversions

/**
 * Provides strict operations for maps, ensuring that operations do not allow for lazy evaluation.
 * This is useful in contexts where immediate evaluation is required to avoid potential issues with laziness.
 */
object StrictMapOps {
  /**
   * Implicit conversion from a Map to an Ops instance, allowing for strict operations on the map.
   * @param m The map to convert.
   * @tparam K The type of the keys in the map.
   * @tparam V The type of the values in the map.
   * @return An Ops instance that provides strict operations on the map.
   */
  implicit def toStrictMapOps[K, V](m: Map[K, V]): Ops[K, V] = new Ops(m)

  final class Ops[K, V](private val self: Map[K, V]) extends AnyVal {
    /**
     * Maps the values of the map strictly, i.e., it does not allow for lazy evaluation.
     * This means that the function `f` is applied to each value immediately, and the
     * resulting map is created without any lazy evaluation.
     * @param f The function to apply to each value.
     * @tparam W The type of the resulting values.
     * @return A new map with the same keys and the values transformed by `f`.
     */
    def mapValuesStrict[W](f: V => W): Map[K, W] =
      self.iterator.map { case (k, v) => k -> f(v) }.toMap

    /**
     * Filters the keys of the map strictly, i.e., it does not allow for lazy evaluation.
     * This means that the function `p` is applied to each key immediately, and the
     * resulting map is created without any lazy evaluation.
     * @param p The predicate to apply to each key.
     * @return A new map containing only the entries for which the predicate `p` holds true for the key.
     */
    def filterKeysStrict(p: K => Boolean): Map[K, V] =
      self.iterator.filter { case (k, _) => p(k) }.toMap
  }
}
