package foresight.eqsat.examples.vector

/**
 * The types supported in the vector normalization example.
 */
enum Type {
  /**
   * A 32-bit floating point number.
   */
  case FloatType

  /**
   * A 3D vector of the specified element type.
   */
  case Vector3Type(elementType: Type)

  /**
   * Returns the sequence of scalar types contained within this type.
   * For `FloatType`, it returns a sequence containing itself.
   * For `Vector3Type`, it returns three instances of its element type's scalars.
   */
  def scalars: Seq[Type] = this match {
    case FloatType => Seq(this)
    case Vector3Type(elementType) => elementType.scalars ++ elementType.scalars ++ elementType.scalars
  }

  /**
   * Returns the count of scalar types contained within this type.
   * This is equivalent to the length of the sequence returned by `scalars`.
   */
  def scalarCount: Int = scalars.length

  /**
   * Attempts to broadcast this type with another type.
   * Broadcasting rules:
   * - `FloatType` can be broadcast with `FloatType` or `Vector3Type(t)` resulting in `Vector3Type(t)`.
   * - `Vector3Type(t)` can be broadcast with `FloatType` resulting in `Vector3Type(t)`.
   * - Two `Vector3Type(t1)` and `Vector3Type(t2)` can be broadcast if `t1` equals `t2`, resulting in `Vector3Type(t1)`.
   * - Any other combination results in no valid broadcast (returns `None`).
   *
   * @param that The other type to broadcast with.
   * @return An `Option` containing the resulting broadcasted type, or `None` if broadcasting is not possible.
   */
  def broadcast(that: Type): Option[Type] = (this, that) match {
    case (FloatType, FloatType) => Some(FloatType)
    case (FloatType, Vector3Type(t)) => Some(Vector3Type(t))
    case (Vector3Type(t), FloatType) => Some(Vector3Type(t))
    case (Vector3Type(t1), Vector3Type(t2)) if t1 == t2 => Some(Vector3Type(t1))
    case _ => None
  }
}
