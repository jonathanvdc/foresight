package foresight.eqsat.examples.sdql

/**
 * The kind of a SDQL expression.
 * @param mightBeVector States if the expression might be a vector.
 * @param mightBeDict States if the expression might be a dictionary.
 * @param mightBeScalar States if the expression might be a scalar.
 * @param mightBeBool States if the expression might be a boolean.
 */
final case class SdqlKind(mightBeVector: Boolean, mightBeDict: Boolean, mightBeScalar: Boolean, mightBeBool: Boolean)

/**
 * Companion object for the [[SdqlKind]] class.
 */
object SdqlKind {
  /**
   * An SDQL expression kind that represents a vector.
   */
  val vector: SdqlKind = SdqlKind(mightBeVector = true, mightBeDict = false, mightBeScalar = false, mightBeBool = false)

  /**
   * An SDQL expression kind that represents a dictionary.
   */
  val dict: SdqlKind = SdqlKind(mightBeVector = false, mightBeDict = true, mightBeScalar = false, mightBeBool = false)

  /**
   * An SDQL expression kind that represents a scalar.
   */
  val scalar: SdqlKind = SdqlKind(mightBeVector = false, mightBeDict = false, mightBeScalar = true, mightBeBool = false)

  /**
   * An SDQL expression kind that represents a boolean.
   */
  val bool: SdqlKind = SdqlKind(mightBeVector = false, mightBeDict = false, mightBeScalar = false, mightBeBool = true)

  /**
   * An SDQL expression kind that represents nothing.
   */
  val nothing: SdqlKind = SdqlKind(mightBeVector = false, mightBeDict = false, mightBeScalar = false, mightBeBool = false)

  /**
   * An SDQL expression kind that represents an unknown type.
   */
  val unknown: SdqlKind = SdqlKind(mightBeVector = true, mightBeDict = true, mightBeScalar = true, mightBeBool = true)

  /**
   * Joins two SDQL expression kinds.
   *
   * @param left  The left SDQL expression kind.
   * @param right The right SDQL expression kind.
   * @return The joined SDQL expression kind.
   */
  def join(left: SdqlKind, right: SdqlKind): SdqlKind = {
    SdqlKind(
      left.mightBeVector || right.mightBeVector,
      left.mightBeDict || right.mightBeDict,
      left.mightBeScalar || right.mightBeScalar,
      left.mightBeBool || right.mightBeBool
    )
  }
}
