package foresight.util.collections

import org.junit.Assert._
import org.junit.Test

class ArrayMapTest {

  @Test
  def emptyBasics(): Unit = {
    val m = ArrayMap.empty[String, Int]
    assertEquals(0, m.size)
    assertFalse(m.contains("a"))
    assertEquals(None, m.get("a"))
    assertFalse(m.iterator.hasNext)
  }

  @Test
  def updateAndGet(): Unit = {
    val m0 = ArrayMap.empty[String, Int]
    val m1 = m0.updated("a", 1).asInstanceOf[ArrayMap[String, Int]]
    val m2 = m1.updated("b", 2).asInstanceOf[ArrayMap[String, Int]]
    assertEquals(2, m2.size)
    assertTrue(m2.contains("a"))
    assertTrue(m2.contains("b"))
    assertEquals(Some(1), m2.get("a"))
    assertEquals(Some(2), m2.get("b"))
    assertEquals(None, m2.get("c"))
  }

  @Test
  def overwriteExistingKey(): Unit = {
    val m0 = ArrayMap("a" -> 1, "b" -> 2)
    val m1 = m0.updated("a", 10)
    assertEquals(2, m1.size)
    assertEquals(Some(10), m1.get("a"))
    assertEquals(Some(2), m1.get("b"))
    // equality with a Scala Map should be by contents, not identity
    assertEquals(Map("a" -> 10, "b" -> 2), m1)
  }

  @Test
  def removedMissingReturnsSameInstance(): Unit = {
    val m = ArrayMap("a" -> 1, "b" -> 2)
    val m2 = (m - "c").asInstanceOf[ArrayMap[String, Int]]
    assertTrue(m eq m2)
  }

  @Test
  def removeExistingShrinksAndKeepsOthers(): Unit = {
    val m0 = ArrayMap("a" -> 1, "b" -> 2, "c" -> 3)
    val m1 = (m0 - "b").asInstanceOf[ArrayMap[String, Int]]
    assertEquals(2, m1.size)
    assertFalse(m1.contains("b"))
    assertEquals(Some(1), m1.get("a"))
    assertEquals(Some(3), m1.get("c"))
  }

  @Test
  def iteratorYieldsPairsInInsertionOrder(): Unit = {
    val m = ArrayMap.empty[String, Int]
      .updated("a", 1)
      .updated("b", 2)
      .updated("c", 3)
      .asInstanceOf[ArrayMap[String, Int]]

    val it = m.iterator.toList
    assertEquals(List("a" -> 1, "b" -> 2, "c" -> 3), it)
  }

  @Test
  def overwriteDoesNotChangeIterationOrder(): Unit = {
    val m = ArrayMap("a" -> 1, "b" -> 2, "c" -> 3)
      .updated("b", 20)
      .asInstanceOf[ArrayMap[String, Int]]
    assertEquals(List("a" -> 1, "b" -> 20, "c" -> 3), m.iterator.toList)
  }

  @Test
  def constructorsApplyAndFrom(): Unit = {
    val m1 = ArrayMap("x" -> 1, "y" -> 2)
    val m2 = ArrayMap.from(List("x" -> 1, "y" -> 2))
    assertEquals(m1, m2)
    assertEquals(m1.hashCode(), m2.hashCode())
  }

  @Test
  def equalityParitiesWithScalaMap(): Unit = {
    val am = ArrayMap("k1" -> 1, "k2" -> 2, "k1" -> 3) // last write wins
    val sm = Map("k1" -> 3, "k2" -> 2)
    assertEquals(sm, am)
    assertEquals(sm.hashCode(), am.hashCode())
  }

  @Test
  def emptyMapReturnsDefaultOnGetOrElse(): Unit = {
    val m = ArrayMap.empty[String, Int]
    assertEquals(42, m.getOrElse("missing", 42))
  }

  @Test
  def getOrElseReturnsValueIfKeyExists(): Unit = {
    val m = ArrayMap("key1" -> 100, "key2" -> 200)
    assertEquals(100, m.getOrElse("key1", 0))
    assertEquals(200, m.getOrElse("key2", 0))
  }

  @Test
  def removeFromEmptyMapReturnsSameInstance(): Unit = {
    val m = ArrayMap.empty[String, Int]
    val m2 = (m - "nonexistent").asInstanceOf[ArrayMap[String, Int]]
    assertTrue(m eq m2)
  }

  @Test
  def wrapArraysUnsafeCreatesValidMap(): Unit = {
    val keys = Array[AnyRef]("a", "b", "c")
    val values = Array[AnyRef](1.asInstanceOf[AnyRef], 2.asInstanceOf[AnyRef], 3.asInstanceOf[AnyRef])
    val m = ArrayMap.unsafeWrapArrays[String, Int](keys, values, 3)
    assertEquals(3, m.size)
    assertEquals(Some(1), m.get("a"))
    assertEquals(Some(2), m.get("b"))
    assertEquals(Some(3), m.get("c"))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def wrapArraysUnsafeThrowsIfKeysArrayTooShort(): Unit = {
    val keys = Array[AnyRef]("a")
    val values = Array[AnyRef](1.asInstanceOf[AnyRef], 2.asInstanceOf[AnyRef])
    ArrayMap.unsafeWrapArrays(keys, values, 2)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def wrapArraysUnsafeThrowsIfValuesArrayTooShort(): Unit = {
    val keys = Array[AnyRef]("a", "b")
    val values = Array[AnyRef](1.asInstanceOf[AnyRef])
    ArrayMap.unsafeWrapArrays(keys, values, 2)
  }

  @Test
  def mapValuesStrictTransformsValuesCorrectly(): Unit = {
    val m = ArrayMap("a" -> 1, "b" -> 2, "c" -> 3)
    val transformed = m.mapValuesStrict(_ * 2)
    assertEquals(Some(2), transformed.get("a"))
    assertEquals(Some(4), transformed.get("b"))
    assertEquals(Some(6), transformed.get("c"))
  }

  @Test
  def mapValuesStrictReturnsSameInstanceIfNoChange(): Unit = {
    val m = ArrayMap("a" -> 1, "b" -> 2, "c" -> 3)
    val transformed = m.mapValuesStrict(identity)
    assertTrue(m eq transformed)
  }

  @Test
  def removedReturnsEmptyMapWhenLastElementRemoved(): Unit = {
    val m = ArrayMap("a" -> 1)
    val m2 = (m - "a").asInstanceOf[ArrayMap[String, Int]]
    assertEquals(0, m2.size)
    assertFalse(m2.contains("a"))
  }

  @Test
  def updatedAddsNewKeyValuePair(): Unit = {
    val m = ArrayMap.empty[String, Int]
    val updated = m.updated("newKey", 42)
    assertEquals(Some(42), updated.get("newKey"))
    assertEquals(1, updated.size)
  }

  @Test
  def updatedOverwritesExistingKey(): Unit = {
    val m = ArrayMap("key" -> 1)
    val updated = m.updated("key", 99)
    assertEquals(Some(99), updated.get("key"))
    assertEquals(1, updated.size)
  }

  @Test
  def newBuilderBuildsMapFromElements(): Unit = {
    val builder = ArrayMap.newBuilder[String, Int]
    builder.addOne("a" -> 1)
    builder.addOne("b" -> 2)
    val m = builder.result()
    assertEquals(2, m.size)
    assertEquals(Some(1), m.get("a"))
    assertEquals(Some(2), m.get("b"))
  }

  @Test
  def newBuilderOverwritesDuplicateKeys(): Unit = {
    val builder = ArrayMap.newBuilder[String, Int]
    builder.addOne("x" -> 1)
    builder.addOne("x" -> 2)
    val m = builder.result()
    assertEquals(1, m.size)
    assertEquals(Some(2), m.get("x"))
  }

  @Test
  def newBuilderClearResetsState(): Unit = {
    val builder = ArrayMap.newBuilder[String, Int]
    builder.addOne("a" -> 1)
    builder.clear()
    builder.addOne("b" -> 2)
    val m = builder.result()
    assertEquals(1, m.size)
    assertEquals(Some(2), m.get("b"))
    assertFalse(m.contains("a"))
  }

  @Test
  def concatWithEmptyMapReturnsOriginal(): Unit = {
    val m = ArrayMap("a" -> 1)
    val m2 = m ++ ArrayMap.empty[String, Int]
    assertEquals(m, m2)
  }

  @Test
  def concatEmptyWithNonEmptyReturnsOther(): Unit = {
    val m = ArrayMap.empty[String, Int]
    val m2 = m ++ ArrayMap("x" -> 42)
    assertEquals(1, m2.size)
    assertEquals(Some(42), m2.get("x"))
  }

  @Test
  def concatCombinesMapsAndOverwritesDuplicates(): Unit = {
    val m1 = ArrayMap("a" -> 1, "b" -> 2)
    val m2 = ArrayMap("b" -> 20, "c" -> 3)
    val m3 = m1 ++ m2
    assertEquals(3, m3.size)
    assertEquals(Some(1), m3.get("a"))
    assertEquals(Some(20), m3.get("b"))
    assertEquals(Some(3), m3.get("c"))
  }

  @Test
  def concatWithItselfKeepsLastValues(): Unit = {
    val m = ArrayMap("a" -> 1, "b" -> 2)
    val m2 = m ++ m
    assertEquals(m, m2)
  }
}
